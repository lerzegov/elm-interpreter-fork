module XModel exposing (..)
-- new version of XArray

import Types exposing (Value(..))
import FastDict as Dict exposing (Dict)
import Html exposing (text)
import Array exposing (Array)
import Array.Extra
import List.Extra
import XParser exposing (XValue(..))
import TypesXModel exposing (..)
import FormulaParser
import AppUtil
import Html exposing (i)

-- helper expressions and functions to check DataArray contenttype
emptyDataArray : DataArray
emptyDataArray = { ref = "", datasetRef = Nothing, data = Array.empty, text = Array.empty
                 , localDims = Nothing, localDimRefs = Nothing
                 , pointedFormulas = Dict.empty
                 }
isDataArrayText : DataArray -> Bool
isDataArrayText dataArray =
    Array.isEmpty dataArray.data && not (Array.isEmpty dataArray.text)
isDataArrayData : DataArray -> Bool
isDataArrayData dataArray =
    not (Array.isEmpty dataArray.data) && Array.isEmpty dataArray.text
isDataArrayMixed : DataArray -> Bool
isDataArrayMixed dataArray =
    not (Array.isEmpty dataArray.data) && not (Array.isEmpty dataArray.text)
isDataArrayEmpty : DataArray -> Bool
isDataArrayEmpty dataArray =
    Array.isEmpty dataArray.data && Array.isEmpty dataArray.text
emptyDataset : Dataset
emptyDataset = { ref = ""
                , dimRefs = []
                , dataArrayRefs = []
                , dataArrays = Dict.empty
                , formulas = ""
                , defaultDataArrayRef = Nothing
                }

emptyXModel : XModel
emptyXModel = { modelRef = "", datasetRefs = [], datasets = Dict.empty, dims = Dict.empty
              , datasetsToRecalc = [] }

-- === XModel helper functions to add or modify arrays and datasets ===

insertDataArray : DataArrayRef -> DataArray -> DataArrays -> DataArrays
insertDataArray ref dataArray dataArrays =
    Dict.insert ref dataArray dataArrays

insertDataset : DatasetRef -> Dataset -> Datasets -> Datasets
insertDataset ref dataset datasets =
    Dict.insert ref dataset datasets

-- === helper functions for parsing and converting data arrays to Value types used by the interpreter ===

-- PROVISIONAL: manage collisions amomg coord names and error handling in returned list
dimsToCoordDimDict : Dims -> Dict Coord DimRef
dimsToCoordDimDict dims =
    Dict.foldl (\dimRef coords accDict ->
        Array.foldl (\coord accDict2 -> Dict.insert coord dimRef accDict2) accDict coords
    ) Dict.empty dims
-- helper functions created using Dict.get or Dict.insert in XArrayEngine raises error
getDatasetByRef : DatasetRef -> Datasets -> Maybe Dataset
getDatasetByRef ref datasets =
    Dict.get ref datasets

datasetExists : DatasetRef -> XModel -> Bool
datasetExists ref xModel =
    case Dict.get ref xModel.datasets of
        Just _ -> True
        Nothing -> False

getDataArrayByRef : DataArrayRef -> DataArrays -> Maybe DataArray
getDataArrayByRef ref dataArrays =
    Dict.get ref dataArrays

getDataArrayWithDimsFromXModel : XModel -> DatasetRef -> DataArrayRef -> Maybe DataArray
getDataArrayWithDimsFromXModel xModel datasetRef dataArrayRef =
    Maybe.andThen (\dataset ->
        Maybe.andThen (\dataArray ->
            let 
                localDims = dataArray.localDims 
                localdimRefs = dataArray.localDimRefs
                (dimRefs, dims) = case (localdimRefs, localDims) of
                    (Just localdimRefsJust, Just locDimsJust) -> (Just localdimRefsJust, Just locDimsJust)
                    _ ->  dimInfoForDataArray xModel dataArray
            in
            Just { dataArray | localDimRefs = dimRefs, localDims = dims }
        ) (Dict.get dataArrayRef dataset.dataArrays)
    ) (Dict.get datasetRef xModel.datasets)

getParsedDataArrayToValue : Maybe XModel -> DatasetRef -> DataArrayRef -> Bool -> Maybe Value
getParsedDataArrayToValue maybeXModel datasetRef dataArrayRef hasExternalDataset=
    Maybe.andThen (\xModel ->
        Maybe.andThen (\dataArray ->
            Just (dataArrayWithDimsToValue dataArray hasExternalDataset)
        ) (getDataArrayWithDimsFromXModel xModel datasetRef dataArrayRef)
    ) maybeXModel

locDataArrayfromRangeDef : Maybe XModel -> DatasetRef -> DataArrayRef -> List (DimRef, CoordSpecifier) -> Maybe DataArray
locDataArrayfromRangeDef maybeXModel datasetRef dataArrayRef dimCoordTuples =
    case maybeXModel of
        Just xModel ->
            let
                arrayWithDims = getDataArrayWithDimsFromXModel xModel datasetRef dataArrayRef
            in
            if List.isEmpty dimCoordTuples then
                arrayWithDims
            else
            case  arrayWithDims of
                Just dataArray ->
                    let
                        locArray = locAr dataArray dimCoordTuples
                    in
                    case locArray of
                        Just locArrayJust -> 
                            Just locArrayJust
                        Nothing -> Nothing
                Nothing ->
                    Nothing
        Nothing ->
            Nothing




locDataArrayfromRangeDefToValue : Maybe XModel -> DatasetRef -> DataArrayRef -> List (DimRef, CoordSpecifier) -> Bool
            -> Maybe Value
locDataArrayfromRangeDefToValue maybeXModel datasetRef dataArrayRef dimCoordTuples hasExternalDataset =
    Maybe.andThen (\locArray -> Just (dataArrayWithDimsToValue locArray hasExternalDataset)) 
        (locDataArrayfromRangeDef maybeXModel datasetRef dataArrayRef dimCoordTuples)



coordsToDimCoordSpecs : Dims -> List Coord -> List (DimRef, CoordSpecifier)
coordsToDimCoordSpecs dims coords =
    let
        coordDimDict = dimsToCoordDimDict dims
        dimRefsForCoords = List.map (\coord -> Dict.get coord coordDimDict |> Maybe.withDefault "") coords
    in
    -- Debug.log ("coordsToDimCoordSpecs: " ++ Debug.toString dims)
    List.map2 (\coord dimRef -> (dimRef, SingleCoord coord)) coords dimRefsForCoords


-- conversions to elm-interpreter Value type (coded by chatgpt)
-- NB Record is a Value type that can be hyerarchically accessed IN THE CONSOLE     
-- with the dot notation, like a JSON object
dataArrayToValue : DataArray -> Bool -> Value
dataArrayToValue dataArray hasExternalDataset =
    let locDims = dataArray.localDims in
    DataAr <| Dict.fromList
        [ ("ref", String dataArray.ref)
        , ("data", floatArrayToValue dataArray.data)
        , ("text", strArrayToValue dataArray.text)
        , ("localDims", case locDims of
                Just dims -> dimsToValueAsList dims
                Nothing -> List []
            )
        , ("localDimRefs", case dataArray.localDimRefs of
                Just dimRefs -> strListToValue dimRefs
                Nothing -> List []
            )
        , ("hasExternalDataset", Bool hasExternalDataset)
        ]

dataArrayWithDimsToValue : DataArray -> Bool -> Value
dataArrayWithDimsToValue dataArray hasExternalDataset =

    let 
        localDims = dataArray.localDims 
        localdimRefs = dataArray.localDimRefs
        (dimRefs, dims) = case (localdimRefs, localDims) of
            (Just localdimRefsJust, Just locDimsJust) -> (Just localdimRefsJust, Just locDimsJust)
            _ ->  (Nothing, Nothing)
    in
    case (dimRefs, dims) of
        (Just dimRefsJust, Just dimsJust) ->
            DataAr <| Dict.fromList
                [ ("ref", String dataArray.ref)
                , ("datasetRef", String (dataArray.datasetRef |> Maybe.withDefault "")) 
                , ("data", floatArrayToValue dataArray.data)
                , ("text", strArrayToValue dataArray.text)
                , ("localDims", dimsToValueAsList dimsJust)
                , ("localDimRefs", strListToValue dimRefsJust)
                , ("hasExternalDataset", Bool hasExternalDataset)
                ]
        _ ->
            DataAr <| Dict.fromList
                [ ("ref", String dataArray.ref)
                , ("datasetRef", String  "") 
                , ("data", floatArrayToValue Array.empty)
                , ("text", strArrayToValue Array.empty)
                , ("localDims", List [])
                , ("localDimRefs", List [])
                 , ("hasExternalDataset", Bool hasExternalDataset)
                ]


getDataArrayByDVarIndex : Dataset -> Int -> DataArray
getDataArrayByDVarIndex dataset dVarIndex =
    let
        dataArrayRef = getAt dVarIndex dataset.dataArrayRefs
    in
    case dataArrayRef of
        Just dArRef ->
            Dict.get dArRef dataset.dataArrays |> Maybe.withDefault emptyDataArray
        Nothing ->
            emptyDataArray

strArrayToValue : Array String -> Value
strArrayToValue strArray =
    JsArray <| Array.map String strArray

strListToValue : List String -> Value
strListToValue strList =
    List <| List.map String strList

floatArrayToValue : Array Float -> Value
floatArrayToValue floatArray =
    JsArray <| Array.map Float floatArray

dimsToValue : Dims -> Value
dimsToValue dims =
    Record <| Dict.map (\_ coords -> strArrayToValue coords) dims

dimsToValueAsList : Dims -> Value
dimsToValueAsList dims =
    let
        dimList = Dict.toList <| Dict.map (\_ coords -> 
                strArrayToValue coords) dims
    in
    List <| List.map (\(dim, coords) -> Tuple (String dim) coords) dimList

-- === inverse conversions fron values to XModel types ===
valueToStrArray : Value -> Array String
valueToStrArray value =
    case value of
        JsArray vals ->
            Array.map (\v ->
                case v of
                    String s -> Just s
                    _ -> Nothing
            ) vals
            |> Array.Extra.filterMap identity -- This removes the Nothings, keeping only the Just values.
        _ -> Array.empty

valueToStrList : Value -> List String
valueToStrList value =
    case value of
        List vals ->
            List.map (\v ->
                case v of
                    String s -> Just s
                    _ -> Nothing
            ) vals
            |> List.filterMap identity -- This removes the Nothings, keeping only the Just values.
        _ -> []

valueToFloatArray : Value -> Array Float
valueToFloatArray value =
    case value of
        JsArray vals ->
            Array.map (\v ->
                case v of
                    Float s -> Just s
                    Int i -> Just (toFloat i)
                    _ -> Nothing
            ) vals
            |> Array.Extra.filterMap identity -- This removes the Nothings, keeping only the Just values.
        List vals ->
            List.map (\v ->
                case v of
                    Float s -> Just s
                    Int i -> Just (toFloat i)
                    _ -> Nothing
            ) vals
            |> List.filterMap identity -- This removes the Nothings, keeping only the Just values.
            |> Array.fromList
        _ -> Array.empty

valueToStringArray : Value -> Array String
valueToStringArray value =
    case value of
        JsArray vals ->
            Array.map (\v ->
                case v of
                    String s -> Just s
                    _ -> Nothing
            ) vals
            |> Array.Extra.filterMap identity -- This removes the Nothings, keeping only the Just values.
        _ -> Array.empty

valueToDims : Value -> Dims
valueToDims value =
    case value of
        List dimsList ->
            let
                dimsTupleList = List.map valueToDimTuple dimsList
            in
            List.foldl (\(dimRef, coordArray) accDict -> Dict.insert dimRef coordArray accDict) Dict.empty dimsTupleList
        _ -> Dict.empty

valueToDimsTest : Value -> String
valueToDimsTest value =
    case value of
        List dimsList ->
            let
                dimsTupleList = List.map valueToDimTuple dimsList
            in
            -- Debug.toString (List.foldl (\(dimRef, coordArray) accDict -> Dict.insert dimRef coordArray accDict) Dict.empty dimsTupleList)
            (Debug.toString dimsList) ++ "\n" ++ (Debug.toString dimsTupleList)
        _ -> "no pattern match"
valueToDimTuple : Value -> (DimRef, Array Coord)
valueToDimTuple value =
    case value of
        Tuple (String dimRef) (JsArray coordArray) ->
            (dimRef, valueToStrArray (JsArray coordArray))
        _ -> ("", Array.empty)
-- hasExternalDataset not marshalled into xModel, used only in the interpreter
valueToDataArray : Value -> DataArray
valueToDataArray value =
    case value of
        DataAr dict ->
            let
                ref = case Dict.get "ref" dict of
                    Just (String s) -> s
                    _ -> ""
                datasetRef = case Dict.get "datasetRef" dict of
                    Just (String s) -> Just s
                    _ -> Nothing
                
                data = case Dict.get "data" dict of
                    Just (JsArray floats) -> valueToFloatArray (JsArray floats)
                    _ -> Array.empty
                
                text = case Dict.get "text" dict of
                    Just (JsArray strings) -> valueToStrArray (JsArray strings)
                    _ -> Array.empty
                localDims = case Dict.get "localDims" dict of
                    Just val -> Just (valueToDims val)
                    _ -> Nothing
                localDimRefs = case Dict.get "localDimRefs" dict of
                    Just val -> Just (valueToStrList val)
                    _ -> Nothing
                -- pointeFormulas not handled
            in
            DataArray ref datasetRef data text localDims localDimRefs Dict.empty -- PROVVI
        _ ->
            --DataArray "" Nothing Array.empty Array.empty Nothing Nothing
            emptyDataArray



-- FUNCTIONS FOR HANDLING STRUCTURE AND DATA

-- returns number of categDims in a DataArray
nrDims : Dataset  -> Int
nrDims dataset = List.length dataset.dimRefs


dimVariantToDimRef : DimVariantRef -> DimRef
dimVariantToDimRef dimVariantRef =
    case dimVariantRef of
        CategDimRef dimRef -> dimRef
        DVarDimRef dVarRef -> dVarRef

-- kept annotation similar to following functions 
-- alternative is XModel -> DatasetRef -> Array Coord
getDVarDimCoords : Dataset -> Array Coord
getDVarDimCoords dataset =
    let
        dVarCoords = Array.fromList dataset.dataArrayRefs
    in
    dVarCoords


-- assumes dims are unique by name in XModel
dimVariantCoords : Dims -> Dataset -> DimVariantRef -> Array Coord
dimVariantCoords dims dataset dimVariantRef =
    case dimVariantRef of
        CategDimRef dim -> Dict.get dim dims |> Maybe.withDefault Array.empty
        DVarDimRef _ -> Array.fromList dataset.dataArrayRefs

dimVariantName : DimVariantRef -> String
dimVariantName dimVariant =
    case dimVariant of
        CategDimRef dimRef -> dimRef
        DVarDimRef dVarRef -> dVarRef

dimVariantRefByDimName : Dataset -> String -> DimVariantRef
dimVariantRefByDimName dataset dimName =
    let
        maybeDim : Maybe DimRef
        maybeDim = List.filter (\dim -> dim == dimName) dataset.dimRefs
            |> List.head
    in
    case maybeDim  of
        Just dim -> CategDimRef dim
        Nothing -> DVarDimRef dVarIdentifier
shape : Dims -> Dataset ->List DimVariantRef -> List Int
shape dims dataset dimVarRefs = 
    List.map (\dimVarRef ->
                let
                    dimVarCoords = dimVariantCoords dims dataset dimVarRef
                in
                Array.length dimVarCoords
             ) dimVarRefs

shapeDim : Dims -> List DimRef -> List Int
shapeDim dims dimRefs = 
    List.map (\dRef ->
                let
                    dimCoords = Dict.get dRef dims |> Maybe.withDefault Array.empty
                in
                Array.length dimCoords
             ) dimRefs

coordsByDimRef : Dims -> DimRef -> Array Coord
coordsByDimRef dims dimRef = 
    Dict.get dimRef dims |> Maybe.withDefault Array.empty

-- for a Dataset, returns the index (pos) in the Dims list (iter order)
-- for a given DimRef (categDim)
dimIndexByDimRef : Dataset -> DimRef -> Maybe Int
dimIndexByDimRef dataset dimRef =
    elemIndexList dimRef dataset.dimRefs -- duplicated here to avoid module error in the interpreter
-- for a Dataset, returns the index (pos) of the coord in the coords of a dim
-- for a given tuple (String=dimRef, Coord)
coordIndexByDimRefCoordTuples : Dims -> (DimRef, Coord) -> Maybe Int
coordIndexByDimRefCoordTuples dims (dimRef, coord) =
    let
        coords = Dict.get dimRef dims |> Maybe.withDefault Array.empty
    in
    elemIndex coord coords 
datasetForDataArray : XModel -> DataArray -> Maybe Dataset
datasetForDataArray xModel dataArray =
    case dataArray.datasetRef of
        Just ref -> Dict.get ref xModel.datasets
        Nothing -> Nothing

-- returns the full Dim info for the DimRefs in a DataArray, witouht filtering
dimInfoForDataArray : XModel -> DataArray -> (Maybe (List DimRef), Maybe Dims)
dimInfoForDataArray xModel dataArray =
    let
        dataset = datasetForDataArray xModel dataArray
    in
    case (dataArray.localDimRefs, dataArray.localDims, dataset) of
        (Just dimRefs, Just dims, _) -> (Just dimRefs, Just dims)
        (Just dimRefs, Nothing, _) -> (Just dimRefs, dimsForDimRefs xModel dimRefs)
        (Nothing, Nothing, Just datasetJust) -> (Just datasetJust.dimRefs, dimsForDimRefs xModel datasetJust.dimRefs)
        _ -> (Just ["empty","dims"], Just Dict.empty) --(Nothing, Nothing)

-- returns the full Dim info for the DimRefs in a DataArray, witouht filtering
dimInfoForDatasetRef : XModel -> DatasetRef -> (Maybe (List DimRef), Maybe Dims)
dimInfoForDatasetRef xModel datasetRef =
    let
        maybeDataset = getDatasetByRef datasetRef xModel.datasets
    in
    case maybeDataset of 
        Just dataset -> (Just dataset.dimRefs, dimsForDimRefs xModel dataset.dimRefs)
        Nothing -> (Just ["empty","dims"], Just Dict.empty) --(Nothing, Nothing)



-- for a given DimVariant, returns the index in the flat array of the data
-- for a given list of indices, i.e. positions of the coords in the respective DimVariant
--with strides precalculated
calcFlatIndexFast : List Stride ->  PosVec -> Maybe FlatIndex
calcFlatIndexFast strides posVec  =
    if List.length posVec == List.length strides then
        if List.member -1 posVec then 
             Nothing
        else
            Just <| List.foldl (\(index, stride) acc -> acc + index * stride) 0 (zip posVec strides)
    else
        Nothing
calcDataLengthFromDims : DataArray-> Maybe Int
calcDataLengthFromDims dataArrayWithDims =
    Maybe.andThen
        (\dims ->
            let
                sizes = Dict.map (\_ coords -> Array.length coords) dims |> Dict.values 
            in
            Just (List.foldl (*) 1 sizes)
        )
        dataArrayWithDims.localDims
-- strides are calculate starting from the last (innernost) dimension
-- which has stride 1; the strides for the preceding dimensions are the product
-- of the sizes of the following dimensions
-- the result is a list of strides, one for each dimension, reversed to match the order of the dims
calculateStrides : List Size -> List Stride
calculateStrides sizes =
    let
        reversedSizes = List.reverse sizes
        cumulativeProduct = scanl (*) 1 (reversedSizes)
    in
    cumulativeProduct
        |> List.reverse
        |> List.drop 1

-- TODO: check duplication with generatePosVecsNoMaybe
cartesianProductPosVecs : Maybe (List (Array CoordIndex)) -> Maybe (Array PosVec)
cartesianProductPosVecs maybeLists =
    case maybeLists of
        Just lists ->
            Just (cartesianProductListOfArToArOfList lists)
        Nothing ->
            Nothing

-- for a list of Maybe posVecs matched by a list fo DimVariants, returns a Maybe list of flat indices
-- used by iloc
calcFlatIndices :  Dims -> Dataset -> Maybe (Array PosVec) -> List DimVariantRef -> Maybe (Array FlatIndex)
calcFlatIndices dims dataset maybePosVecs dimRefs =
    case maybePosVecs of -- non controllo che le dimensioni di posvec e strides siano uguali
        Just posVecs ->
            let
                curShape = shape dims dataset dimRefs
                strides = calculateStrides curShape
            in
            Just <| Array.map (\posVec -> 
                         if List.member -1 posVec then 
                            -1
                        else
                        -- same as calcFlatIndexFast strides indices withouth Maybe
                        List.foldl (\(index, stride) acc -> acc + index * stride) 0 (zip posVec strides)
                        ) posVecs
        Nothing ->
            Nothing

calcFlatIndicesAr :  DataArray -> Maybe (Array PosVec) -> Maybe (Array FlatIndex)
calcFlatIndicesAr dataArray maybePosVecs =
    let
        dims = dataArray.localDims |> Maybe.withDefault Dict.empty
        dimRefs = dataArray.localDimRefs |> Maybe.withDefault []
    in
    case maybePosVecs of -- non controllo che le dimensioni di posvec e strides siano uguali
        Just posVecs ->
            let
                curShape = shapeDim dims dimRefs
                strides = calculateStrides curShape
            in
            Just <| Array.map (\posVec -> 
                         if List.member -1 posVec then 
                            -1
                        else
                        -- same as calcFlatIndexFast strides indices withouth Maybe
                        List.foldl (\(index, stride) acc -> acc + index * stride) 0 (zip posVec strides)
                        ) posVecs
        Nothing ->
            Nothing

-- used by iloc
calcFlatIndicesFast :  Dims -> List DimRef -> Array (PosVec)  -> Array FlatIndex
calcFlatIndicesFast dims dimRefs posVecs  =
            let
                sizes = shapeDim dims dimRefs 
                strides = calculateStrides sizes
            in
            Array.map (\posVec -> 
                        if List.member -1 posVec then 
                            -1
                        else
                        -- same as calcFlatIndexFast strides indices withouth Maybe
                            List.foldl (\(index, stride) acc -> acc + index * stride) 0 (zip posVec strides)
                        ) posVecs


-- converts a FlatIndex to a PosVec, given the ordered list of dim sizes
-- spiegazione matlab 
-- https://stackoverflow.com/questions/12429492/how-to-convert-a-monodimensional-index-to-corresponding-indices-in-a-multidimens
itemPosVec : FlatIndex -> List Size -> Maybe PosVec
itemPosVec flatIndex sizes =
    let     
        strides = calculateStrides sizes
        sizesWithStrides = zip sizes strides
        nrDim = List.length sizes
        calcIndexByDim flatIdx j size stride =
            if j == 0 then -- first dim
                flatIdx // stride
            else if j == nrDim - 1 then -- last dim
                remainderBy size flatIdx -- NB 1st arg is divisor!!
            else
                remainderBy size (flatIdx // stride) 

        indices = 
            List.indexedMap (\j (sz, st) -> (calcIndexByDim flatIndex) j sz st) sizesWithStrides
    in
    Just indices

itemPosVecsForDims : Dims -> List DimRef -> Array PosVec
itemPosVecsForDims dims dimRefs =
    let     
        sizes = shapeDim dims dimRefs
        indicesByDim = List.map (\size -> List.range 0 (size - 1)) sizes
        posVecs = cartesianProduct indicesByDim
    in
    Array.fromList posVecs

itemCoordVecsForDims : Dims -> List DimRef -> Array CoordVec
itemCoordVecsForDims dims dimRefs =
    let     
        curCoords = List.map (\dimRef -> Dict.get dimRef dims 
                        |> Maybe.withDefault Array.empty |> Array.toList ) dimRefs
        posVecs = cartesianProduct curCoords
    in
    Array.fromList posVecs


-- helper function to convert an IndexSpecifier to a list of filtered indices
expandIndexSpecifier : Dims -> Dataset -> DimVariantRef -> IndexSpecifier -> Array CoordIndex
expandIndexSpecifier dims dataset dimVarRef specifier =
    let
        dimVarCoords = dimVariantCoords dims dataset dimVarRef
    in
    case specifier of
        SingleIndex idx ->
            Array.fromList [idx]
        IndexRange (idxStart, idxEnd) ->
            Array.fromList (List.range idxStart idxEnd)
        IndexList indices ->
            indices
        IndexNone ->
            Array.fromList (List.range 0 (Array.length dimVarCoords - 1))

-- used on complete DataArrays
expandIndexSpecifierAr : DataArray -> DimRef -> IndexSpecifier -> Array CoordIndex
expandIndexSpecifierAr dataArray dimRef specifier =
    let
        dims = dataArray.localDims |> Maybe.withDefault Dict.empty
        dimCoords = Dict.get dimRef dims |> Maybe.withDefault Array.empty
    in
    case specifier of
        SingleIndex idx ->
            Array.fromList [idx]
        IndexRange (idxStart, idxEnd) ->
            Array.fromList (List.range idxStart idxEnd)
        IndexList indices ->
            indices
        IndexNone ->
            Array.fromList (List.range 0 (Array.length dimCoords - 1))

-- helper functions to construct (dimRef, IndexSpecifier) tuplespassed  to iloc
toSingleIdx : DimRef -> CoordIndex -> (DimRef, IndexSpecifier)
toSingleIdx dimRef idx = (dimRef, SingleIndex idx)

toIdxList : DimRef -> Array CoordIndex -> (DimRef, IndexSpecifier)
toIdxList dimRef idxList = (dimRef, IndexList idxList)

toIdxRange : DimRef -> CoordIndex -> CoordIndex -> (DimRef, IndexSpecifier)
toIdxRange dimRef idxStart idxEnd = (dimRef, IndexRange (idxStart, idxEnd))

toIdxNone : DimRef -> (DimRef, IndexSpecifier)
toIdxNone dimRef = (dimRef, IndexNone)

dimsForDataset : Dims -> Dataset -> Dims
dimsForDataset xModelDims dataset =
    let
        dimRefs = dataset.dimRefs
    in
    Dict.filter (\k _ -> List.member k dimRefs) xModelDims
dimsForDimRefs : XModel -> List DimRef -> Maybe Dims
dimsForDimRefs xModel dimRefs =
    let
        rawDims = Dict.filter (\k _ -> List.member k dimRefs) xModel.dims
    in 
    if Dict.size rawDims == List.length dimRefs then
        Just rawDims
    else
        Nothing

-- compares two Dims to check if they have the same dim/coords for all dims
-- except a "pivot" one where both have a single coord, and returns the DimRef for that one
pivotDimRefForDims : Dims -> Dims -> Maybe DimRef 
pivotDimRefForDims dims1 dims2 =
    let
        dim1Refs = Dict.keys dims1
        dim2Refs = Dict.keys dims2
        matchesCheckResult = 
            if dim1Refs == dim2Refs then
                List.foldl (\curRef curMatchRefTuple -> 
                    let
                        coords1 = Dict.get curRef dims1 |> Maybe.withDefault Array.empty
                        coords2 = Dict.get curRef dims2 |> Maybe.withDefault Array.empty
                        coordsNotEmptyMatch = (coords1 == coords2 && coords1 /= Array.empty)
                        bothSingleCoords = (Array.length coords1 == 1 && Array.length coords2 == 1)
                        prevMatches = Tuple.first curMatchRefTuple
                        prevRef = Tuple.second curMatchRefTuple
                    in
                    if coordsNotEmptyMatch then
                        (prevMatches + 1, prevRef)
                    else if bothSingleCoords then
                        (prevMatches, curRef)
                    else
                        (prevMatches, prevRef)
                ) (0,"") dim1Refs

            else 
                (0,"")
        pivotDimRef = 
            if Tuple.first matchesCheckResult == List.length dim1Refs - 1 then
                Just (Tuple.second matchesCheckResult)
            else 
                Nothing
    in
    pivotDimRef

dimVariantRefsForDataset : Dataset -> Bool -> List DimVariantRef
dimVariantRefsForDataset dataset withDVar =
    let
        dimRefs = dataset.dimRefs
        categDimRefs = List.map (\k -> (CategDimRef k)) dimRefs
        dVarInList = if withDVar then [DVarDimRef dVarIdentifier] else []
    in
    categDimRefs ++ dVarInList

-- takes a list of tuples (dim, IndexSpecifier) as input, completes it with a loop over dimensions, 
-- and then extracts the data into a Maybe DataArray
-- NB new DataArray belonging to a Dataset has no info about dims, only the data
-- the DataArray returned by iloc has the localDims and localDimRefs fields set
-- why use DimVariantRef and not DimRef? => to handle DVarDimRef in XView
iloc : Dims -> Dataset -> DataArray -> List (DimVariantRef, IndexSpecifier) -> Maybe (DataArray)
iloc dims dataset dataArray dimRefIndicesTuples =
    let
        -- loop on dataset dimRefs ++ dVar and complete dimRefIndicesTuples with IndexNone for missing dims
        completeDimRefIndicesTuples :  List (DimVariantRef, IndexSpecifier) 
        completeDimRefIndicesTuples = 
            List.map (\dimVarRef -> 
                let
                    filteredTuple = List.filter (\(d, _) -> d == dimVarRef) dimRefIndicesTuples
                    foundSpecifier = filteredTuple
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault IndexNone
                in
                -- Debug.log ("iloc filteredTuple: " ++ Debug.toString filteredTuple)
                (dimVarRef, foundSpecifier)
            ) (dimVariantRefsForDataset dataset False) -- false to exclude DVarDimRef
        
        localExpandSpecifier dimRefArg dimIndexSpecifier = expandIndexSpecifier dims dataset dimRefArg dimIndexSpecifier

        dimVariantRefs : List DimVariantRef
        dimVariantRefs = List.map (Tuple.first) completeDimRefIndicesTuples
        
        dimRefs : List DimRef
        dimRefs = List.map (Tuple.first >> dimVariantToDimRef)  completeDimRefIndicesTuples
        expandedIndices = List.map 
                    (\locTuple -> 
                        localExpandSpecifier (Tuple.first locTuple) (Tuple.second locTuple)
                    ) completeDimRefIndicesTuples


        -- provisional: makes a local copy of Dims with filtered dims and coords used by the DataArray
        updatedDimsList : List (DimRef, Array Coord)
        updatedDimsList = -- returns a list of dim records with filtered coords
            List.map2 (\dimRef indices ->
                let
                    dimCoords = Dict.get dimRef dims |> Maybe.withDefault Array.empty
                    filteredCoords = Array.map (\idx -> Array.get idx dimCoords |> Maybe.withDefault "") indices
                in
                (dimRef, filteredCoords)
            ) dimRefs expandedIndices
        updatedDims : Dims
        updatedDims = Dict.fromList updatedDimsList

        flatIndices = Just expandedIndices -- gets posVecs from expandedIndices then calculates flat indices
            |> cartesianProductPosVecs
            |> Maybe.andThen (\posVecs -> calcFlatIndices dims dataset (Just posVecs) dimVariantRefs)

        (filteredData, filteredText)  = case flatIndices of
            Just indices ->
                (extractElementsArray dataArray.data indices, extractElementsArray dataArray.text indices)
            Nothing -> -- if flatIndices is Nothing, return an empty ArrayVariant of tyhe same type
                (Array.empty,Array.empty)
    in
    -- Debug.log ("dimRefIndicesTuples: " ++ Debug.toString dimRefIndicesTuples)
    -- Return the filtered DataArray with updated dimensions and data
    Just { dataArray | data = filteredData, text = filteredText, localDims = Just updatedDims, localDimRefs = Just dimRefs }

-- used on complete DataArray, dataset DVars not handled only one DVar oer DataArray
ilocAr : DataArray -> List (DimRef, IndexSpecifier) -> Maybe (DataArray)
ilocAr dataArray dimRefIndicesTuples =
    let
        dims = dataArray.localDims |> Maybe.withDefault Dict.empty
        dimRefs = dataArray.localDimRefs |> Maybe.withDefault []
        
        completeDimRefIndicesTuples :  List (DimRef, IndexSpecifier) 
        completeDimRefIndicesTuples = 
            List.map (\dimRef -> 
                let
                    filteredTuple = List.filter (\(d, _) -> d == dimRef) dimRefIndicesTuples
                    foundSpecifier = filteredTuple
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault IndexNone
                in
                -- Debug.log ("iloc filteredTuple: " ++ Debug.toString filteredTuple)
                (dimRef, foundSpecifier)
            ) dimRefs -- false to exclude DVarDimRef
        localExpandSpecifier dimRefArg dimIndexSpecifier = expandIndexSpecifierAr dataArray dimRefArg dimIndexSpecifier
        
        expandedIndices = List.map 
                    (\locTuple -> 
                        localExpandSpecifier (Tuple.first locTuple) (Tuple.second locTuple)
                    ) completeDimRefIndicesTuples
        updatedDimsList : List (DimRef, Array Coord)
        updatedDimsList = -- returns a list of dim records with filtered coords
            List.map2 (\dimRef indices ->
                let
                    dimCoords = Dict.get dimRef dims |> Maybe.withDefault Array.empty
                    filteredCoords = Array.map (\idx -> Array.get idx dimCoords |> Maybe.withDefault "") indices
                in
                (dimRef, filteredCoords)
            ) dimRefs expandedIndices
        updatedDims : Dims
        updatedDims = Dict.fromList updatedDimsList

        flatIndices = Just expandedIndices -- gets posVecs from expandedIndices then calculates flat indices
            |> cartesianProductPosVecs
            |> Maybe.andThen (\posVecs -> calcFlatIndicesAr dataArray (Just posVecs))

        (filteredData, filteredText)  = case flatIndices of
            Just indices ->
                (extractElementsArray dataArray.data indices, extractElementsArray dataArray.text indices)
            Nothing -> -- if flatIndices is Nothing, return an empty ArrayVariant of tyhe same type
                (Array.empty,Array.empty)
    in
    -- Debug.log ("dimRefIndicesTuples: " ++ Debug.toString dimRefIndicesTuples)
    -- Return the filtered DataArray with updated dimensions and data
    Just { dataArray | data = filteredData, text = filteredText, localDims = Just updatedDims, localDimRefs = Just dimRefs }


-- HANDLING COORDS, ANALOGOUS TO INDICES

strToCoordSpecifier : String -> CoordSpecifier
strToCoordSpecifier str =
    if str == "none" then
        CoordNone
    else if String.startsWith "single::" str then
        let
            coord = String.dropLeft 8 str |> String.trim
        in
        SingleCoord coord
    else if String.startsWith "range::" str then
        let
            coords = String.dropLeft 7 str |> String.split "," |> List.map String.trim
        in
        case coords of
            [coordFirst, coordLast] -> CoordRange (coordFirst, coordLast)
            _ -> CoordNone
    else if String.startsWith "list::" str then
        let
            coords = Array.fromList (String.dropLeft 6 str |> String.split "," |> List.map String.trim)
        in
        CoordList coords
    else
        CoordNone

-- helper functions to construct (dim, LocSpecifier) tuples passed to loc
toSingleCoord : DimRef -> Coord -> (DimVariantRef, CoordSpecifier)
toSingleCoord dimRef coord = (CategDimRef dimRef, SingleCoord coord)

toCoordList : DimRef -> Array Coord -> (DimVariantRef, CoordSpecifier)
toCoordList dimRef coordArray = (CategDimRef dimRef, CoordList coordArray)

toCoordRange : DimRef -> Coord -> Coord -> (DimVariantRef, CoordSpecifier)
toCoordRange dimRef coordStart coordEnd = (CategDimRef dimRef, CoordRange (coordStart, coordEnd))

toCoordNone : DimRef -> (DimVariantRef, CoordSpecifier)
toCoordNone dimRef = ( CategDimRef dimRef, CoordNone)

toSingleCoordAr : DimRef -> Coord -> (DimRef, CoordSpecifier)
toSingleCoordAr dimRef coord = ( dimRef, SingleCoord coord)

toCoordListAr : DimRef -> Array Coord -> (DimRef, CoordSpecifier)
toCoordListAr dimRef coordArray = ( dimRef, CoordList coordArray)

toCoordRangeAr : DimRef -> Coord -> Coord -> (DimRef, CoordSpecifier)
toCoordRangeAr dimRef coordStart coordEnd = ( dimRef, CoordRange (coordStart, coordEnd))

toCoordNoneAr : DimRef -> (DimRef, CoordSpecifier)
toCoordNoneAr dimRef = ( dimRef, CoordNone)


-- takes a list of tuples (dimVarRef, LocSpecifier) as input, completes it with a loop over dimensions,
-- chatgpt implementation wraps iloc, with wrong coords returns all coords like CoordNone
loc : Dims -> Dataset -> DataArray -> List (DimVariantRef, CoordSpecifier) -> Maybe (DataArray )
loc dims dataset dataArray dimRefCoordsTuples =
    -- Convert dimCoordsTuples to IndexSpecifiers and call iloc
    let
        indexSpecifiers = List.map (\(dimRef, coordSpecifier) -> 
            (dimRef, convertSpecifier dims dataset dimRef coordSpecifier |> Maybe.withDefault IndexNone)
            ) dimRefCoordsTuples
    in
    -- Debug.log ("dimRefCoordsTuples: " ++ Debug.toString dimRefCoordsTuples)
    iloc dims dataset dataArray indexSpecifiers


locAr : DataArray -> List (DimRef, CoordSpecifier) -> Maybe (DataArray )
locAr dataArray dimRefCoordsTuples =
    -- Convert dimCoordsTuples to IndexSpecifiers and call iloc
    let
        indexSpecifiers = List.map (\(dimRef, coordSpecifier) -> 
            (dimRef, convertSpecifierAr dataArray dimRef coordSpecifier |> Maybe.withDefault IndexNone)
            ) dimRefCoordsTuples
    in
    -- Debug.log ("dimRefCoordsTuples: " ++ Debug.toString dimRefCoordsTuples)
    ilocAr dataArray indexSpecifiers

getDataArrayByLoc : XModel -> DatasetRef -> DataArrayRef -> List String -> Maybe DataArray
getDataArrayByLoc xModel datasetRef dataArrayRef dimRefCoordsStrPatterns =
    let
        dataset = getDatasetByRef datasetRef xModel.datasets |> Maybe.withDefault emptyDataset
        dataArray = getDataArrayByRef dataArrayRef dataset.dataArrays |> Maybe.withDefault emptyDataArray
        dimRefCoordsTuples = List.map strToDimRefCoordsTuple dimRefCoordsStrPatterns
    in
    loc xModel.dims dataset dataArray dimRefCoordsTuples

-- helper function
strToDimRefCoordsTuple : String -> (DimVariantRef, CoordSpecifier)
strToDimRefCoordsTuple str =
    let
        (dimVariantRef, coordPart) =
            case String.split "||" str of
                [only] -> (only, "")
                [dimRef, coords] -> (dimRef, coords)
                _ -> ("", "")
    in
    case coordPart of
        "" -> (CategDimRef dimVariantRef, CoordNone)
        _ ->
            if String.contains ".." coordPart then
                -- CoordRange detected
                let
                    coords = String.split ".." coordPart
                in
                case coords of
                    [firstCoord, lastCoord] -> (CategDimRef dimVariantRef, CoordRange (firstCoord, lastCoord))
                    _ -> (CategDimRef dimVariantRef, CoordNone) -- Fallback for malformed range
            else
                -- Treat as CoordList or SingleCoord
                let
                    coords = Array.fromList (String.split "," coordPart)
                in
                case Array.length coords of
                    1 -> (CategDimRef dimVariantRef, SingleCoord (Array.get 0 coords |> Maybe.withDefault ""))
                    _ -> (CategDimRef dimVariantRef, CoordList coords)

strToDimRefCoordsTupleVerbose : String -> (DimVariantRef, CoordSpecifier)
strToDimRefCoordsTupleVerbose str =
    let
        -- Split the string into DimVariantRef and the specifier part.
        parts = String.split "||" str
        dimVariantRef : DimVariantRef 
        dimVariantRef =
            case List.head parts of
                Just ref -> CategDimRef ref
                Nothing -> CategDimRef ""  -- Consider how to handle errors or empty strings appropriately.
        
        specifierStr =
            case List.drop 1 parts of
                spec :: _ -> spec
                [] -> ""
        
        -- Determine the type of coord specifier based on the specifier string.
        coordSpecifier = strToCoordSpecifier specifierStr
    in
    (dimVariantRef, coordSpecifier)

coordDict : Dims -> Dataset -> DimVariantRef -> CoordDict
coordDict dims dataset dimVarRef = 
    let
        coordsforDim = dimVariantCoords dims dataset dimVarRef
    in
    Dict.fromList (
    Array.toList coordsforDim 
    |> List.indexedMap (\j coord -> (coord,j))
    )

coordDictFast : Array Coord -> CoordDict
coordDictFast coordsforDim = 
    Dict.fromList (
        Array.indexedMap (\j coord -> (coord,j)) coordsforDim 
        |> Array.toList
    ) 
coordToIndex : CoordDict -> Coord -> CoordIndex
coordToIndex dimVarDict coord =
    case Dict.get coord dimVarDict of
        Just idx -> idx
        Nothing -> -1

-- Convert coordSpecifier to IndexSpecifier
convertSpecifier : Dims -> Dataset -> DimVariantRef -> CoordSpecifier -> Maybe IndexSpecifier
convertSpecifier dims dataset dimVar specifier =
    let
        coordsforDim = dimVariantCoords dims dataset dimVar
        coordDictLocal = 
            Dict.fromList (
                Array.toList coordsforDim 
                |> List.indexedMap (\j coord -> (coord,j))
            )
    in

    case specifier of
        SingleCoord coord ->
            Dict.get coord coordDictLocal |> Maybe.map SingleIndex

        CoordRange (startCoord, endCoord) ->
            case (Dict.get startCoord coordDictLocal, Dict.get endCoord coordDictLocal) of
                (Just startIndex, Just endIndex) ->
                    if startIndex <= endIndex then
                        Just (IndexRange (startIndex, endIndex))
                    else
                        Nothing
                _ ->
                    Nothing

        CoordList coords -> 
            let
                indices = Array.map (\coord -> 
                    Dict.get coord coordDictLocal |> Maybe.withDefault -1) coords
            in
                Just (IndexList indices)

        CoordNone ->
            Just IndexNone


convertSpecifierAr : DataArray -> DimRef -> CoordSpecifier -> Maybe IndexSpecifier
convertSpecifierAr dataArray dim specifier =
    let
        dims = dataArray.localDims |> Maybe.withDefault Dict.empty
        coordsforDim = Dict.get dim dims |> Maybe.withDefault Array.empty
        coordDictLocal = 
            Dict.fromList (
                Array.toList coordsforDim 
                |> List.indexedMap (\j coord -> (coord,j))
            )
    in

    case specifier of
        SingleCoord coord ->
            Dict.get coord coordDictLocal |> Maybe.map SingleIndex

        CoordRange (startCoord, endCoord) ->
            case (Dict.get startCoord coordDictLocal, Dict.get endCoord coordDictLocal) of
                (Just startIndex, Just endIndex) ->
                    if startIndex <= endIndex then
                        Just (IndexRange (startIndex, endIndex))
                    else
                        Nothing
                _ ->
                    Nothing

        CoordList coords -> 
            let
                indices = Array.map (\coord -> 
                    Dict.get coord coordDictLocal |> Maybe.withDefault -1) coords
            in
                Just (IndexList indices)

        CoordNone ->
            Just IndexNone


updateDataArrayByIndices : DataArray -> Array FlatIndex -> DataArray
updateDataArrayByIndices dataArray indices =
    {dataArray | data = extractElementsArray dataArray.data indices
                , text = extractElementsArray dataArray.text indices}

updateDataArraysByIndices : List DataArray -> Array FlatIndex -> List DataArray
updateDataArraysByIndices dataArrays indices =
        List.map (\dataArray -> 
                updateDataArrayByIndices dataArray indices) dataArrays

findDVarDimIndex : Array DimVariantRef -> Maybe Int
findDVarDimIndex specifiers =
    findIndex (\dimVariant -> isDVarDim dimVariant) specifiers -- findIndex redefined here with elemIndex

isDVarDim : DimVariantRef -> Bool
isDVarDim dimVariantRef =
    case dimVariantRef of
        DVarDimRef _ ->
            True
        _ ->
            False

-- returns the datum from a Dataset as string from flatIndex and position of the current variable in dVar
getStrDatumFromDatasetForFlatIndexDVarIndex : Dataset -> FlatIndex -> Int -> String
getStrDatumFromDatasetForFlatIndexDVarIndex dataset flatIndex dVarIndex =
    let
        dataArrayRef = getAt dVarIndex dataset.dataArrayRefs
        retDatum = case dataArrayRef of
                Just dArRef ->
                    let
                        dAr = Dict.get dArRef dataset.dataArrays |> Maybe.withDefault emptyDataArray
                        dVarDatum = Array.get flatIndex dAr.text

                    in
                    dVarDatum
                Nothing ->
                    Just ""
    in
    retDatum |> Maybe.withDefault ""

getXValueDatumFromDatasetForFlatIndexDVarIndex : Dataset -> FlatIndex -> Int -> XValue
getXValueDatumFromDatasetForFlatIndexDVarIndex dataset flatIndex dVarIndex =
    let
        dataArrayRef = getAt dVarIndex dataset.dataArrayRefs
    in
    case dataArrayRef of
        Just dArRef ->
            let
                dAr = Dict.get dArRef dataset.dataArrays |> Maybe.withDefault emptyDataArray
                dVarDatum =
                    if isDataArrayEmpty dAr then
                        XEmpty
                    else if isDataArrayData dAr || isDataArrayMixed dAr then
                        case Array.get flatIndex dAr.data of
                            Just value ->if isNaN value then XEmpty else XFloat value
                            Nothing -> XEmpty -- Handle the case where the index is out of bounds or data is not as expected
                    else if isDataArrayText dAr then
                        case Array.get flatIndex dAr.text of
                            Just value -> XString value
                            Nothing -> XEmpty -- Handle the case where the index is out of bounds or text is not as expected
                    else
                        XEmpty -- Handle other cases, if necessary
            in
            dVarDatum
        Nothing ->
            XEmpty


setDatumFromStringToDataset : Dataset -> FlatIndex -> Int -> String -> Dataset
setDatumFromStringToDataset dataset flatIndex dVarIndex datum =
    let
        -- Attempt to update the data array at the given index with the new datum
        updateDataArray : DataArray -> DataArray
        updateDataArray dAr =
            let
                updatedData = setDatumFromStringToArray dAr flatIndex datum
            in
            updatedData

        -- Update the data array within the dataset, if it exists
        dataArrayRef = getAt dVarIndex dataset.dataArrayRefs 
        updatedDataArrays = case dataArrayRef of
            Just arRef ->
                    let
                        upDataAr = Dict.get arRef dataset.dataArrays |> Maybe.withDefault emptyDataArray
                        updatedDataArray = updateDataArray upDataAr
                    in
                    Dict.insert arRef updatedDataArray dataset.dataArrays
            Nothing ->
                dataset.dataArrays
       --  Dict.insert dataArrayRef updateDataArray dataset.dataArrays
    in
    { dataset | dataArrays = updatedDataArrays }

setXValueDatumToDataset : Dataset -> FlatIndex -> Int -> XValue -> Dataset
setXValueDatumToDataset dataset flatIndex dVarIndex datum =
    let
        -- Attempt to update the data array at the given index with the new datum
        updateDataArray : DataArray -> DataArray
        updateDataArray dAr =
            let
                updatedData = setXValueDatumToArray dAr flatIndex datum
            in
            updatedData

        -- Update the data array within the dataset, if it exists
        dataArrayRef = getAt dVarIndex dataset.dataArrayRefs 
        updatedDataArrays = case dataArrayRef of
            Just arRef ->
                    let
                        upDataAr = Dict.get arRef dataset.dataArrays |> Maybe.withDefault emptyDataArray
                        updatedDataArray = updateDataArray upDataAr
                    in
                    Dict.insert arRef updatedDataArray dataset.dataArrays
            Nothing ->
                dataset.dataArrays
       --  Dict.insert dataArrayRef updateDataArray dataset.dataArrays
    in
    { dataset | dataArrays = updatedDataArrays }
-- sets the datum to a DataArray from a string given flatIndex 
setDatumFromStringToArray : DataArray -> FlatIndex -> String -> DataArray
setDatumFromStringToArray dataArray flatIndex datum =
    let
        updatedData = Array.Extra.update flatIndex (\_ -> Maybe.withDefault (0/0) (String.toFloat datum)) dataArray.data
        updatedText = Array.Extra.update flatIndex (\_ -> datum) dataArray.text
    in
    { dataArray | data = updatedData, text = updatedText }

setXValueDatumToArray : DataArray -> FlatIndex -> XValue -> DataArray
setXValueDatumToArray dataArray flatIndex datum =
    let
        updatedData = case datum of
            XFloat value -> if isDataArrayData dataArray then
                    Array.Extra.update flatIndex (\r -> value) dataArray.data
                else
                    dataArray.data
            XString _ -> dataArray.data
            XEmpty -> if isDataArrayData dataArray then
                    Array.Extra.update flatIndex (\r -> 0/0) dataArray.data
                else
                    dataArray.data
            XError _ -> dataArray.data 

        updatedText = case datum of
            XFloat _ -> dataArray.text
            XString strValue -> Array.Extra.update flatIndex (\r -> strValue) dataArray.text
            XEmpty -> if isDataArrayText dataArray then
                        Array.Extra.update flatIndex (\r -> "") dataArray.text
                    else
                        dataArray.text
            -- provisional, error msg always in text
            XError errMsg -> Array.Extra.update flatIndex (\r -> errMsg) dataArray.text

    in
    { dataArray | data = updatedData, text = updatedText }
-- duplicates extractElementsArray that has no default value assignment
getDataFromArrayForIndices : Array FlatIndex -> Array a -> a -> Array a
getDataFromArrayForIndices indices dataArray defaultValue =
    Array.map (\i -> Array.get i dataArray |> Maybe.withDefault defaultValue) indices





-------------------------------------------------------------------------------
--- DUPLICATED GENERAL FUNCTIONS TO AVOID MODULE ERR IN THE INTERPRETER

-- copied from List.Extra because elm-interpreter not recognizing them at runtime

{-| Returns `Just` the element at the given index in the list,
or `Nothing` if the index is out of range.
-}
getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
elemIndexList : a -> List a -> Maybe Int
elemIndexList x =
    findIndexList ((==) x)

findIndexList : (a -> Bool) -> List a -> Maybe Int
findIndexList =
    findIndexHelpList 0


findIndexHelpList : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelpList index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelpList (index + 1) predicate xs

elemIndex : a -> Array a -> Maybe Int
elemIndex x arr =
    findIndex ((==) x) arr

findIndex : (a -> Bool) -> Array a -> Maybe Int
findIndex predicate arr =
    findIndexHelp 0 predicate arr

findIndexHelp : Int -> (a -> Bool) -> Array a -> Maybe Int
findIndexHelp index predicate arr =
    if index >= Array.length arr then
        Nothing
    else
        case Array.get index arr of
            Just x ->
                if predicate x then
                    Just index
                else
                    findIndexHelp (index + 1) predicate arr
            Nothing ->
                Nothing
cartesianProduct : List (List a) -> List (List a)
cartesianProduct ll =
    case ll of
        [] ->
            [ [] ]

        xs :: xss ->
            lift2 (::) xs (cartesianProduct xss)
lift2 : (a -> b -> c) -> List a -> List b -> List c
lift2 f la lb =
    la |> andThen (\a -> lb |> andThen (\b -> [ f a b ]))

andThen : (a -> List b) -> List a -> List b
andThen =
    List.concatMap

-- only handles unpacking of List to/from Arrays
cartesianProductListOfArToArOfList : List (Array a) -> Array (List a)
cartesianProductListOfArToArOfList listOfArrays =
    listOfArrays
        |> List.map Array.toList
        |> cartesianProduct
        |> Array.fromList





scanl : (a -> b -> b) -> b -> List a -> List b
scanl f b xs =
    let
        scan1 x accAcc =
            case accAcc of
                acc :: _ ->
                    f x acc :: accAcc

                [] ->
                    []

        -- impossible
    in
    List.reverse (List.foldl scan1 [ b ] xs)

zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair

updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list

    else
        let
            tail : List a
            tail =
                List.drop index list
        in
        case tail of
            x :: xs ->
                List.take index list ++ fn x :: xs

            [] ->
                list


-- MY GENERAL LIST OR ARRAY FUNCTIONS

extractElements : List a -> List Int -> List a
extractElements arr indices =
    indices
        |> List.map (\index -> getAt index arr)
        |> List.filterMap identity

-- duplicates getFromArrayForIndices that has default value assignment
extractElementsArray : Array a -> Array Int -> Array a
extractElementsArray arr indices =
    indices
        |> Array.map (\index -> Array.get index arr)
        |> Array.Extra.filterMap identity

updateElementsArray : Array a -> Array a -> Array Int -> Array a
updateElementsArray arrayToUpdate updatedValues updatedIndices =
    let
        updateAtIndex (index, value) arr   =
            if index >= 0 && index < Array.length arr then
                Array.set index value arr
            else
                arr
        tuples = Array.Extra.zip updatedIndices updatedValues
    in
    Array.foldl updateAtIndex arrayToUpdate tuples


-- equivalent to haskell List.sequence, unwraps a list of Maybes to a Maybe List
-- validation is done on the resulting Maybe List that is Nothing if any of the elements is Nothing
listSequence : List (Maybe a) -> Maybe (List a)
listSequence maybes =
    List.foldr
        (\maybe acc ->
            case maybe of
                Just value ->
                    Maybe.map (\list -> value :: list) acc
                Nothing ->
                    Nothing
        )
        (Just [])
        maybes

arraySequence : Array (Maybe a) -> Maybe (Array a)
arraySequence maybeArray =
    Array.foldr
        (\maybeElem acc ->
            case (maybeElem, acc) of
                (Just elem, Just elems) ->
                    Just (Array.push elem elems)

                _ ->
                    Nothing
        )
        (Just Array.empty)
        (Array.Extra.reverse maybeArray)

type XTree a b
    = Node (List ( b, XTree a b ))
    | Leaf (List a)


-- rifatta per usare XTree per Tree che altrimenti doveva essere importato da MyPivotTable con import circolare
applyHorizontally : (( comparable, XTree row comparable ) -> a) -> XTree row comparable -> List (List a)
applyHorizontally f tree =
    let
        applyOneLevel : XTree row comparable -> List a
        applyOneLevel tree_ =
            case tree_ of
                Leaf _ ->
                    []

                Node lst ->
                    List.map f lst

        getSubTrees : XTree row comparable -> List (XTree row comparable)
        getSubTrees tree_ =
            case tree_ of
                Leaf _ ->
                    []

                Node lst ->
                    List.map Tuple.second lst

        g : List (XTree row comparable) -> List (List a) -> List (List a)
        g trees prevResult =
            let
                result : List a
                result =
                    List.concatMap applyOneLevel trees

                nextResult : List (List a)
                nextResult =
                    result :: prevResult
            in
            case result of
                [] ->
                    prevResult

                _ ->
                    g (List.concatMap getSubTrees trees) nextResult
    in
    g [ tree ] [] |> List.reverse


getWidth : XTree row comparable -> Int
getWidth tree =
    case tree of
        Leaf _ ->
            1

        Node lst ->
            lst |> List.map (Tuple.second >> getWidth) |> List.sum

-- === DataArray functions used in the interpreter ===
-- MAPPING DIMENSIONS AND OPERATIONS ON DATAARRAYS

type alias BinaryFuncFloat = Float -> Float -> Float
type alias UnaryFuncFloat = Float -> Float
type alias AggrUnaryFuncFloat = Array Float -> Float 
type alias RankUnaryFuncFloat = Array Float -> Float -> Float 
type alias AggrBinaryFuncFloat = Array Float -> Array Float -> Float 

type alias BinaryFuncString = String -> String -> String
type alias UnaryFuncString = String -> String
type alias AggrFuncString = Array String -> String

arRank : RankUnaryFuncFloat -- 1-based rank, duplicates get the same rank
arRank floatArray val = 
    let
        sortedList = List.sort (Array.toList floatArray)
        n = List.length sortedList
        rank = List.Extra.elemIndex val sortedList |> Maybe.withDefault -1 |> toFloat
    in
    rank  + 1

    
arSum : AggrUnaryFuncFloat
arSum floatArray = Array.foldl (+) 0 floatArray

arAverage : AggrUnaryFuncFloat
arAverage floatArray = 
    let
        n = Array.length floatArray |> toFloat
    in
    arSum floatArray / n

arMedian : AggrUnaryFuncFloat
arMedian floatArray = 
    let
        sortedList = List.sort (Array.toList floatArray)
        n = List.length sortedList
        mid = n // 2
        sortedArray = Array.fromList sortedList
    in
    if remainderBy n 2 == 0 then
        ((Array.get mid sortedArray |> Maybe.withDefault 0) + (Array.get (mid - 1) sortedArray |> Maybe.withDefault 0)) / 2
    else
        Array.get mid sortedArray |> Maybe.withDefault 0

arMode : AggrUnaryFuncFloat
arMode floatArray =
    let
        countDict =
            Array.foldl
                (\val dict ->
                    Dict.update val
                        (\maybeCount ->
                            Just (Maybe.withDefault 0 maybeCount + 1)
                        )
                        dict
                )
                Dict.empty
                floatArray

        maxCount =
            Dict.values countDict
                |> List.maximum

        modeList =
            Dict.toList countDict
                |> List.filter (\(_, count) -> Just count == maxCount)
                |> List.map Tuple.first

    in
    if List.isEmpty modeList then
        0
    else
        List.head modeList |> Maybe.withDefault 0



arProduct : AggrUnaryFuncFloat
arProduct floatArray = Array.foldl (*) 1 floatArray

-- Generalized arMax function
arMax : AggrUnaryFuncFloat
arMax floatArray =
    let
        initValue = Array.get 0 floatArray
        foldFunction = \val acc -> max val acc
    in
    case initValue of
        Just val -> Array.foldl foldFunction val (Array.slice 1 (Array.length floatArray) floatArray)
        Nothing -> (0/0) -- or another default value indicating an empty array

-- Generalized arMin function
arMin : AggrUnaryFuncFloat
arMin floatArray =
    let
        initValue = Array.get 0 floatArray
        foldFunction = \val acc -> min val acc
    in
    case initValue of
        Just val -> Array.foldl foldFunction val (Array.slice 1 (Array.length floatArray) floatArray)
        Nothing -> (0/0) -- or another default value indicating an empty array

arVariance : AggrUnaryFuncFloat
arVariance floatArray = 
    let
        n = Array.length floatArray |> toFloat
        mean = arSum floatArray / n
        sumSquares = Array.foldl (\val acc -> acc + (val - mean)^2) 0 floatArray
    in
    sumSquares / n

arStdDev : AggrUnaryFuncFloat
arStdDev floatArray = 
    let
        variance = arVariance floatArray
    in
    sqrt variance

arVarianceSample : AggrUnaryFuncFloat
arVarianceSample floatArray = 
    let
        n = Array.length floatArray |> toFloat
        mean = arSum floatArray / n
        sumSquares = Array.foldl (\val acc -> acc + (val - mean)^2) 0 floatArray
    in
    sumSquares / (n - 1)

arStdDevSample : AggrUnaryFuncFloat
arStdDevSample floatArray = 
    let
        variance = arVarianceSample floatArray
    in
    sqrt variance


arPairSumProduct : AggrBinaryFuncFloat
arPairSumProduct floatArray1 floatArray2 = 
    let
        floatArray = Array.Extra.map2 (*) floatArray1 floatArray2
    in
    
    Array.foldl (+) 0 floatArray

arPairCovariance : AggrBinaryFuncFloat
arPairCovariance floatArray1 floatArray2 = 
    let
        n = Array.length floatArray1 |> toFloat
        mean1 = arSum floatArray1 / n
        mean2 = arSum floatArray2 / n
        sumProducts = Array.Extra.map2 (\val1 val2 -> (val1 - mean1) * (val2 - mean2)) floatArray1 floatArray2
    in
    Array.foldl (+) 0 sumProducts / n

arPairCorrelation : AggrBinaryFuncFloat
arPairCorrelation floatArray1 floatArray2 = 
    let
        covariance = arPairCovariance floatArray1 floatArray2
        stdDev1 = arStdDev floatArray1
        stdDev2 = arStdDev floatArray2
    in
    covariance / (stdDev1 * stdDev2)

arPairCovarianceSample : AggrBinaryFuncFloat
arPairCovarianceSample floatArray1 floatArray2 = 
    let
        n = Array.length floatArray1 |> toFloat
        mean1 = arSum floatArray1 / n
        mean2 = arSum floatArray2 / n
        sumProducts = Array.Extra.map2 (\val1 val2 -> (val1 - mean1) * (val2 - mean2)) floatArray1 floatArray2
    in
    Array.foldl (+) 0 sumProducts / (n - 1)

arPairCorrelationSample : AggrBinaryFuncFloat
arPairCorrelationSample floatArray1 floatArray2 = 
    let
        covariance = arPairCovarianceSample floatArray1 floatArray2
        stdDev1 = arStdDevSample floatArray1
        stdDev2 = arStdDevSample floatArray2
    in
    covariance / (stdDev1 * stdDev2)

joinStringPair : String -> BinaryFuncString
joinStringPair separator str1 str2 = str1 ++ separator ++ str2

joinStringArray : String -> AggrFuncString
joinStringArray separator strArray = Array.foldl (\new acc -> acc ++ separator ++ new) "" strArray

taxMultiplier : UnaryFuncFloat
taxMultiplier val = val * 0.2

myId : UnaryFuncFloat
myId val = val

id : UnaryFuncFloat
id = identity


keepNotNaN : BinaryFuncFloat
keepNotNaN val1 val2 = if isNaN val1  then val2 else val1

-- used to update with formula calculated values
-- provisional, to be replaced with calc status for each datum in whole data array
-- if the valPart is NaN returns the valWhole, otherwise the valPart that is the calculated value
-- so if not NaN the valPart overwrites the valWhole
refreshWithValPart : BinaryFuncFloat
refreshWithValPart valPart valWhole = 
    if (isNaN valPart)  then valWhole else valPart 

-- Helper functions for array functions
-- given coordVecDest in the destination DataArray, returns the posVec in the source DataArray
-- used to get data in a formula from another DataArray with different dims (ex fron scenario to ce)
mapPosVec : Dims -> List DimRef -> List DimRef -> CoordVec -> Maybe PosVec
mapPosVec dimsSrc dimRefsSrc dimRefsDest coordVecDest =
    -- loop on dimRefsSrc
    List.foldl (\dimRef posVec ->
        let
            -- gett coords for the dimRef
            coordsSrc = Dict.get dimRef dimsSrc |> Maybe.withDefault Array.empty
            -- create a dict of coordIndices by coords for the dimRef
            coordDictSrc = coordDictFast coordsSrc
        in
        -- gets pos of current dimRef in dimRefsDest
        case List.Extra.elemIndex dimRef dimRefsDest of
            -- if the dimRef is in dimRefsDest tries to match the coord in coordVecDest
            Just destIndex ->
                let
                    -- get the coord for the destIndex in coordVecDest passed to the function
                    maybeCoord = List.Extra.getAt destIndex coordVecDest 
                    srcIndex = case maybeCoord of
                        -- if the coord is in the coordsSrc
                        Just coord ->
                            -- gets the coordIndex for the coord in dimsSrc
                            -- fi not found the dict is corrupted and returns -1
                            Maybe.withDefault -1 (Dict.get coord coordDictSrc)
                        -- if the coord is not in the coordsSrc
                        Nothing -> -1
                in
                Maybe.map (\vec -> vec ++ [srcIndex]) posVec
            -- if the dimRef is only in srcDims, adds the 1st coordIndex in dimsSrc
            Nothing ->
                Maybe.map (\vec -> vec ++ [0]) posVec
    ) (Just []) dimRefsSrc

-- given an arDest consuming data in a formula from arSrc, 
-- returns an array of posVecs in the src DataArray parallel to the posVecs in the dest DataArray
-- and the dims and dimRefs of the src DataArray
mapPosVecs : DataArray -> DataArray -> Maybe (Array PosVec, Dims, List DimRef)
mapPosVecs arSrc arDest =
    case (arSrc.localDims, arSrc.localDimRefs) of
        (Just dimsSrc, Just dimRefsSrc) ->
            case (arDest.localDims, arDest.localDimRefs) of
                (Just dimsDest, Just dimRefsDest) ->
                    let
                        coordVecs = itemCoordVecsForDims dimsDest dimRefsDest
                        posVecs = Array.map (\coordVecDest -> 
                                case mapPosVec dimsSrc dimRefsSrc dimRefsDest coordVecDest of
                                    Just posVec -> posVec
                                    Nothing -> []
                            ) coordVecs
                    in 
                    Just (posVecs, dimsSrc, dimRefsSrc)
                _ -> Nothing
        _ -> Nothing



-- returns an array of flatIndices in the src DataArray parallel to the flatIndices in the dest DataArray
-- used to get data from the src DataArray and consume them in the dest DataArray
mapFlatIndices : DataArray -> DataArray ->  Array FlatIndex
mapFlatIndices arSrc arDest = -- arDest is the DataArray consuming data from arSrc
    let
        maybePosVecs = mapPosVecs arSrc arDest

    in
    case maybePosVecs of
        Just (posVecs, dims, dimRefs) -> 
            calcFlatIndicesFast dims dimRefs posVecs

        Nothing -> Array.empty


-- ==================== DataArray functions used by the interpreter ====================

-- unary calculation does not need dims, only the data, this func is a wrapper for the map
funcDataArrayUnary : Result String DataArray -> UnaryFuncFloat 
    -> Result String DataArray
funcDataArrayUnary arSrc unaryFunc = 
    case arSrc of
        Ok arJust -> 
            let
                calcData = Array.map unaryFunc arJust.data
            in
            Ok { arJust | data = calcData }
        Err err -> Err (err ++ " catched in funcDataArrayUnary")
            

-- test function used also in FuncDataArray but in interpreter not working
descrValue : Value -> String
descrValue value = 
    case value of
        Record _ -> "Record"
        String _ -> "Text"
        Float _ -> "Number"
        _  -> "Other"

-- returns a new DataArray aggregating the data of the source DataArray by the given aggrDimRefs using the aggrFunc
-- aggrDimRefs must be a subset of the source DataArray dims, they are eliminated in the returned DataArray 
aggrDataArrayUnary : XModel -> Result String DataArray -> AggrUnaryFuncFloat -> List DimRef -> DataArrayRef
    -> Result String DataArray
aggrDataArrayUnary xModel arSrc aggrFunc aggrDimRefs dataArrayRef = 
    case arSrc of
        Ok arJust -> 
            let
                (srcDimRefs , srcDims) = dimInfoForDataArray xModel arJust
            in
            case (srcDims,  srcDimRefs) of
                (Just srcDimsJust, Just srcDimRefsJust) ->
                    if List.all (\aggrDimRef -> List.member aggrDimRef srcDimRefsJust) aggrDimRefs then
                        let
                            keptDimRefs = List.filter (\dimRef -> not (List.member dimRef aggrDimRefs)) srcDimRefsJust
                            keptDims = Dict.filter (\key _ -> List.member key keptDimRefs) srcDimsJust
                            dataset = datasetForDataArray xModel arJust |> Maybe.withDefault emptyDataset
                            aggrData = Array.map (\coordVec -> 
                                let
                                    coordSpecifiers = List.map2 (\ dimRef coord -> toSingleCoord dimRef coord) keptDimRefs coordVec

                                    coordVecDAr = loc srcDimsJust dataset arJust coordSpecifiers |> Maybe.withDefault emptyDataArray
                                    floatArray = coordVecDAr.data
                                in
                                aggrFunc floatArray
                                ) (itemCoordVecsForDims keptDims keptDimRefs)
                        in
                        Ok { arJust | ref = dataArrayRef, datasetRef = Just dataset.ref, data = aggrData, localDims = Just keptDims, localDimRefs = Just keptDimRefs} 
                    else
                        Err "aggrDimRefs not in srcDimRefs"

                _ -> Err "Error in aggrDataArrayUnary"
        Err err -> Err (err ++ " catched in funcDataArrayUnary")
-- used in elm-interpreter 
aggrDataArrayUnaryAr : Result String DataArray -> AggrUnaryFuncFloat -> List DimRef -> DataArrayRef
    -> Result String DataArray
aggrDataArrayUnaryAr arSrc aggrFunc aggrDimRefs dataArrayRef = 
    case arSrc of
        Ok arJust -> 
            let
                (srcDimRefs , srcDims) = (arJust.localDimRefs, arJust.localDims)
            in
            case (srcDims,  srcDimRefs) of
                (Just srcDimsJust, Just srcDimRefsJust) ->
                    if List.all (\aggrDimRef -> List.member aggrDimRef srcDimRefsJust) aggrDimRefs then
                        let
                            keptDimRefs = List.filter (\dimRef -> not (List.member dimRef aggrDimRefs)) srcDimRefsJust
                            keptDims = Dict.filter (\key _ -> List.member key keptDimRefs) srcDimsJust
                            aggrData = Array.map (\coordVec -> 
                                let
                                    coordSpecifiers = List.map2 (\ dimRef coord -> toSingleCoordAr dimRef coord) keptDimRefs coordVec

                                    coordVecDAr = locAr arJust coordSpecifiers |> Maybe.withDefault emptyDataArray
                                    floatArray = coordVecDAr.data
                                in
                                aggrFunc floatArray
                                ) (itemCoordVecsForDims keptDims keptDimRefs)
                        in
                        Ok { arJust | ref = dataArrayRef, data = aggrData, localDims = Just keptDims, localDimRefs = Just keptDimRefs} 
                    else
                        Err "aggrDimRefs not in srcDimRefs"

                _ -> Err "Error in aggrDataArrayUnary"
        Err err -> Err (err ++ " catched in funcDataArrayUnary")

-- applies a binary function to the two DataArrays data, assuming DataArrays are passed with localDims
-- if dims are different, matches the data by coords onto arDest .data
-- returns tuple (maybe DataArray, maybe pivotDimRef)
funcDataArrayPair : XModel -> DataArray -> DataArray  -> BinaryFuncFloat -> (Maybe DataArray, Maybe DimRef)
funcDataArrayPair xModel arSrc arDest binaryFunc = 
    let
        (srcDimRefs , srcDims) = dimInfoForDataArray xModel arSrc
        (destDimRefs , destDims) = dimInfoForDataArray xModel arDest
    in
    case (srcDims,  destDims) of
        -- extend checks to localDimRefs 
        (Just dimsSrc, Just dimsDest) ->
            case (srcDimRefs, destDimRefs) of
            (Just dimRefsSrc, Just dimRefsDest) ->
                let
                    pivotRef = pivotDimRefForDims dimsSrc dimsDest
                    arSrcdata = 
                        -- brute force map calc on the whole data arrays with the same localDims
                        -- with single coord array mapping does not work
                        if dimsSrc == dimsDest
                            && dimRefsSrc == dimRefsDest
                            && (Array.length arSrc.data) == (Array.length arDest.data) then
                        -- if (Array.length arSrc.data) == (Array.length arDest.data) then
                                arSrc.data
                        -- if same dims except one with one coord, data are parallel and can be passed as is
                        else if pivotRef /= Nothing then
                                arSrc.data
                        else
                        -- map the data of the source DataArray to the destination DataArray by dim/coords
                        -- TODO to be re-checked
                            let
                                arSrcWithDims = { arSrc | localDims = Just dimsSrc, localDimRefs = Just dimRefsSrc }
                                arDestWithDims = { arDest | localDims = Just dimsDest, localDimRefs = Just dimRefsDest }
                                mappedIndices = mapFlatIndices arSrcWithDims arDestWithDims -- mapping needs localDims
                            in
                            --Debug.log ("arSrc.localDims: " ++ Debug.toString(srcDims))
                            --Debug.log ("mappedIndices: " ++ Debug.toString(mappedIndices))
                            Array.map (\flIdx -> Array.get flIdx arSrc.data |> Maybe.withDefault (0/0) ) mappedIndices   
                    calcData = Array.Extra.map2 binaryFunc arDest.data arSrcdata              
                in
                    -- Debug.log ("arSrcdata: " ++ Debug.toString(arSrcdata))
                    -- Debug.log ("arDest.data: " ++ Debug.toString(arDest.data))
                    -- Debug.log ("calcData: " ++ Debug.toString(calcData))
                    ( Just { arDest | data = calcData }, pivotRef)  -- keeps dims and dimRefs, but changes data as calculated
            _ -> (Nothing, Nothing)
        _ -> (Nothing, Nothing)

-- removed xModel from the signature, differemt from the homonime used in interpreter
funcDataArrayPairAr : DataArray -> DataArray  -> BinaryFuncFloat -> (Maybe DataArray, Maybe DimRef)
funcDataArrayPairAr arSrc arDest binaryFunc = 
    let
        (srcDimRefs , srcDims) = (arSrc.localDimRefs, arSrc.localDims)
        (destDimRefs , destDims) = (arDest.localDimRefs, arDest.localDims)
    in
    case (srcDims,  destDims) of
        -- extend checks to localDimRefs 
        (Just dimsSrc, Just dimsDest) ->
            case (srcDimRefs, destDimRefs) of
            (Just dimRefsSrc, Just dimRefsDest) ->
                let
                    pivotRef = pivotDimRefForDims dimsSrc dimsDest
                    arSrcdata = 
                        -- brute force map calc on the whole data arrays with the same localDims
                        -- with single coord array mapping does not work
                        if dimsSrc == dimsDest
                            && dimRefsSrc == dimRefsDest
                            && (Array.length arSrc.data) == (Array.length arDest.data) then
                        -- if (Array.length arSrc.data) == (Array.length arDest.data) then
                                arSrc.data
                        -- if same dims except one with one coord, data are parallel and can be passed as is
                        else if pivotRef /= Nothing then
                                arSrc.data
                        else
                        -- map the data of the source DataArray to the destination DataArray by dim/coords
                        -- TODO to be re-checked
                            let
                                arSrcWithDims = { arSrc | localDims = Just dimsSrc, localDimRefs = Just dimRefsSrc }
                                arDestWithDims = { arDest | localDims = Just dimsDest, localDimRefs = Just dimRefsDest }
                                mappedIndices = mapFlatIndices arSrcWithDims arDestWithDims -- mapping needs localDims
                            in
                            --Debug.log ("arSrc.localDims: " ++ Debug.toString(srcDims))
                            --Debug.log ("mappedIndices: " ++ Debug.toString(mappedIndices))
                            Array.map (\flIdx -> Array.get flIdx arSrc.data |> Maybe.withDefault (0/0) ) mappedIndices   
                    calcData = Array.Extra.map2 binaryFunc arSrcdata arDest.data              
                in
                    ( Just { arDest | data = calcData }, pivotRef)  -- keeps dims and dimRefs, but changes data as calculated
            _ -> (Nothing, Nothing)
        _ -> (Nothing, Nothing)

-- updates the data of arWhole DataArray with the data of the arPart DataArray using the binaryFunc to choose or calc the data
updateDataArrayPair : XModel -> Result String DataArray -> Result String DataArray  -> BinaryFuncFloat 
    -> Result String DataArray
updateDataArrayPair xModel arWhole arPart binaryFunc = 
    case (arWhole, arPart) of
        (Ok arWholeOk, Ok arPartOk) ->
                let
                    (wholeDimRefs , wholeDims) = dimInfoForDataArray xModel arWholeOk
                    (partDimRefs , partDims) = dimInfoForDataArray xModel arPartOk
                in
            case (wholeDims,  partDims) of
                -- extend checks to localDimRefs 
                (Just dimsWhole, Just dimsPart) ->
                    let
                        arWholedataTuples = 
                            -- brute force map calc on the whole data arrays with the same localDims
                            if dimsWhole == dimsPart && (Array.length arWholeOk.data) == (Array.length arPartOk.data) then
                                let 
                                    indexArray = Array.fromList (List.range 0 (Array.length arWholeOk.data)) 
                                    dataTuples = Array.Extra.zip arWholeOk.data arPartOk.data
                                in
                                Array.Extra.zip indexArray dataTuples
                            else
                            -- get indices in whole DataArray for each coord in part DataArray and calc the data
                                let
                                    arWholeOkWithDims = { arWholeOk | localDims = Just dimsWhole, localDimRefs = wholeDimRefs }
                                    arPartOkWithDims = { arPartOk | localDims = Just dimsPart, localDimRefs = partDimRefs }
                                    mappedIndices = mapFlatIndices arWholeOkWithDims arPartOkWithDims
                                    extractedWholeDataTuples = Array.Extra.map2 (\ wholeFlIdx partDatum -> 
                                        (wholeFlIdx 
                                        ,  ( (Array.get wholeFlIdx arWholeOk.data) |> Maybe.withDefault (0/0)
                                        , partDatum) )
                                            ) mappedIndices arPartOk.data
                                in
                                extractedWholeDataTuples
                        --calcDataOnPart = Array.Extra.map2 binaryFunc arPart.data arWholedata
                        updatatedWholeData = Array.foldl (\(flIdx, (valWhole, valPart)) dataWhole ->
                            let
                                valCalc = binaryFunc valPart valWhole -- see binaryFunc's for the order of the values
                            in
                            Array.set flIdx valCalc dataWhole
                            ) arWholeOk.data arWholedataTuples
                    in
                    -- Debug.log ("\n\nupdatatedWholeData: " ++ Debug.toString(updatatedWholeData))
                    -- Debug.log ("\narPart: " ++ Debug.toString(arPart))
                    Ok { arWholeOk | data = updatatedWholeData } -- keeps dims and dimRefs, but changes data as calculated
                _ -> Err "Error in updateDataArrayPair"
        (_, _)  -> 
                -- Debug.log ("arWhole: " ++ Debug.toString(arWhole))
                -- Debug.log ("\narPart: " ++ Debug.toString(arPart))
                Err "Error in updateDataArrayPair >= WRONG (Ok arWholeOk, Ok arPartOk)"



-- ============== RANGE FUNCTIONS ==================
getDefaultDataArrayRef : Dataset -> DataArrayRef
getDefaultDataArrayRef dataset = 
    case dataset.defaultDataArrayRef of
        Just ref -> ref
        Nothing -> -- if no default, return the first one
            case dataset.dataArrayRefs of
                [] -> ""
                ref :: _ -> ref
-- used to check duplicated coord names in a Dims specification
type alias CoordSearchDict = Dict Coord (List DimRef)

makeCoordSearchDict : XModel -> DatasetRef -> List DimRef-> Bool -> CoordSearchDict
makeCoordSearchDict xModel datasetRef excludedDimRefs excludeDVars = 
    let
        dataset = getDatasetByRef datasetRef xModel.datasets
            |> Maybe.withDefault emptyDataset
        dims = 
            if excludedDimRefs == [] then
                dimsForDataset xModel.dims dataset
            else
                Dict.filter (\key _ -> not (List.member key excludedDimRefs)) (dimsForDataset xModel.dims dataset)
        dVarDict = 
            if excludeDVars then
                Dict.empty
            else
                List.foldl -- insert dVars and dVarIdentifier
                    (\dVarRef dVarAcc -> 
                        Dict.insert dVarRef [dVarIdentifier] dVarAcc
                    ) Dict.empty dataset.dataArrayRefs
    in   
    Dict.foldl -- add coords and list of dimRefs
        (\dimRef coordArray outerAcc -> 
            Array.foldl 
                (\coord innerAcc -> 
                    case Dict.get coord innerAcc of
                        Just dimRefs -> 
                            Dict.insert coord (dimRef :: dimRefs) innerAcc
                        Nothing -> 
                            Dict.insert coord [dimRef] innerAcc
                ) outerAcc coordArray
        ) dVarDict dims
-- NB check of duplication is relevant only for the same dataset, not for the whole model
-- so be careful not to pass the whole dims of the model, better the calculated localDims of a DataArray
rangeDefToName : XModel -> DatasetRef -> RangeDef -> String
rangeDefToName xModel curDatasetRef rangeDef = 
    let
        curDataset = getDatasetByRef curDatasetRef xModel.datasets |> Maybe.withDefault emptyDataset
        dims = dimsForDataset xModel.dims curDataset
        searchDict = makeCoordSearchDict xModel curDatasetRef [] False

        getUniqueCoordDef : DimRef -> Coord -> Maybe String
        getUniqueCoordDef dimRef coord = 
            case Dict.get coord searchDict of
                Just dimRefs -> 
                    case dimRefs of
                        [oneDimRef] -> Just coord
                        [] -> Nothing -- coord present without matching dim
                        _ -> Just (dimRef ++ "DOT" ++ coord)
                Nothing -> Nothing -- coord not present in searchDict

        coordSpecs : List (DimRef, CoordSpecifier) -> String
        coordSpecs dimCoords = List.foldl 
            (\(dimRef, coordSpec) accStr -> 
                case coordSpecToString coordSpec of 
                    Just coordSpecJust -> 
                        case getUniqueCoordDef dimRef coordSpecJust of
                            Just uniqueCoord -> accStr ++ "_" ++ uniqueCoord
                            Nothing -> accStr ++ "_ERR" ++ dimRef
                    Nothing -> accStr
            ) "" dimCoords
        defArrayRef = getDefaultDataArrayRef curDataset
    in

    case (rangeDef.datasetRef, rangeDef.dataArrayRef, rangeDef.dimCoords) of
        (Nothing, Nothing, Nothing) -> 
            "undef__undef"
        (Just ds, Nothing, Nothing) -> 
            FormulaParser.lowerFirst ds 
        (Nothing, Just da, Nothing) -> 
            (FormulaParser.lowerFirst curDatasetRef) ++ "__" ++ da
        (Just ds, Just da, Nothing) -> 
            (FormulaParser.lowerFirst ds) ++ "__" ++ da
        (Nothing, Nothing, Just dc) ->
            (FormulaParser.lowerFirst curDatasetRef) ++ "__" ++ defArrayRef ++ (coordSpecs dc)
        (Just ds, Nothing, Just dc) -> 
            (FormulaParser.lowerFirst ds) ++ "__" ++ defArrayRef ++ (coordSpecs dc)
        (Nothing, Just da, Just dc) -> 
            (FormulaParser.lowerFirst curDatasetRef) ++ "__" ++ da ++ (coordSpecs dc)
        (Just ds, Just da, Just dc) -> 
            (FormulaParser.lowerFirst ds) ++ "__" ++ da ++ (coordSpecs dc)

-- generalize treatment of trailing "_" to use both for prompts and for parsing in calculation
rangeNameToDef : XModel -> DatasetRef -> String -> Result String RangeDef
rangeNameToDef xModel curDatasetRef rangeName = -- rangeName without trailing "_"
    let
        trySplitDataset = String.split "__" rangeName
        firstTokenOrEmpty = List.head trySplitDataset |> Maybe.withDefault ""
        splitRest lst = -- list of strings separated by "_" in the head of lst
            case List.head lst of
                Just head -> String.split "_" head
                Nothing -> []
        searchDictDs = makeCoordSearchDict xModel curDatasetRef [] False
        isOnlyDatasetNotCoord = 
            List.length trySplitDataset == 1 -- only one segment
            && List.length (splitRest trySplitDataset) == 1 -- no tokens after or in absence of "__ "
                && firstTokenOrEmpty /= "" -- there is a token before or in absence of "__" 
                && Dict.get firstTokenOrEmpty searchDictDs == Nothing -- that token is not a coord or dVar
        inputDs : Result String String
        inputDs = 
                if List.length trySplitDataset > 1 || isOnlyDatasetNotCoord then 
                    let 
                        maybeDs =
                            firstTokenOrEmpty |> FormulaParser.capitalizeFirst
                    in
                    if maybeDs /= "" then
                        if not (datasetExists maybeDs xModel) then 
                            Err ("Undefined dataset: " ++ maybeDs)
                        else 
                            Ok maybeDs
                    else 
                      Ok ""  -- no input dataset
                else
                    Ok ""  -- no input dataset
        ( ds, rest, errMsgDs) = 
            case inputDs of
                Ok "" -> 
                    (curDatasetRef
                    , trySplitDataset |> splitRest
                    , ""
                    )
                Ok inputDsStr ->
                    ( inputDsStr
                    , trySplitDataset |> List.drop 1 |> splitRest
                    , ""
                    )
                Err errMsgStr -> 
                    ("", [], errMsgStr)
        -- makes a dict of coords for the dataset in the range or passed as arg
        curDataset = case getDatasetByRef ds xModel.datasets of
            Just datasetJust -> datasetJust
            Nothing -> emptyDataset
        defArrayRef = getDefaultDataArrayRef curDataset

        searchDict = makeCoordSearchDict xModel ds [] False

        (inputDa, coordSpecsInName, errMsg) = -- scans the rest of the split for coords and dVar
            List.foldl (\token (daAcc, coordAcc, errAcc) -> 
                let
                    maybeCTuple  = getCoordSpec token
                in
                case maybeCTuple of
                    Just (dimRef, SingleCoord coord) -> 
                        if dimRef == dVarIdentifier then
                            (coord, coordAcc, errAcc) -- dVar found signaled by dVarIdentifier
                        else
                            (daAcc, coordAcc ++ [(dimRef, SingleCoord coord)], errAcc) -- dimRef and coord found

                    _ -> (daAcc
                         , coordAcc
                         , errAcc ++ "token " ++ token ++ " undefined" 
                         ) -- not a coord or dVar
            ) ("", [], errMsgDs) rest
        
        (da , isExplDaRef) = 
            if inputDa == "" then
                (defArrayRef, False)
            else
                (inputDa, True)

        getCoordSpec : String -> Maybe (DimRef, CoordSpecifier)
        getCoordSpec coordSpec = 
            let
                splitCoordSpec = String.split "DOT" coordSpec
            in
            case splitCoordSpec of
                [coord] -> -- only coord passed in the name is checked against dims
                    case Dict.get coord searchDict of
                        Just dimRefs ->
                            case dimRefs of
                                [dimRef] -> 
                                    Just (dimRef, SingleCoord coord) -- only one dimRef, ok
                                _ -> Nothing -- more than one dimRef, error ambiguous definition
                        Nothing -> Nothing -- error, coord not found in dims
                [dimRef, coord] -> -- dimRef and coord passed in the name are checked against dims
                    case Dict.get coord searchDict of
                        Nothing-> Nothing
                        Just foundDimRefsForCoord ->
                            if List.member dimRef foundDimRefsForCoord  then
                                Just (dimRef, SingleCoord coord)
                            else
                                Nothing
                _ -> Nothing

    in
    if errMsg /= "" then
        Err errMsg
    else if ds == "" then
        Err "Dataset not found" 
    else if da == "" then
        Err "DataArray not found"
    else
    Ok { datasetRef = Just ds
       , dataArrayRef = Just da
       , dimCoords = Just coordSpecsInName
       , isExplicitDataArrayRef = isExplDaRef }

-- used in codemirror to parse the range name being entered and propose the next possible tokens
promptCoordsForPartialRangeName : XModel -> DatasetRef -> String -> List { label : String }
promptCoordsForPartialRangeName xModel curDatasetRef partialName = 
    let
        --  text after the last "_"
        getSubstrAndLastToken : String -> (String, String)
        getSubstrAndLastToken partialNameArg =
            let
                -- Find the last token by splitting and reversing the string
                tokens = String.split "_" partialNameArg
                lastTokenRet = List.reverse tokens |> List.head |> Maybe.withDefault ""
                lastTokenLength = String.length lastTokenRet
                -- Calculate the position to split the string
                prependedLength = String.length partialNameArg - lastTokenLength

                -- Get the substring before the last token
                beforeLastToken =
                    if prependedLength > 0 then
                        String.left (prependedLength) partialNameArg
                    else
                        ""
            in
            (beforeLastToken, lastTokenRet)

        (prependedRangeName, lastToken) = getSubstrAndLastToken partialName
        prependedRangeNameClean = AppUtil.removeTrailingUnderscores prependedRangeName
        partialRangeDef = rangeNameToDef xModel curDatasetRef prependedRangeNameClean
        -- updates the datasetRef as specified in the partialRangeDef
        (dsRef , isExplDaRef) = case partialRangeDef of
            Ok rangeDef -> case rangeDef.datasetRef of
                Just dsRefJust -> (dsRefJust, rangeDef.isExplicitDataArrayRef)
                Nothing -> ("", False) --curDatasetRef -- if no datasetRef in the partialName, keep the current TODO check if this is correct
            Err _ -> ("" , False) --curDatasetRef
        ds = case getDatasetByRef dsRef xModel.datasets  of
            Just datasetJust -> datasetJust
            Nothing -> emptyDataset
        dims = dimsForDataset xModel.dims ds
        dimRefsInPartialName = 
            case partialRangeDef of
                Ok rangeDef  -> case rangeDef.dimCoords of
                    Just dimCoords -> List.map Tuple.first dimCoords
                    Nothing -> []
                Err _ -> []
                        
        searchDict  = makeCoordSearchDict xModel dsRef dimRefsInPartialName isExplDaRef
        
        -- filter for lastToken and fold available coords, if lastToken="*" fold all
        availableCoords : CoordSearchDict -> String -> List String
        availableCoords searchDictArg lastTokenArg =
            let
                -- Flatten the dict into a list of (compositeKey, originalCoord)
                flattenedList =
                    Dict.foldl
                        (\coord dimRefs acc ->
                            let
                                compositeKeys = -- makes a list of (compositeKey => to sort on, originalCoord)
                                    if String.startsWith lastTokenArg coord || lastTokenArg == "*" then
                                        List.map (\dimRef -> 
                                            if List.length dimRefs > 1 then 
                                                (dimRef ++ "." ++ coord, dimRef ++ "DOT" ++ coord) 
                                            else 
                                                (dimRef ++ "." ++ coord, coord)
                                            ) dimRefs
                                    else
                                        []
                            in
                            compositeKeys ++ acc
                        )
                        []
                        searchDictArg

                -- Sort the list by composite keys
                sortedCompositeKeys =
                    flattenedList
                        |> List.sortWith (\(a, _) (b, _) -> compare a b)

                -- Extract the original coords from the sorted list
                sortedCoords =
                    List.map Tuple.second sortedCompositeKeys
            in
            sortedCoords
        prompts = List.map (\r -> { label = prependedRangeName ++  r }) (availableCoords searchDict lastToken)
    in
    prompts  -- [partialName, prependedRangeName , lastToken ]  [Debug.toString searchDict] --



-- ============== TEST DATA ==================
-- tried to move to separate module but imported types not recognized there

-- ==Dims==
azienda : DimRef
azienda = "azienda"

anno : DimRef
anno = "anno"

voce : DimRef
voce = "voce"

scenario : DimRef
scenario = "scenario"

grChar : DimRef
grChar = "grChar"

cigar : DimRef
cigar = "cigar"

aziendaCoords : Array Coord
-- deCapitalized to make coord names usable as variables, problem with numbers
aziendaCoords = Array.fromList ["alfa", "beta"]
annoCoords : Array Coord
annoCoords = Array.fromList ["2021", "2022", "2023"] -- , "2024", "2025", "2026", "2027", "2028", "2029"]
voceCoords : Array Coord
voceCoords = Array.fromList ["ricavi", "costoVen", "margContrib", "speseVGA", "ebitda", "amm", "ebit", "tax", "unlevNetIncome"]
scenarioCoords : Array Coord
scenarioCoords = Array.fromList ["base", "worst", "best", "forecast"]


caratteriGreciCoords : Array Coord
caratteriGreciCoords = Array.fromList [ "alfa", "beta", "gamma", "delta"]

cigarCoords : Array Coord
cigarCoords = Array.fromList ["alfa", "nazionali"]

myDims : Dims
myDims = Dict.fromList
    [ (azienda, aziendaCoords)
    , (anno, annoCoords)
    , (voce, voceCoords)
    , (scenario, scenarioCoords)
    , (grChar, caratteriGreciCoords)
    , (cigar, cigarCoords)
    ]


-- == ce Dataset ==

ce : DatasetRef
ce = "Ce" -- used as module name for formulas must be capitalized

valore : DataArrayRef
valore = "valore" -- used as default dataArrayRef

note : DataArrayRef
note = "note"
calcValore : DataArrayRef
calcValore = "calcValore"
calcNote : DataArrayRef
calcNote = "calcNote"
valoreList : List Float
valoreList =[100,0/0,0/0,6,0/0,12,0/0,0/0,0/0 -- 2021 Alfa
            ,1007,0/0,0/0,67,0/0,127,0/0,0/0,0/0 -- 2021 Beta
            ,209,0/0,0/0,17,0/0,29,0/0,0/0,0/0 -- 2022 Alfa
            ,2007,0/0,0/0,127,0/0,247,0/0,0/0,0/0 -- 2022 Beta
            ,300,0/0,0/0,18,0/0,16,0/0,0/0,0/0 -- 2023 Alfa
            ,3007,0/0,0/0,187,0/0,167,0/0,0/0,0/0 -- 2023 Beta
            ]
valoreArray : Array Float
valoreArray = Array.fromList (valoreList ) -- ++ valoreList ++ valoreList)

ceDataArrays : DataArrays
ceDataArrays = Dict.fromList
    [ (valore, { ref = valore
                 , datasetRef = Just ce
                 , data = valoreArray
                 , text = Array.empty
                 , localDims = Nothing
                 , localDimRefs = Nothing
                 , pointedFormulas = Dict.empty
                 })
    ]
ceFormulas : ModuleSource
ceFormulas = """module Ce exposing (..)
-- getExprDataArray errors on taxRate, browser hangs; cyclic update may stall?!
-- error in getExprDataArray are overcome by parsing the module
-- check cycle in calcExpressionToXModel
mySub : Float -> Float -> Float -- parsing does not catch declaration errors
mySub val1 val2 = val1 - val2 -- errors are raised in range parsing
taxRate : Float -- declaring aconstant expr is not needed, no longer parsing errors
taxRate = 0.4
-- mapping between datasets thanks to order swap in evalNonVariant before 
-- executing Kernel.twoNumbers when a binary func is applied
costoVen = ce__valore_ricavi * 0.48 * macro__cambioUsdEur_base -- uses expansion of rangeName from getExprDataArray
ce__valore_margContrib = mySub ce__valore_ricavi ce__valore_costoVen
ce__valore_ebitda = ce__valore_margContrib - ce__valore_speseVGA
ce__valore_ebit = ce__valore_ebitda - ce__valore_amm 
ce__valore_tax = ce__valore_ebit * taxRate
ce__valore_unlevNetIncome = ce__valore_ebit - ce__valore_tax


"""

macroFormulas : ModuleSource
macroFormulas = """module Macro exposing (..)
macro__inflazione_forecast = macro__inflazione_base * 1.02
macro__cambioUsdEur_forecast = macro__cambioUsdEur_base * 3.07
macro__cambioCalc = macro__cambioUsdEur * 5 -- calc on dataArrays not yet implemented
dependentDatasetRefs = ["Ce"]

"""

azFormulas : ModuleSource
azFormulas = """module Az exposing (..)
-- add formulas for Az
az__nrDip_beta = az__nrDip_alfa + az__nrDip_alfa -- unary func not working

"""
--     , (note, { ref = note
--                  , datasetRef = Just ce
--                  , data = Array.empty
--                  , text = Array.repeat (Array.length valoreArray) ""
--                  , localDims = Nothing
--                  , localDimRefs = Nothing
--                  })
--    , (calcValore, { ref = calcValore
--                  , datasetRef = Just ce
--                  , data = Array.repeat (Array.length valoreArray)  (0/0) 
--                  , text = Array.empty
--                  , localDims = Nothing
--                  , localDimRefs = Nothing
--                  })
--    , (calcNote, { ref = calcNote
--                  , datasetRef = Just ce
--                  , data = Array.empty
--                  , text = Array.repeat (Array.length valoreArray) ""
--                  , localDims = Nothing
--                  , localDimRefs = Nothing
--                  })



-- == az Dataset ==

az : DatasetRef
az = "Az"
nome : DataArrayRef
nome = "nome"
settore : DataArrayRef
settore = "settore"
nrDip : DataArrayRef
nrDip = "nrDip"

nomeArray : Array String
nomeArray = Array.fromList [ "Alfa SpA", "Beta SpA"
                            , "Alfa SpA", "Beta SpA"
                            , "Alfa SpA", "Beta SpA"]

settoreArray : Array String
settoreArray = Array.fromList [ "Industria", "Commercio" -- per anno
                              , "Industria", "Commercio"
                              , "Industria", "Commercio"
                              ]

nrDipArray : Array Float
nrDipArray = Array.fromList [ 10, 0/0
                            , 12, 0/0
                            , 20, 0/0]

azDataArrays : DataArrays
azDataArrays = Dict.fromList
    [ (nome, { ref = nome
                 , datasetRef = Just az
                 , data = Array.empty
                 , text = nomeArray
                 , localDims = Nothing
                 , localDimRefs = Nothing
                 , pointedFormulas = Dict.empty
                 })
    , (settore, { ref = settore
                , datasetRef = Just az
                 , data = Array.empty
                 , text = settoreArray
                 , localDims = Nothing
                 , localDimRefs = Nothing
                 , pointedFormulas = Dict.empty
                 })
    , (nrDip, { ref = nrDip
                 , datasetRef = Just az
                 , data = nrDipArray
                 , text = Array.empty
                 , localDims = Nothing
                 , localDimRefs = Nothing
                 , pointedFormulas = Dict.empty
                 })
    ]
-- == macro Dataset ==

macro : DatasetRef
macro = "Macro"
inflazione : DataArrayRef
inflazione = "inflazione"

cambioUsdEur : DataArrayRef
cambioUsdEur = "cambioUsdEur"

cambioCalc : DataArrayRef
cambioCalc = "cambioCalc"

inflazioneArray : Array Float
inflazioneArray = Array.fromList [2.3, 10.5, 1.5, 0/0 -- 2021
                                , 3.1, 14, 2.4, 0/0
                                , 4.6, 20, 3.1, 0/0
                                ]


cambioUsdEurArray : Array Float
cambioUsdEurArray = Array.fromList [1.1, 1.2, 1.3, 0/0 -- 2021
                                    , 1.0, 0.9, 0.8, 0/0
                                    , 1.5, 1.6, 1.7, 0/0
                                    ]
cambioCalcArray : Array Float
cambioCalcArray = Array.fromList [0/0, 0/0, 0/0, 0/0
                                    , 0/0, 0/0, 0/0, 0/0
                                    , 0/0, 0/0, 0/0, 0/0
                                    ]
macroDataArrays : DataArrays
macroDataArrays = Dict.fromList
    [ (inflazione, { ref = inflazione
                 , datasetRef = Just macro
                 , data = inflazioneArray
                 , text = Array.empty
                 , localDims = Nothing
                 , localDimRefs = Nothing
                 , pointedFormulas = Dict.empty
                 })
    , (cambioUsdEur, { ref = cambioUsdEur
                 , datasetRef = Just macro
                 , data = cambioUsdEurArray
                 , text = Array.empty
                 , localDims = Nothing
                 , localDimRefs = Nothing
                 , pointedFormulas = Dict.empty
                 })
    , (cambioCalc, { ref = cambioCalc
                    , datasetRef = Just macro
                    , data = cambioCalcArray
                    , text = Array.empty
                    , localDims = Nothing
                    , localDimRefs = Nothing
                    , pointedFormulas = Dict.empty
                    })
    ]
-- === Greche dataset to test duplicate coord names ===

greche : DatasetRef
greche = "Greche"

greekDAr : DataArrayRef
greekDAr = "caratteriGreci"

caratteriGreciArrayText : Array String
caratteriGreciArrayText = Array.fromList [ "α", "β", "γ", "δ", "ε", "ζ", "η", "θ"]

caratteriGreciArrayFloat : Array Float
caratteriGreciArrayFloat = Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7]

myDatasets = Dict.fromList
    [ (ce, { ref = ce
             -- order changed, now seems ok
             , dimRefs = [anno,azienda,voce] -- ordered list of dims
             , dataArrayRefs = [valore] -- [valore, note, calcValore, calcNote]
             , dataArrays = ceDataArrays
             , formulas = ceFormulas
             , defaultDataArrayRef = Just valore
             })
    , (az,  { ref = az
             , dimRefs = [anno, azienda] -- ordered list of dims
             , dataArrayRefs = [nome, settore, nrDip]
             , dataArrays = azDataArrays
             , formulas = azFormulas
             , defaultDataArrayRef = Nothing
             })
    , (macro,   { ref = macro
                , dimRefs = [anno,scenario] -- ordered list of dims
                , dataArrayRefs = [inflazione, cambioUsdEur, cambioCalc]
                , dataArrays = macroDataArrays
                , formulas = macroFormulas
                , defaultDataArrayRef = Nothing
                })
    , (greche,   { ref = greche
                , dimRefs = [grChar, cigar] -- ordered list of dims
                , dataArrayRefs = [greekDAr]
                , dataArrays = Dict.fromList
                    [ (greekDAr, { ref = greekDAr
                        , datasetRef = Just greche
                        , data = caratteriGreciArrayFloat
                        , text = caratteriGreciArrayText
                        , localDims = Nothing
                        , localDimRefs = Nothing
                        , pointedFormulas = Dict.empty
                        })
                    ]
                , formulas = ""
                , defaultDataArrayRef = Nothing
                })
    ]

myXModel : XModel
myXModel = { modelRef = "finPlan"
           , datasetRefs = [ce, az, macro]
           , datasets = myDatasets
           , dims = myDims -- arrays are hyerarchicallly stored in datasets
           , datasetsToRecalc = []
           }


-- == test values for elm-interpreter ==

-- added by Luca
-- env is a store of Value's processable in Expression.evalExpression
-- I added DataArray as Value type, they are visible to console expressions "as is"
-- but cannot be processed, that would require adding logic to Value.toExpr for that type
-- for data marshalling and unmarshalling to/from standard Value types
createTestEnvValues : Dict.Dict String Value
createTestEnvValues =
    -- let                
    --     ds = XModel.myDatasets
    --     dsValue = XModel.datasetsToValue ds
    --     dsReverted = XModel.valueToDatasets dsValue -- should be equal to ds, it works
    -- in
    -- Debug.log( "isDsReverted" ++ Debug.toString (if ds == dsReverted then " dsReverted == ds " else "false)"))
    -- Debug.log( "ds" ++ Debug.toString ds)
    -- Debug.log( "dsReverted" ++ Debug.toString ds)
    Dict.fromList
        [ 
        -- ( "intValue", Int 42 )
        -- , ( "floatValue", Float 3.14 )
        -- , ( "listValue", List [ Int 1, Int 2, Int 3 ] )
        -- , ( "tupleValue", Tuple (Int 1) (Float 2.2) )
        -- , ( "tripleValue", Triple (Int 1) (Float 2.2) (String "three") )
       -- , ( "datasetValue", Dataset myDataset )
        -- let newRecord = { recordValue | name = "Gennaro"} in newRecord
        -- returns only { name = "Gennaro" }
        -- , ( "recordValue", Record (Dict.fromList [ ( "name", String "John" ), ( "age", Int 42 ) ]) )
        -- , ( "intArrayValue", List (dataArrayToListValue intArray ) )
        -- , ( "floatArrayValue", List (dataArrayToListValue floatArray ) )
        -- , ( "stringArrayValue", List (dataArrayToListValue stringArray ) )
        -- disabled after introducing array name parsing
        -- , ("xm", xModelWithArDimsToValue myXModel) -- all shortened
        ]
