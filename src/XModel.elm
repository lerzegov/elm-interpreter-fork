module XModel exposing (..)

-- new version of XArray

import AppUtil exposing (myLog, logIf)
import Array exposing (Array)
import Array.Extra
import FastDict as Dict exposing (Dict)
import FormulaParser
import Html exposing (i, text)
import List.Extra
import Types exposing (Value(..))
import TypesXModel exposing (..)
import XParser exposing (XValue(..))
import AppUtil exposing (myLog)





-- helper expressions and functions to check DataArray contenttype



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
    -- loops on dims to create from all of them a Dict with coord as key and dimRef as value
    Dict.foldl
        (\dimRef coords accDict ->
            Array.foldl (\coord accDict2 -> Dict.insert coord dimRef accDict2) accDict coords
        )
        Dict.empty
        dims

-- helper functions created using Dict.get or Dict.insert in XArrayEngine raises error


getDatasetByRef : DatasetRef -> Datasets -> Maybe Dataset
getDatasetByRef ref datasets =
    Dict.get ref datasets


datasetExists : DatasetRef -> XModel -> Bool
datasetExists ref xModel =
    case Dict.get ref xModel.datasets of
        Just _ ->
            True

        Nothing ->
            False


getDataArrayByRef : DataArrayRef -> DataArrays -> Maybe DataArray
getDataArrayByRef ref dataArrays =
    Dict.get ref dataArrays



getDataArrayWithDimsFromXModel : XModel -> DatasetRef -> DataArrayRef -> Maybe DataArray
getDataArrayWithDimsFromXModel xModel datasetRef dataArrayRef =
    Maybe.andThen
        (\dataset ->
            Maybe.andThen
                (\dataArray ->
                    let
                        localDims =
                            dataArray.localDims

                        localdimRefs =
                            dataArray.localDimRefs

                        ( dimRefs, dims ) =
                            case ( localdimRefs, localDims ) of
                                ( Just localdimRefsJust, Just locDimsJust ) ->
                                    ( Just localdimRefsJust, Just locDimsJust )

                                -- takes dimInfo from XModel
                                _ ->
                                    dimInfoForDataArray xModel dataArray
                    in
                    Just { dataArray | localDimRefs = dimRefs, localDims = dims }
                )
                (Dict.get dataArrayRef dataset.dataArrays)
        )
        (Dict.get datasetRef xModel.datasets)



-- conversion to Value type used by the interpreter


getParsedDataArrayToValue : Maybe XModel -> DatasetRef -> DataArrayRef -> Bool -> Maybe Value
getParsedDataArrayToValue maybeXModel datasetRef dataArrayRef hasExternalDataset =
    Maybe.andThen
        (\xModel ->
            Maybe.andThen
                (\dataArray ->
                    Just (dataArrayWithDimsToValue dataArray hasExternalDataset)
                )
                (getDataArrayWithDimsFromXModel xModel datasetRef dataArrayRef)
        )
        maybeXModel


locDataArrayfromRangeDef : Maybe XModel -> DatasetRef -> DataArrayRef -> List ( DimRef, CoordSpecifier ) -> Maybe DataArray
locDataArrayfromRangeDef maybeXModel datasetRef dataArrayRef dimCoordTuples =
    case maybeXModel of
        Just xModel ->
            let
                arrayWithDims =
                    getDataArrayWithDimsFromXModel xModel datasetRef dataArrayRef
            in
            if List.isEmpty dimCoordTuples then
                arrayWithDims

            else
                case arrayWithDims of
                    Just dataArray ->
                        let
                            locArray =
                                locAr dataArray dimCoordTuples
                        in
                        case locArray of
                            Just locArrayJust ->
                                Just locArrayJust

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing

        Nothing ->
            Nothing


locDataArrayfromRangeDefToValue :
    Maybe XModel
    -> DatasetRef
    -> DataArrayRef
    -> List ( DimRef, CoordSpecifier )
    -> Bool
    -> Maybe Value
locDataArrayfromRangeDefToValue maybeXModel datasetRef dataArrayRef dimCoordTuples hasExternalDataset =
    Maybe.andThen (\locArray -> Just (dataArrayWithDimsToValue locArray hasExternalDataset))
        (locDataArrayfromRangeDef maybeXModel datasetRef dataArrayRef dimCoordTuples)


coordsToDimCoordSpecs : Dims -> List Coord -> List ( DimRef, CoordSpecifier )
coordsToDimCoordSpecs dims coords =
    let
        coordDimDict =
            dimsToCoordDimDict dims

        dimRefsForCoords =
            List.map (\coord -> Dict.get coord coordDimDict |> Maybe.withDefault "") coords
    in
    -- Debug.log ("coordsToDimCoordSpecs: " ++ Debug.toString dims)
    List.map2 (\coord dimRef -> ( dimRef, CoordSingle coord )) coords dimRefsForCoords



-- conversions to elm-interpreter Value type (coded by chatgpt)
-- NB Record is a Value type that can be hyerarchically accessed IN THE CONSOLE
-- with the dot notation, like a JSON object


dataArrayToValue : DataArray -> Bool -> Value
dataArrayToValue dataArray hasExternalDataset =
    let
        locDims =
            dataArray.localDims
    in
    DataAr <|
        Dict.fromList
            [ ( "ref", String dataArray.ref )
            , ( "data", floatArrayToValue dataArray.data )
            , ( "text", strArrayToValue dataArray.text )
            , ( "localDims"
              , case locDims of
                    Just dims ->
                        dimsToValueAsList dims

                    Nothing ->
                        List []
              )
            , ( "localDimRefs"
              , case dataArray.localDimRefs of
                    Just dimRefs ->
                        strListToValue dimRefs

                    Nothing ->
                        List []
              )
            , ( "hasExternalDataset", Bool hasExternalDataset )
            ]


dataArrayWithDimsToValue : DataArray -> Bool -> Value
dataArrayWithDimsToValue dataArray hasExternalDataset =
    let
        localDims =
            dataArray.localDims

        localdimRefs =
            dataArray.localDimRefs

        ( dimRefs, dims ) =
            case ( localdimRefs, localDims ) of
                ( Just localdimRefsJust, Just locDimsJust ) ->
                    ( Just localdimRefsJust, Just locDimsJust )

                _ ->
                    ( Nothing, Nothing )
    in
    case ( dimRefs, dims ) of
        ( Just dimRefsJust, Just dimsJust ) ->
            DataAr <|
                Dict.fromList
                    [ ( "ref", String dataArray.ref )
                    , ( "datasetRef", String (dataArray.datasetRef |> Maybe.withDefault "") )
                    , ( "data", floatArrayToValue dataArray.data )
                    , ( "text", strArrayToValue dataArray.text )
                    , ( "localDims", dimsToValueAsList dimsJust )
                    , ( "localDimRefs", strListToValue dimRefsJust )
                    , ( "hasExternalDataset", Bool hasExternalDataset )
                    ]

        _ ->
            DataAr <|
                Dict.fromList
                    [ ( "ref", String dataArray.ref )
                    , ( "datasetRef", String "" )
                    , ( "data", floatArrayToValue Array.empty )
                    , ( "text", strArrayToValue Array.empty )
                    , ( "localDims", List [] )
                    , ( "localDimRefs", List [] )
                    , ( "hasExternalDataset", Bool hasExternalDataset )
                    ]



-- returns a DataArray given its position in dataArrayRefs


getDataArrayByDVarIndex : Dataset -> Int -> DataArray
getDataArrayByDVarIndex dataset dVarIndex =
    let
        dataArrayRef =
            getAtList dVarIndex dataset.dataArrayRefs
    in
    case dataArrayRef of
        Just dArRef ->
            Dict.get dArRef dataset.dataArrays |> Maybe.withDefault emptyDataArray

        Nothing ->
            emptyDataArray



-- helper funcs used in conversion of array data and text to Value type


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
        dimList =
            Dict.toList <|
                Dict.map
                    (\_ coords ->
                        strArrayToValue coords
                    )
                    dims
    in
    List <| List.map (\( dim, coords ) -> Tuple (String dim) coords) dimList



-- === inverse conversions fron values to XModel types ===


valueToStrArray : Value -> Array String
valueToStrArray value =
    case value of
        JsArray vals ->
            Array.map
                (\v ->
                    case v of
                        String s ->
                            Just s

                        _ ->
                            Nothing
                )
                vals
                |> Array.Extra.filterMap identity

        -- This removes the Nothings, keeping only the Just values.
        _ ->
            Array.empty


valueToStrList : Value -> List String
valueToStrList value =
    case value of
        List vals ->
            List.map
                (\v ->
                    case v of
                        String s ->
                            Just s

                        _ ->
                            Nothing
                )
                vals
                |> List.filterMap identity

        -- This removes the Nothings, keeping only the Just values.
        _ ->
            []


valueToFloatArray : Value -> Array Float
valueToFloatArray value =
    case value of
        JsArray vals ->
            Array.map
                (\v ->
                    case v of
                        Float s ->
                            Just s

                        Int i ->
                            Just (toFloat i)

                        _ ->
                            Nothing
                )
                vals
                |> Array.Extra.filterMap identity

        -- This removes the Nothings, keeping only the Just values.
        List vals ->
            List.map
                (\v ->
                    case v of
                        Float s ->
                            Just s

                        Int i ->
                            Just (toFloat i)

                        _ ->
                            Nothing
                )
                vals
                |> List.filterMap identity
                -- This removes the Nothings, keeping only the Just values.
                |> Array.fromList

        _ ->
            Array.empty


valueToStringArray : Value -> Array String
valueToStringArray value =
    case value of
        JsArray vals ->
            Array.map
                (\v ->
                    case v of
                        String s ->
                            Just s

                        _ ->
                            Nothing
                )
                vals
                |> Array.Extra.filterMap identity

        -- This removes the Nothings, keeping only the Just values.
        _ ->
            Array.empty


valueToDims : Value -> Dims
valueToDims value =
    case value of
        List dimsList ->
            let
                dimsTupleList =
                    List.map valueToDimTuple dimsList
            in
            List.foldl (\( dimRef, coordArray ) accDict -> Dict.insert dimRef coordArray accDict) Dict.empty dimsTupleList

        _ ->
            Dict.empty


valueToDimsTest : Value -> String
valueToDimsTest value =
    case value of
        List dimsList ->
            let
                dimsTupleList =
                    List.map valueToDimTuple dimsList
            in
            -- Debug.toString (List.foldl (\(dimRef, coordArray) accDict -> Dict.insert dimRef coordArray accDict) Dict.empty dimsTupleList)
            Debug.toString dimsList ++ "\n" ++ Debug.toString dimsTupleList

        _ ->
            "no pattern match"


valueToDimTuple : Value -> ( DimRef, Array Coord )
valueToDimTuple value =
    case value of
        Tuple (String dimRef) (JsArray coordArray) ->
            ( dimRef, valueToStrArray (JsArray coordArray) )

        _ ->
            ( "", Array.empty )



-- hasExternalDataset not marshalled into xModel, used only in the interpreter


valueToDataArray : Value -> DataArray
valueToDataArray value =
    case value of
        DataAr dict ->
            let
                ref =
                    case Dict.get "ref" dict of
                        Just (String s) ->
                            s

                        _ ->
                            ""

                datasetRef =
                    case Dict.get "datasetRef" dict of
                        Just (String s) ->
                            Just s

                        _ ->
                            Nothing

                data =
                    case Dict.get "data" dict of
                        Just (JsArray floats) ->
                            valueToFloatArray (JsArray floats)

                        _ ->
                            Array.empty

                text =
                    case Dict.get "text" dict of
                        Just (JsArray strings) ->
                            valueToStrArray (JsArray strings)

                        _ ->
                            Array.empty

                localDims =
                    case Dict.get "localDims" dict of
                        Just val ->
                            Just (valueToDims val)

                        _ ->
                            Nothing

                localDimRefs =
                    case Dict.get "localDimRefs" dict of
                        Just val ->
                            Just (valueToStrList val)

                        _ ->
                            Nothing

                -- pointeFormulas not handled
            in
            DataArray ref datasetRef data text localDims localDimRefs Dict.empty

        -- PROVVI
        _ ->
            --DataArray "" Nothing Array.empty Array.empty Nothing Nothing
            emptyDataArray



-- FUNCTIONS FOR HANDLING STRUCTURE AND DATA
-- returns number of categDims in a DataArray


nrDims : Dataset -> Int
nrDims dataset =
    List.length dataset.dimRefs



-- kept annotation similar to following functions
-- alternative is XModel -> DatasetRef -> Array Coord
-- in a Dataset, the coords of the DVarDim (hard coded dVar) are the array names in dataArrayRefs


getDVarDimCoords : Dataset -> Array Coord
getDVarDimCoords dataset =
    let
        dVarCoords =
            Array.fromList dataset.dataArrayRefs
    in
    dVarCoords



-- returns the coords of a categDim or a dVarDim in a Dataset
-- assumes dims are unique by name in XModel
-- needs Dataset arg to get the dVarDim coords


dimOrDArCoords : Dims -> Dataset -> DimOrDAr -> Array Coord
dimOrDArCoords dims dataset dimOrDArRef =
    case dimOrDArRef of
        CategDimRef dim ->
            Dict.get dim dims |> Maybe.withDefault Array.empty

        DVarDimRef _ ->
            getDVarDimCoords dataset



-- returns the name of a categDim or a dVarDim


dimOrDArName : DimOrDAr -> String
dimOrDArName dimOrDAr =
    case dimOrDAr of
        CategDimRef dimRef ->
            dimRef

        DVarDimRef dVarRef ->
            dVarRef


dimOrDArRefByDimName : Dataset -> String -> DimOrDAr
dimOrDArRefByDimName dataset dimName =
    let
        maybeDim : Maybe DimRef
        maybeDim =
            List.filter (\dim -> dim == dimName) dataset.dimRefs
                |> List.head
    in
    case maybeDim of
        Just dim ->
            CategDimRef dim

        Nothing ->
            DVarDimRef dVarIdentifier



-- returns a list of dim coords sizes for a given list of dimOrDArRefs in a Dataset, including dVarDim


shape : Dims -> Dataset -> List DimOrDAr -> List Int
shape dims dataset dimVarRefs =
    List.map
        (\dimVarRef ->
            let
                dimVarCoords =
                    dimOrDArCoords dims dataset dimVarRef
            in
            Array.length dimVarCoords
        )
        dimVarRefs



-- returns a list of dim coords sizes for a given list of dimRefs, dVarDim ignored


shapeDim : Dims -> List DimRef -> List Int
shapeDim dims dimRefs =
    List.map
        (\dRef ->
            let
                dimCoords =
                    Dict.get dRef dims |> Maybe.withDefault Array.empty
            in
            Array.length dimCoords
        )
        dimRefs



-- simple helper function


coordsByDimRef : Dims -> DimRef -> Array Coord
coordsByDimRef dims dimRef =
    Dict.get dimRef dims |> Maybe.withDefault Array.empty



-- for a Dataset, returns the index (pos) in the Dims list (iter order)
-- for a given DimRef (categDim)


dimIndexByDimRef : Dataset -> DimRef -> Maybe Int
dimIndexByDimRef dataset dimRef =
    elemIndexList dimRef dataset.dimRefs



-- duplicated here to avoid module error in the interpreter
-- for a Dataset, returns the index (pos) of the coord in the coords of a dim
-- for a given tuple (String=dimRef, Coord)


coordIndexByDimRefCoordTuples : Dims -> ( DimRef, Coord ) -> Maybe Int
coordIndexByDimRefCoordTuples dims ( dimRef, coord ) =
    let
        coords =
            Dict.get dimRef dims |> Maybe.withDefault Array.empty
    in
    elemIndexAr coord coords


datasetForDataArray : XModel -> DataArray -> Maybe Dataset
datasetForDataArray xModel dataArray =
    case dataArray.datasetRef of
        Just ref ->
            Dict.get ref xModel.datasets

        Nothing ->
            Nothing



-- returns the full Dim info for the DimRefs in a DataArray, witouht filtering


dimInfoForDataArray : XModel -> DataArray -> ( Maybe (List DimRef), Maybe Dims )
dimInfoForDataArray xModel dataArray =
    let
        dataset =
            datasetForDataArray xModel dataArray
    in
    case ( dataArray.localDimRefs, dataArray.localDims, dataset ) of
        ( Just dimRefs, Just dims, _ ) ->
            ( Just dimRefs, Just dims )

        ( Just dimRefs, Nothing, _ ) ->
            ( Just dimRefs, dimsForDimRefs xModel dimRefs )

        ( Nothing, Nothing, Just datasetJust ) ->
            ( Just datasetJust.dimRefs, dimsForDimRefs xModel datasetJust.dimRefs )

        _ ->
            ( Just [ "empty", "dims" ], Just Dict.empty )



-- returns the full Dim info for the DimRefs in a Dataset, witouht filtering


dimInfoForDatasetRef : XModel -> DatasetRef -> ( Maybe (List DimRef), Maybe Dims )
dimInfoForDatasetRef xModel datasetRef =
    let
        maybeDataset =
            getDatasetByRef datasetRef xModel.datasets
    in
    case maybeDataset of
        Just dataset ->
            ( Just dataset.dimRefs, dimsForDimRefs xModel dataset.dimRefs )

        Nothing ->
            ( Just [ "empty", "dims" ], Just Dict.empty )



-- returns the size of the data array for a given DataArray with localDims


calcDataLengthFromDims : DataArray -> Maybe Int
calcDataLengthFromDims dataArrayWithDims =
    Maybe.andThen
        (\dims ->
            let
                sizes =
                    Dict.map (\_ coords -> Array.length coords) dims |> Dict.values
            in
            Just (List.foldl (*) 1 sizes)
        )
        dataArrayWithDims.localDims



-- strides are calculated starting from the last (innernost) dimension
-- which has stride 1; the strides for the preceding dimensions are the product
-- of the sizes of the following dimensions
-- the result is a list of strides, one for each dimension, reversed to match the order of the dims


calculateStrides : List Size -> List Stride
calculateStrides sizes =
    let
        reversedSizes =
            List.reverse sizes

        cumulativeProduct =
            scanl (*) 1 reversedSizes
    in
    cumulativeProduct
        |> List.reverse
        |> List.drop 1



-- used by iloc


cartesianProductPosVecs : Maybe (List (Array CoordIndex)) -> Maybe (Array PosVec)
cartesianProductPosVecs maybeLists =
    case maybeLists of
        Just lists ->
            Just (cartesianProductListOfArToArOfList lists)

        Nothing ->
            Nothing



-- given dims in XModel and Dataset, for a list of Maybe posVecs matched by a list fo DimVariants,
-- returns a Maybe list of flat indices
-- strides calculated in the function
-- used by iloc


calcFlatIndices : Dims -> Dataset -> Maybe (Array PosVec) -> List DimOrDAr -> Maybe (Array FlatIndex)
calcFlatIndices dims dataset maybePosVecs dimRefs =
    case maybePosVecs of
        -- non controllo che le dimensioni di posvec e strides siano uguali
        Just posVecs ->
            let
                curShape =
                    shape dims dataset dimRefs

                strides =
                    calculateStrides curShape
            in
            Just <|
                Array.map
                    (\posVec ->
                        if List.member -1 posVec then
                            -1

                        else
                            -- same as calcFlatIndexFast strides indices withouth Maybe
                            List.foldl (\( index, stride ) acc -> acc + index * stride) 0 (zip posVec strides)
                    )
                    posVecs

        Nothing ->
            Nothing



-- for a given list of DimOrDAr, with strides precalculated,
-- returns the index in the flat array of the data
-- for its posvec, i.e. positions of the coords in the respective DimVariant


calcFlatIndexFast : List Stride -> PosVec -> Maybe FlatIndex
calcFlatIndexFast strides posVec =
    if List.length posVec == List.length strides then
        if List.member -1 posVec then
            -- -1 is a placeholder for missing or invalid values
            Nothing

        else
            Just <| List.foldl (\( index, stride ) acc -> acc + index * stride) 0 (zip posVec strides)

    else
        -- posvec and strides must have the same length
        Nothing



-- given a DataArray with localDims and a list of Maybe posVecs,
-- returns a Maybe list of flat indices, assumes dVarIndex is jointly handled as in setDatum...


calcFlatIndicesAr : DataArray -> Maybe (Array PosVec) -> Maybe (Array FlatIndex)
calcFlatIndicesAr dataArray maybePosVecs =
    let
        dims =
            dataArray.localDims |> Maybe.withDefault Dict.empty

        dimRefs =
            dataArray.localDimRefs |> Maybe.withDefault []
    in
    case maybePosVecs of
        -- non controllo che le dimensioni di posvec e strides siano uguali
        Just posVecs ->
            let
                curShape =
                    shapeDim dims dimRefs

                strides =
                    calculateStrides curShape
            in
            Just <|
                Array.map
                    (\posVec ->
                        if List.member -1 posVec then
                            -1

                        else
                            -- same as calcFlatIndexFast strides indices withouth Maybe
                            List.foldl (\( index, stride ) acc -> acc + index * stride) 0 (zip posVec strides)
                    )
                    posVecs

        Nothing ->
            Nothing



-- returns flat indices for a list of posvecs, given only XModel dims
-- dVars non considered
-- used by iloc


calcFlatIndicesFast : Dims -> List DimRef -> Array PosVec -> Array FlatIndex
calcFlatIndicesFast dims dimRefs posVecs =
    let
        sizes =
            shapeDim dims dimRefs

        strides =
            calculateStrides sizes
    in
    Array.map
        (\posVec ->
            if List.member -1 posVec then
                -1

            else
                -- same as calcFlatIndexFast strides indices withouth Maybe
                List.foldl (\( index, stride ) acc -> acc + index * stride) 0 (zip posVec strides)
        )
        posVecs



-- converts a FlatIndex to a PosVec, given the ordered list of dim sizes
-- spiegazione matlab
-- https://stackoverflow.com/questions/12429492/how-to-convert-a-monodimensional-index-to-corresponding-indices-in-a-multidimens


flatIndexToPosVec : FlatIndex -> List Size -> Maybe PosVec
flatIndexToPosVec flatIndex sizes =
    let
        strides =
            calculateStrides sizes

        sizesWithStrides =
            zip sizes strides

        nrDim =
            List.length sizes

        calcIndexByDim flatIdx j size stride =
            if j == 0 then
                -- first dim
                flatIdx // stride
                -- division with integer result

            else if j == nrDim - 1 then
                -- last dim
                remainderBy size flatIdx
                -- NB 1st arg is divisor!!

            else
                remainderBy size (flatIdx // stride)

        indices =
            List.indexedMap (\j ( sz, st ) -> calcIndexByDim flatIndex j sz st) sizesWithStrides
    in
    Just indices


itemPosVecsForDims : Dims -> List DimRef -> Array PosVec
itemPosVecsForDims dims dimRefs =
    let
        sizes =
            shapeDim dims dimRefs

        indicesByDim =
            List.map (\size -> List.range 0 (size - 1)) sizes

        posVecs =
            cartesianProduct indicesByDim
    in
    Array.fromList posVecs


itemCoordVecsForDims : Dims -> List DimRef -> Array CoordVec
itemCoordVecsForDims dims dimRefs =
    let
        curCoords =
            List.map
                (\dimRef ->
                    Dict.get dimRef dims
                        |> Maybe.withDefault Array.empty
                        |> Array.toList
                )
                dimRefs

        coordVecs =
            cartesianProduct curCoords
    in
    Array.fromList coordVecs



-- helper function to convert an IndexSpecifier to a list of filtered indices


expandIndexSpecifier : Dims -> Dataset -> DimOrDAr -> IndexSpecifier -> Array CoordIndex
expandIndexSpecifier dims dataset dimVarRef specifier =
    let
        dimVarCoords =
            dimOrDArCoords dims dataset dimVarRef
    in
    case specifier of
        IndexSingle idx ->
            Array.fromList [ idx ]

        IndexRange ( idxStart, idxEnd ) ->
            Array.fromList (List.range idxStart idxEnd)

        IndexList indices ->
            indices

        IndexNone ->
            Array.fromList (List.range 0 (Array.length dimVarCoords - 1))



-- used on DataArrays with localDims


expandIndexSpecifierAr : DataArray -> DimRef -> IndexSpecifier -> Array CoordIndex
expandIndexSpecifierAr dataArray dimRef specifier =
    let
        dims =
            dataArray.localDims |> Maybe.withDefault Dict.empty

        dimCoords =
            Dict.get dimRef dims |> Maybe.withDefault Array.empty
    in
    case specifier of
        IndexSingle idx ->
            Array.fromList [ idx ]

        IndexRange ( idxStart, idxEnd ) ->
            Array.fromList (List.range idxStart idxEnd)

        IndexList indices ->
            indices

        IndexNone ->
            Array.fromList (List.range 0 (Array.length dimCoords - 1))



-- helper functions to construct (dimRef, IndexSpecifier) tuples passed  to iloc


toSingleIdx : DimRef -> CoordIndex -> ( DimRef, IndexSpecifier )
toSingleIdx dimRef idx =
    ( dimRef, IndexSingle idx )


toIdxList : DimRef -> Array CoordIndex -> ( DimRef, IndexSpecifier )
toIdxList dimRef idxList =
    ( dimRef, IndexList idxList )


toIdxRange : DimRef -> CoordIndex -> CoordIndex -> ( DimRef, IndexSpecifier )
toIdxRange dimRef idxStart idxEnd =
    ( dimRef, IndexRange ( idxStart, idxEnd ) )


toIdxNone : DimRef -> ( DimRef, IndexSpecifier )
toIdxNone dimRef =
    ( dimRef, IndexNone )


dimsForDataset : Dims -> Dataset -> Dims
dimsForDataset xModelDims dataset =
    let
        dimRefs =
            dataset.dimRefs
    in
    Dict.filter (\k _ -> List.member k dimRefs) xModelDims


dimsForDimRefs : XModel -> List DimRef -> Maybe Dims
dimsForDimRefs xModel dimRefs =
    let
        rawDims =
            Dict.filter (\k _ -> List.member k dimRefs) xModel.dims
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
        dim1Refs =
            Dict.keys dims1

        dim2Refs =
            Dict.keys dims2

        matchesCheckResult =
            if dim1Refs == dim2Refs then
                List.foldl
                    (\curRef curMatchRefTuple ->
                        let
                            coords1 =
                                Dict.get curRef dims1 |> Maybe.withDefault Array.empty

                            coords2 =
                                Dict.get curRef dims2 |> Maybe.withDefault Array.empty

                            coordsNotEmptyMatch =
                                coords1 == coords2 && coords1 /= Array.empty

                            bothSingleCoords =
                                Array.length coords1 == 1 && Array.length coords2 == 1

                            prevMatches =
                                Tuple.first curMatchRefTuple

                            prevRef =
                                Tuple.second curMatchRefTuple
                        in
                        if coordsNotEmptyMatch then
                            ( prevMatches + 1, prevRef )

                        else if bothSingleCoords then
                            ( prevMatches, curRef )

                        else
                            ( prevMatches, prevRef )
                    )
                    ( 0, "" )
                    dim1Refs

            else
                ( 0, "" )

        pivotDimRef =
            if Tuple.first matchesCheckResult == List.length dim1Refs - 1 then
                Just (Tuple.second matchesCheckResult)

            else
                Nothing
    in
    pivotDimRef


dimOrDArRefsForDataset : Dataset -> Bool -> List DimOrDAr
dimOrDArRefsForDataset dataset withDVar =
    let
        dimRefs =
            dataset.dimRefs

        categDimRefs =
            List.map (\k -> CategDimRef k) dimRefs

        dVarInList =
            if withDVar then
                [ DVarDimRef dVarIdentifier ]

            else
                []
    in
    categDimRefs ++ dVarInList



-- takes a list of tuples (dim, IndexSpecifier) as input, completes it with a loop over dimensions,
-- and then extracts the data into a Maybe DataArray
-- NB new DataArray belonging to a Dataset has no info about dims, only the data
-- the DataArray returned by iloc has the localDims and localDimRefs fields set
-- why use DimVariantRef and not DimRef? => to handle DVarDimRef in XView


iloc : Dims -> Dataset -> DataArray -> List ( DimOrDAr, IndexSpecifier ) -> Maybe DataArray
iloc dims dataset dataArray dimRefIndicesTuples =
    let
        -- loop on dataset dimRefs ++ dVar and complete dimRefIndicesTuples with IndexNone for missing dims
        completeDimRefIndicesTuples : List ( DimOrDAr, IndexSpecifier )
        completeDimRefIndicesTuples =
            List.map
                (\dimVarRef ->
                    let
                        filteredTuple =
                            List.filter (\( d, _ ) -> d == dimVarRef) dimRefIndicesTuples

                        foundSpecifier =
                            filteredTuple
                                |> List.head
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault IndexNone
                    in
                    -- Debug.log ("iloc filteredTuple: " ++ Debug.toString filteredTuple)
                    ( dimVarRef, foundSpecifier )
                )
                (dimOrDArRefsForDataset dataset False)

        -- false to exclude DVarDimRef
        localExpandSpecifier dimRefArg dimIndexSpecifier =
            expandIndexSpecifier dims dataset dimRefArg dimIndexSpecifier

        dimVariantRefs : List DimOrDAr
        dimVariantRefs =
            List.map Tuple.first completeDimRefIndicesTuples

        dimRefs : List DimRef
        dimRefs =
            List.map (Tuple.first >> dimVariantToDimRefString) completeDimRefIndicesTuples

        expandedIndices =
            List.map
                (\locTuple ->
                    localExpandSpecifier (Tuple.first locTuple) (Tuple.second locTuple)
                )
                completeDimRefIndicesTuples

        -- provisional: makes a local copy of Dims with filtered dims and coords used by the DataArray
        updatedDimsList : List ( DimRef, Array Coord )
        updatedDimsList =
            -- returns a list of dim records with filtered coords
            List.map2
                (\dimRef indices ->
                    let
                        dimCoords =
                            Dict.get dimRef dims |> Maybe.withDefault Array.empty

                        filteredCoords =
                            Array.map (\idx -> Array.get idx dimCoords |> Maybe.withDefault "") indices
                    in
                    ( dimRef, filteredCoords )
                )
                dimRefs
                expandedIndices

        updatedDims : Dims
        updatedDims =
            Dict.fromList updatedDimsList

        flatIndices =
            Just expandedIndices
                -- gets posVecs from expandedIndices then calculates flat indices
                |> cartesianProductPosVecs
                |> Maybe.andThen (\posVecs -> calcFlatIndices dims dataset (Just posVecs) dimVariantRefs)

        ( filteredData, filteredText ) =
            case flatIndices of
                Just indices ->
                    ( extractElementsArray dataArray.data indices, extractElementsArray dataArray.text indices )

                Nothing ->
                    -- if flatIndices is Nothing, return an empty ArrayVariant of tyhe same type
                    ( Array.empty, Array.empty )
    in
    -- Debug.log ("dimRefIndicesTuples: " ++ Debug.toString dimRefIndicesTuples)
    -- Return the filtered DataArray with updated dimensions and data
    Just { dataArray | data = filteredData, text = filteredText, localDims = Just updatedDims, localDimRefs = Just dimRefs }



-- used on complete DataArray, dataset DVars not handled only one DVar over DataArray


ilocAr : DataArray -> List ( DimRef, IndexSpecifier ) -> Maybe DataArray
ilocAr dataArray dimRefIndicesTuples =
    let
        dims =
            dataArray.localDims |> Maybe.withDefault Dict.empty

        dimRefs =
            dataArray.localDimRefs |> Maybe.withDefault []

        completeDimRefIndicesTuples : List ( DimRef, IndexSpecifier )
        completeDimRefIndicesTuples =
            List.map
                (\dimRef ->
                    let
                        filteredTuple =
                            List.filter (\( d, _ ) -> d == dimRef) dimRefIndicesTuples

                        foundSpecifier =
                            filteredTuple
                                |> List.head
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault IndexNone
                    in
                    -- Debug.log ("iloc filteredTuple: " ++ Debug.toString filteredTuple)
                    ( dimRef, foundSpecifier )
                )
                dimRefs

        -- false to exclude DVarDimRef
        localExpandSpecifier dimRefArg dimIndexSpecifier =
            expandIndexSpecifierAr dataArray dimRefArg dimIndexSpecifier

        expandedIndices =
            List.map
                (\locTuple ->
                    localExpandSpecifier (Tuple.first locTuple) (Tuple.second locTuple)
                )
                completeDimRefIndicesTuples

        updatedDimsList : List ( DimRef, Array Coord )
        updatedDimsList =
            -- returns a list of dim records with filtered coords
            List.map2
                (\dimRef indices ->
                    let
                        dimCoords =
                            Dict.get dimRef dims |> Maybe.withDefault Array.empty

                        filteredCoords =
                            Array.map (\idx -> Array.get idx dimCoords |> Maybe.withDefault "") indices
                    in
                    ( dimRef, filteredCoords )
                )
                dimRefs
                expandedIndices

        updatedDims : Dims
        updatedDims =
            Dict.fromList updatedDimsList

        flatIndices =
            Just expandedIndices
                -- gets posVecs from expandedIndices then calculates flat indices
                |> cartesianProductPosVecs
                |> Maybe.andThen (\posVecs -> calcFlatIndicesAr dataArray (Just posVecs))

        ( filteredData, filteredText ) =
            case flatIndices of
                Just indices ->
                    ( extractElementsArray dataArray.data indices, extractElementsArray dataArray.text indices )

                Nothing ->
                    -- if flatIndices is Nothing, return an empty ArrayVariant of tyhe same type
                    ( Array.empty, Array.empty )
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
            coord =
                String.dropLeft 8 str |> String.trim
        in
        CoordSingle coord

    else if String.startsWith "range::" str then
        let
            coords =
                String.dropLeft 7 str |> String.split "," |> List.map String.trim
        in
        case coords of
            [ coordFirst, coordLast ] ->
                CoordRange ( coordFirst, coordLast )

            _ ->
                CoordNone

    else if String.startsWith "list::" str then
        let
            coords =
                Array.fromList (String.dropLeft 6 str |> String.split "," |> List.map String.trim)
        in
        CoordList coords

    else
        CoordNone



-- helper functions to construct (dim, LocSpecifier) tuples passed to loc


toSingleCoord : DimRef -> Coord -> ( DimOrDAr, CoordSpecifier )
toSingleCoord dimRef coord =
    ( CategDimRef dimRef, CoordSingle coord )


toCoordList : DimRef -> Array Coord -> ( DimOrDAr, CoordSpecifier )
toCoordList dimRef coordArray =
    ( CategDimRef dimRef, CoordList coordArray )


toCoordRange : DimRef -> Coord -> Coord -> ( DimOrDAr, CoordSpecifier )
toCoordRange dimRef coordStart coordEnd =
    ( CategDimRef dimRef, CoordRange ( coordStart, coordEnd ) )


toCoordNone : DimRef -> ( DimOrDAr, CoordSpecifier )
toCoordNone dimRef =
    ( CategDimRef dimRef, CoordNone )


toSingleCoordAr : DimRef -> Coord -> ( DimRef, CoordSpecifier )
toSingleCoordAr dimRef coord =
    ( dimRef, CoordSingle coord )


toCoordListAr : DimRef -> Array Coord -> ( DimRef, CoordSpecifier )
toCoordListAr dimRef coordArray =
    ( dimRef, CoordList coordArray )


toCoordRangeAr : DimRef -> Coord -> Coord -> ( DimRef, CoordSpecifier )
toCoordRangeAr dimRef coordStart coordEnd =
    ( dimRef, CoordRange ( coordStart, coordEnd ) )


toCoordNoneAr : DimRef -> ( DimRef, CoordSpecifier )
toCoordNoneAr dimRef =
    ( dimRef, CoordNone )



-- takes a list of tuples (dimVarRef, LocSpecifier) as input, completes it with a loop over dimensions,
-- chatgpt implementation wraps iloc, with wrong coords returns all coords like CoordNone


loc : Dims -> Dataset -> DataArray -> List ( DimOrDAr, CoordSpecifier ) -> Maybe DataArray
loc dims dataset dataArray dimRefCoordsTuples =
    -- Convert dimCoordsTuples to IndexSpecifiers and call iloc
    let
        indexSpecifiers =
            List.map
                (\( dimRef, coordSpecifier ) ->
                    ( dimRef, convertSpecifier dims dataset dimRef coordSpecifier |> Maybe.withDefault IndexNone )
                )
                dimRefCoordsTuples
    in
    -- Debug.log ("dimRefCoordsTuples: " ++ Debug.toString dimRefCoordsTuples)
    iloc dims dataset dataArray indexSpecifiers


locAr : DataArray -> List ( DimRef, CoordSpecifier ) -> Maybe DataArray
locAr dataArray dimRefCoordsTuples =
    -- Convert dimCoordsTuples to IndexSpecifiers and call iloc
    let
        indexSpecifiers =
            List.map
                (\( dimRef, coordSpecifier ) ->
                    ( dimRef, convertSpecifierAr dataArray dimRef coordSpecifier |> Maybe.withDefault IndexNone )
                )
                dimRefCoordsTuples
    in
    -- Debug.log ("dimRefCoordsTuples: " ++ Debug.toString dimRefCoordsTuples)
    ilocAr dataArray indexSpecifiers



-- not used in elm-foldbook nor here


getDataArrayByLoc : XModel -> DatasetRef -> DataArrayRef -> List String -> Maybe DataArray
getDataArrayByLoc xModel datasetRef dataArrayRef dimRefCoordsStrPatterns =
    let
        dataset =
            getDatasetByRef datasetRef xModel.datasets |> Maybe.withDefault emptyDataset

        dataArray =
            getDataArrayByRef dataArrayRef dataset.dataArrays |> Maybe.withDefault emptyDataArray

        dimRefCoordsTuples =
            List.map strToDimRefCoordsTuple dimRefCoordsStrPatterns
    in
    loc xModel.dims dataset dataArray dimRefCoordsTuples



-- helper function
-- takes a string `dimRef||coordSpecifier` and returns a tuple (dimRef, CoordSpecifier)


strToDimRefCoordsTuple : String -> ( DimOrDAr, CoordSpecifier )
strToDimRefCoordsTuple str =
    let
        ( dimVariantRef, coordPart ) =
            case String.split "||" str of
                [ only ] ->
                    -- no "||" in sttr
                    ( only, "" )

                [ dimRef, coords ] ->
                    ( dimRef, coords )

                _ ->
                    ( "", "" )
    in
    case coordPart of
        "" ->
            ( CategDimRef dimVariantRef, CoordNone )

        _ ->
            if String.contains ".." coordPart then
                -- CoordRange detected
                let
                    coords =
                        String.split ".." coordPart
                in
                case coords of
                    [ firstCoord, lastCoord ] ->
                        ( CategDimRef dimVariantRef, CoordRange ( firstCoord, lastCoord ) )

                    _ ->
                        ( CategDimRef dimVariantRef, CoordNone )
                -- Fallback for malformed range

            else
                -- Treat as CoordList or SingleCoord
                let
                    coords =
                        Array.fromList (String.split "," coordPart)
                in
                case Array.length coords of
                    1 ->
                        ( CategDimRef dimVariantRef, CoordSingle (Array.get 0 coords |> Maybe.withDefault "") )

                    _ ->
                        ( CategDimRef dimVariantRef, CoordList coords )


strToDimRefCoordsTupleVerbose : String -> ( DimOrDAr, CoordSpecifier )
strToDimRefCoordsTupleVerbose str =
    let
        -- Split the string into DimVariantRef and the specifier part.
        parts =
            String.split "||" str

        dimVariantRef : DimOrDAr
        dimVariantRef =
            case List.head parts of
                Just ref ->
                    CategDimRef ref

                Nothing ->
                    CategDimRef ""

        -- Consider how to handle errors or empty strings appropriately.
        specifierStr =
            case List.drop 1 parts of
                spec :: _ ->
                    spec

                [] ->
                    ""

        -- Determine the type of coord specifier based on the specifier string.
        coordSpecifier =
            strToCoordSpecifier specifierStr
    in
    ( dimVariantRef, coordSpecifier )



-- returns Dict from Coord to CoordIndex


coordDict : Dims -> Dataset -> DimOrDAr -> CoordDict
coordDict dims dataset dimVarRef =
    let
        coordsforDim =
            dimOrDArCoords dims dataset dimVarRef
    in
    Dict.fromList
        (Array.toList coordsforDim
            |> List.indexedMap (\j coord -> ( coord, j ))
        )


coordDictFast : Array Coord -> CoordDict
coordDictFast coordsforDim =
    Dict.fromList
        (Array.indexedMap (\j coord -> ( coord, j )) coordsforDim
            |> Array.toList
        )


coordToIndex : CoordDict -> Coord -> CoordIndex
coordToIndex dimVarDict coord =
    case Dict.get coord dimVarDict of
        Just idx ->
            idx

        Nothing ->
            -1



-- Convert coordSpecifier to IndexSpecifier


convertSpecifier : Dims -> Dataset -> DimOrDAr -> CoordSpecifier -> Maybe IndexSpecifier
convertSpecifier dims dataset dimVar specifier =
    let
        coordsforDim =
            dimOrDArCoords dims dataset dimVar

        coordDictLocal =
            Dict.fromList
                (Array.toList coordsforDim
                    |> List.indexedMap (\j coord -> ( coord, j ))
                )
    in
    case specifier of
        CoordSingle coord ->
            Dict.get coord coordDictLocal |> Maybe.map IndexSingle

        CoordRange ( startCoord, endCoord ) ->
            case ( Dict.get startCoord coordDictLocal, Dict.get endCoord coordDictLocal ) of
                ( Just startIndex, Just endIndex ) ->
                    if startIndex <= endIndex then
                        Just (IndexRange ( startIndex, endIndex ))

                    else
                        Nothing

                _ ->
                    Nothing

        CoordList coords ->
            let
                indices =
                    Array.map
                        (\coord ->
                            Dict.get coord coordDictLocal |> Maybe.withDefault -1
                        )
                        coords
            in
            Just (IndexList indices)

        CoordNone ->
            Just IndexNone


convertSpecifierAr : DataArray -> DimRef -> CoordSpecifier -> Maybe IndexSpecifier
convertSpecifierAr dataArray dim specifier =
    let
        dims =
            dataArray.localDims |> Maybe.withDefault Dict.empty

        coordsforDim =
            Dict.get dim dims |> Maybe.withDefault Array.empty

        coordDictLocal =
            Dict.fromList
                (Array.toList coordsforDim
                    |> List.indexedMap (\j coord -> ( coord, j ))
                )
    in
    case specifier of
        CoordSingle coord ->
            Dict.get coord coordDictLocal |> Maybe.map IndexSingle

        CoordRange ( startCoord, endCoord ) ->
            case ( Dict.get startCoord coordDictLocal, Dict.get endCoord coordDictLocal ) of
                ( Just startIndex, Just endIndex ) ->
                    if startIndex <= endIndex then
                        Just (IndexRange ( startIndex, endIndex ))

                    else
                        Nothing

                _ ->
                    Nothing

        CoordList coords ->
            let
                indices =
                    Array.map
                        (\coord ->
                            Dict.get coord coordDictLocal |> Maybe.withDefault -1
                        )
                        coords
            in
            Just (IndexList indices)

        CoordNone ->
            Just IndexNone



-- returns dataArray filtered by indices


updateDataArrayByIndices : DataArray -> Array FlatIndex -> DataArray
updateDataArrayByIndices dataArray indices =
    { dataArray
        | data = extractElementsArray dataArray.data indices
        , text = extractElementsArray dataArray.text indices
    }


updateDataArraysByIndices : List DataArray -> Array FlatIndex -> List DataArray
updateDataArraysByIndices dataArrays indices =
    List.map
        (\dataArray ->
            updateDataArrayByIndices dataArray indices
        )
        dataArrays


findDVarDimIndex : Array DimOrDAr -> Maybe Int
findDVarDimIndex specifiers =
    findIndex (\dimOrDAr -> isDVarDim dimOrDAr) specifiers



-- findIndex redefined here with elemIndex


isDVarDim : DimOrDAr -> Bool
isDVarDim dimOrDAr =
    case dimOrDAr of
        DVarDimRef _ ->
            True

        _ ->
            False



-- returns the datum from a Dataset as string from flatIndex and position of the current variable in dVar


getStrDatumFromDatasetForFlatIndexDVarIndex : Dataset -> FlatIndex -> Int -> String
getStrDatumFromDatasetForFlatIndexDVarIndex dataset flatIndex dVarIndex =
    let
        dataArrayRef =
            getAtList dVarIndex dataset.dataArrayRefs

        retDatum =
            case dataArrayRef of
                Just dArRef ->
                    let
                        dAr =
                            Dict.get dArRef dataset.dataArrays |> Maybe.withDefault emptyDataArray

                        dVarDatum =
                            Array.get flatIndex dAr.text
                    in
                    dVarDatum

                Nothing ->
                    Just ""
    in
    retDatum |> Maybe.withDefault ""



-- returns XValue handled by XParser in SpreadsheetUI


getXValueDatumFromDatasetForFlatIndexDVarIndex : Dataset -> FlatIndex -> Int -> XValue
getXValueDatumFromDatasetForFlatIndexDVarIndex dataset flatIndex dVarIndex =
    let
        dataArrayRef =
            getAtList dVarIndex dataset.dataArrayRefs
    in
    case dataArrayRef of
        Just dArRef ->
            let
                dAr =
                    Dict.get dArRef dataset.dataArrays |> Maybe.withDefault emptyDataArray

                dVarDatum =
                    if isDataArrayEmpty dAr then
                        XEmpty

                    else if isDataArrayData dAr || isDataArrayMixed dAr then
                        case Array.get flatIndex dAr.data of
                            Just value ->
                                if isNaN value then
                                    XEmpty

                                else
                                    XFloat value

                            Nothing ->
                                XEmpty
                        -- Handle the case where the index is out of bounds or data is not as expected

                    else if isDataArrayText dAr then
                        case Array.get flatIndex dAr.text of
                            Just value ->
                                XString value

                            Nothing ->
                                XEmpty
                        -- Handle the case where the index is out of bounds or text is not as expected

                    else
                        XEmpty

                -- Handle other cases, if necessary
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
                updatedData =
                    setDatumFromStringToArray dAr flatIndex datum
            in
            updatedData

        -- Update the data array within the dataset, if it exists
        dataArrayRef =
            getAtList dVarIndex dataset.dataArrayRefs

        updatedDataArrays =
            case dataArrayRef of
                Just arRef ->
                    let
                        upDataAr =
                            Dict.get arRef dataset.dataArrays |> Maybe.withDefault emptyDataArray

                        updatedDataArray =
                            updateDataArray upDataAr
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
                updatedData =
                    setXValueDatumToArray dAr flatIndex datum
            in
            updatedData

        -- Update the data array within the dataset, if it exists
        dataArrayRef =
            getAtList dVarIndex dataset.dataArrayRefs

        updatedDataArrays =
            case dataArrayRef of
                Just arRef ->
                    let
                        upDataAr =
                            Dict.get arRef dataset.dataArrays |> Maybe.withDefault emptyDataArray

                        updatedDataArray =
                            updateDataArray upDataAr
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
        updatedData =
            Array.Extra.update flatIndex (\_ -> Maybe.withDefault (0 / 0) (String.toFloat datum)) dataArray.data

        updatedText =
            Array.Extra.update flatIndex (\_ -> datum) dataArray.text
    in
    { dataArray | data = updatedData, text = updatedText }


setXValueDatumToArray : DataArray -> FlatIndex -> XValue -> DataArray
setXValueDatumToArray dataArray flatIndex datum =
    let
        updatedData =
            case datum of
                XFloat value ->
                    if isDataArrayData dataArray then
                        Array.Extra.update flatIndex (\r -> value) dataArray.data

                    else
                        dataArray.data

                XString _ ->
                    dataArray.data

                XEmpty ->
                    if isDataArrayData dataArray then
                        Array.Extra.update flatIndex (\r -> 0 / 0) dataArray.data

                    else
                        dataArray.data

                XError _ ->
                    dataArray.data

        updatedText =
            case datum of
                XFloat _ ->
                    dataArray.text

                XString strValue ->
                    Array.Extra.update flatIndex (\r -> strValue) dataArray.text

                XEmpty ->
                    if isDataArrayText dataArray then
                        Array.Extra.update flatIndex (\r -> "") dataArray.text

                    else
                        dataArray.text

                -- provisional, error msg always in text
                XError errMsg ->
                    Array.Extra.update flatIndex (\r -> errMsg) dataArray.text
    in
    { dataArray | data = updatedData, text = updatedText }



-- duplicates extractElementsArray that has no default value assignment


getDataFromArrayForIndices : Array FlatIndex -> Array a -> a -> Array a
getDataFromArrayForIndices indices dataArray defaultValue =
    Array.map (\i -> Array.get i dataArray |> Maybe.withDefault defaultValue) indices


updateDataArrayName : Dataset -> DataArrayRef -> String -> Dataset
updateDataArrayName dataset dataArrayRef newName =
    let
        dataArray =
            Dict.get dataArrayRef dataset.dataArrays 
                
        updatedDataArrays =
            case dataArray of
                Just dAr ->
                    Dict.insert dataArrayRef
                        { dAr | ref = newName }
                        dataset.dataArrays

                Nothing ->
                    dataset.dataArrays

    in
    { dataset | dataArrays = updatedDataArrays }


-- COORD , DIM AND DATA ARRAY INSERTING DELETING AND RENAMING
newCoordName : DimRef -> Dims -> Coord
newCoordName dimRef dims =
    let
        dimCoords =
            Dict.get dimRef dims |> Maybe.withDefault Array.empty

        coordName =
            dimRef ++ String.fromInt (Array.length dimCoords + 1)
    in
    coordName

newDataArrayName : Dataset -> Coord
newDataArrayName curDataset =
    let
        dataArrayRefs =
            curDataset.dataArrayRefs

        coordName =
            dVarIdentifier ++ String.fromInt (List.length dataArrayRefs + 1)
    in
    coordName
appendCoord : Coord -> DimRef -> HeaderPath -> XModel -> XModel
appendCoord coord dimRef curHeaderPath curXModel =
    let
        maybeCoords =
            Dict.get dimRef curXModel.dims

        updatedDims =
            case maybeCoords of
                Just coords ->
                    Dict.insert dimRef (Array.append coords (Array.fromList [ coord ])) curXModel.dims

                Nothing ->
                    Dict.insert dimRef (Array.fromList [ coord ]) curXModel.dims
        updatedHeaderPath =
            updateHeaderPath curHeaderPath coord
    in
    updateXModelFromCoordChange updatedDims InsertRemoveCoord updatedHeaderPath curXModel

updateHeaderPath : HeaderPath -> Coord -> HeaderPath
updateHeaderPath curHeaderPath coord =
    case List.reverse curHeaderPath of
        curCell::rest ->
            (Tuple.first curCell, coord) :: rest |> List.reverse

        _ ->
            curHeaderPath

removeCoord : DimRef -> Coord -> HeaderPath -> XModel -> XModel
removeCoord dimRef coord curHeaderPath curXModel =
    let
        maybeCoords =
            Dict.get dimRef curXModel.dims

        (updatedDims , returnedHeaderPath) =
            case maybeCoords of
                Just coords ->
                    if (Array.length coords) > 1 then
                        let
                            coordIndex =
                                elemIndexAr coord coords |> Maybe.withDefault -1
                            coordIndexToFocus =
                                if coordIndex + 1 == Array.length coords then
                                    coordIndex - 1
                                else
                                    coordIndex + 1
                            coordToFocus =
                                Array.get coordIndexToFocus coords |> Maybe.withDefault "errorGettingCoord"
                            updatedHeaderPath =
                                       updateHeaderPath curHeaderPath coordToFocus
                        in
                        ( Dict.insert dimRef (Array.Extra.removeWhen (\c -> c == coord) coords) curXModel.dims
                        , updatedHeaderPath
                        )
                    else
                        ( curXModel.dims , curHeaderPath )
                Nothing ->
                    ( curXModel.dims , curHeaderPath )
    in
    updateXModelFromCoordChange updatedDims InsertRemoveCoord returnedHeaderPath curXModel

removeCoordAt : Int ->  DimRef -> HeaderPath -> XModel -> XModel
removeCoordAt coordIndex dimRef curHeaderPath curXModel =
    let
        maybeCoords =
            Dict.get dimRef curXModel.dims

        (updatedDims , returnedHeaderPath) =
            case maybeCoords of
                Just coords ->
                    if (Array.length coords) > 1 then
                        let
                            coordIndexToFocus =
                                if coordIndex + 1 == Array.length coords then
                                    coordIndex - 1
                                else
                                    coordIndex + 1
                            coordToFocus =
                                Array.get coordIndexToFocus coords |> Maybe.withDefault "errorGettingCoord"
                            updatedHeaderPath =
                                       updateHeaderPath curHeaderPath coordToFocus
                        in
                        ( Dict.insert dimRef (Array.Extra.removeAt coordIndex coords) curXModel.dims
                        , updatedHeaderPath
                        )
                    else
                        ( curXModel.dims , curHeaderPath )
                    

                Nothing ->
                    ( curXModel.dims , curHeaderPath )
    in
    updateXModelFromCoordChange updatedDims InsertRemoveCoord returnedHeaderPath curXModel


-- helper function handling the remapping of data in a DataArray to added or modifed coords in Dims, 
-- updatedDims have no new or deleted dims, only updated coords
updateXModelFromCoordChange : Dims -> CoordChange -> HeaderPath -> XModel -> XModel
updateXModelFromCoordChange updatedDims changeType updatedHeaderPath curXModel =
    let
        -- loop on curXModel.dims, compare coords with updatedDims for the same key, if different add the key to the returned lis
        changedDimRefs : List DimRef
        changedDimRefs =
            Dict.foldl
                (\dimRef coords accList ->
                    case Dict.get dimRef updatedDims of
                        Just updatedCoords ->
                            if coords /= updatedCoords then
                                dimRef :: accList

                            else
                                accList

                        Nothing ->
                            accList
                )
                []
                curXModel.dims
        
        -- 
        affectedDatasets =
            List.filter (\datasetRef -> AppUtil.intersectBool changedDimRefs (Dict.get datasetRef curXModel.datasets 
                |> Maybe.withDefault emptyDataset).dimRefs) curXModel.datasetRefs

        updateDataset curDataset =
            let
                updatedDataArrays =
                    case changeType of
                        RenameCoord ->
                            curDataset.dataArrays

                        InsertRemoveCoord ->
                            Dict.map (\_ dataArray -> 
                                let
                                    resultDataArray =
                                        myLog "resultDataArray remapped"
                                        (remapDataInDataArrayToNewDims curXModel (Ok dataArray) (Nothing , updatedDims ))
                                in
                                case resultDataArray of
                                    Ok dataArrayOk ->
                                        dataArrayOk

                                    Err _ ->
                                        dataArray
                            ) 
                            curDataset.dataArrays
            in
            { curDataset | dataArrays = updatedDataArrays }

        updatedDatasets =
            List.foldl
                (\datasetRef acc ->
                    Dict.update datasetRef (Maybe.map updateDataset) acc
                )
                curXModel.datasets
                affectedDatasets
        datasetRefsToRecalc =
            List.append curXModel.datasetsToRecalc affectedDatasets
        -- no dimChangesToProcess
    in
    { curXModel 
        | dims = updatedDims
        , datasets = updatedDatasets
        , datasetsToRecalc = datasetRefsToRecalc 
        , updatedHeaderPath = Just updatedHeaderPath
    }
newCoord : DimRef -> Coord -> HeaderPath -> XModel -> XModel
newCoord dimRef curCoord headerPath curXModel =
    let
        coord =
            newCoordName dimRef curXModel.dims
        updatedDims = 
            insertCoordAfter coord dimRef curCoord curXModel.dims
        updatedHeaderPath =
            updateHeaderPath headerPath coord
    in
    updateXModelFromCoordChange updatedDims InsertRemoveCoord updatedHeaderPath curXModel

newDataArray : DatasetRef -> DataArrayRef -> HeaderPath -> XModel -> XModel
newDataArray curDatasetRef curDataArrayRef headerPath curXModel =
    let -- default creates a dataArray with data not text
        curDataset = -- no cross dataset updates
            Dict.get curDatasetRef curXModel.datasets |> Maybe.withDefault emptyDataset
        dataArrayRef =
            newDataArrayName curDataset
        curDataArray =
            getDataArrayByRef curDataArrayRef curDataset.dataArrays 
                |> Maybe.withDefault emptyDataArray
        dataLength =
            if isDataArrayData curDataArray then
                Array.length curDataArray.data
            else
                Array.length curDataArray.text
        updatedDataArray =
            { emptyDataArray
                | ref = dataArrayRef
                , datasetRef = Just curDatasetRef
                , data = Array.repeat dataLength (0/0)
                , text = Array.empty
            }
        updatedDataArrayRefs = 
            List.append curDataset.dataArrayRefs [dataArrayRef]
        updatedHeaderPath =
            updateHeaderPath headerPath dataArrayRef
        updatedDataArrays =
            Dict.insert dataArrayRef updatedDataArray curDataset.dataArrays
        updatedDataset =
            { curDataset | dataArrayRefs = updatedDataArrayRefs, dataArrays = updatedDataArrays }
        updatedDatasets =
            Dict.insert curDatasetRef updatedDataset curXModel.datasets
        updatedXModel =
            { curXModel 
                | datasets = updatedDatasets 
                , updatedHeaderPath = Just updatedHeaderPath
                }
    in
    updatedXModel

removeDataArray : DatasetRef -> DataArrayRef -> HeaderPath -> XModel -> XModel
removeDataArray curDatasetRef curDataArrayRef headerPath curXModel =
    let
        curDataset =
            Dict.get curDatasetRef curXModel.datasets |> Maybe.withDefault emptyDataset
        updatedDataArrayRefs =
            List.filter (\ref -> ref /= curDataArrayRef) curDataset.dataArrayRefs
        updatedDataArrays =
            Dict.remove curDataArrayRef curDataset.dataArrays
        updatedDataset =
            { curDataset | dataArrayRefs = updatedDataArrayRefs, dataArrays = updatedDataArrays }
        updatedDatasets =
            Dict.insert curDatasetRef updatedDataset curXModel.datasets
        dArIndex =
            elemIndexList curDataArrayRef curDataset.dataArrayRefs |> Maybe.withDefault -1
        dArIndexToFocus =
            if dArIndex + 1 == List.length curDataset.dataArrayRefs then
                dArIndex - 1
            else
                dArIndex + 1
        dArToFocus =
            getAtList dArIndexToFocus curDataset.dataArrayRefs |> Maybe.withDefault "errorGettingCoord"
        updatedHeaderPath =
            updateHeaderPath headerPath dArToFocus
    in
    { curXModel 
        | datasets = updatedDatasets 
        , updatedHeaderPath = Just updatedHeaderPath
    }
insertCoordAt : Coord -> DimRef -> Int -> Dims -> Dims
insertCoordAt coord dimRef coordIndex dims =
    let
        maybeCoords =
            Dict.get dimRef dims
    in
    case maybeCoords of
        Just coords ->
            Dict.insert dimRef (Array.Extra.insertAt coordIndex coord coords) dims

        Nothing ->
            dims

insertCoordBefore : Coord -> DimRef -> Coord -> Dims -> Dims
insertCoordBefore coord dimRef curCoord dims =
    let
        maybeCoords =
            Dict.get dimRef dims
    in
    case maybeCoords of
        Just coords ->
            let
                coordIndex =
                    elemIndexAr curCoord coords |> Maybe.withDefault -1
            in
            if coordIndex >= 0 then
                Dict.insert dimRef (Array.Extra.insertAt coordIndex coord coords) dims

            else
                dims

        Nothing ->
            dims

insertCoordAfter : Coord -> DimRef -> Coord -> Dims -> Dims
insertCoordAfter coord dimRef curCoord dims =
    let
        maybeCoords =
            Dict.get dimRef dims
    in
    case maybeCoords of
        Just coords ->
            let
                coordIndex =
                    elemIndexAr curCoord coords |> Maybe.withDefault -1
            in
            if coordIndex >= 0 then
                Dict.insert dimRef (Array.Extra.insertAt (coordIndex+1) coord coords) dims

            else
                dims

        Nothing ->
            dims

-- coord and array name updates
renameCoord : Dims -> DimOrDAr -> Coord -> XValue -> HeaderPath -> XModel -> XModel
renameCoord dims dimVariant oldCoordValue newCoordValue curHeaderPath curXModel =
    case dimVariant of
        CategDimRef dimRef ->
            let
                updatedDims =
                    updateDimCoordValue dims dimVariant oldCoordValue newCoordValue
                updatedHeaderPath =
                    updateHeaderPath curHeaderPath (XParser.render newCoordValue)
            in
            updateXModelFromCoordChange updatedDims RenameCoord updatedHeaderPath curXModel

        _ ->
            curXModel

-- dimRef and array name updates
newDimName : Dims -> DimRef
newDimName dims =
    let
        dimName =
            "dim" ++ AppUtil.getUppercaseLetter (List.length (Dict.keys dims) + 1)
    in
    dimName

newDimRef : DimRef -> DatasetRef -> Tray -> XModel -> XModel
newDimRef dimName curDatasetRef tray curXModel =
    updateXModelFromDimRefChange curDatasetRef curXModel.dims (InsertDim dimName tray ) curXModel

renameDimRef : Dims -> DatasetRef -> DimRef  -> XValue -> XModel -> XModel
renameDimRef dims curDatasetRef oldDimRef newDimRefValue  curXModel =
-- used both for real renaming and "linking" to an exisitng dimRef
    let
        changedDimRef =
            XParser.render newDimRefValue
        hasToLink = myLog "hasToLink" <|
            List.member changedDimRef (Dict.keys dims) && oldDimRef /= changedDimRef
        updatedDims =
            if hasToLink then
                updateDimsFromDimRefRename dims oldDimRef newDimRefValue
            else -- left if to handle future cases
                updateDimsFromDimRefRename dims oldDimRef newDimRefValue
            
        changeToProcess = --myLog "changeToProcess" <|
            if hasToLink  then
                LinkDim oldDimRef changedDimRef
            else
                RenameDim oldDimRef changedDimRef 
    in
    updateXModelFromDimRefChange curDatasetRef updatedDims changeToProcess curXModel

-- only in current dataset
removeDimRef : DimRef -> DatasetRef  -> XModel -> XModel
removeDimRef dimName curDatasetRef curXModel =
    updateXModelFromDimRefChange curDatasetRef curXModel.dims (RemoveDim dimName) curXModel

-- in all datasets and in XModel dims
destroyDimRef : DimRef -> DatasetRef  -> XModel -> XModel
destroyDimRef dimName curDatasetRef curXModel =
    updateXModelFromDimRefChange curDatasetRef curXModel.dims (DestroyDim dimName) curXModel

-- provisional, may be rationalized to single functions for each type of change
-- DVarDimRef is filtered out before and not handled here, only CategDimRef
updateXModelFromDimRefChange : DatasetRef  -> Dims -> DimChange -> XModel -> XModel
updateXModelFromDimRefChange curDatasetRef passedDims changeType curXModel =
    case changeType of
        RenameDim oldDimRef changedDimRef ->
            let
                affectedDatasets =
                    List.filter (\datasetRef -> AppUtil.intersectBool [oldDimRef] (Dict.get datasetRef curXModel.datasets 
                        |> Maybe.withDefault emptyDataset).dimRefs) curXModel.datasetRefs
                --_ = myLog "RenameDim" <|
                --    (oldDimRef, changedDimRef, affectedDatasets)

                updateDataset curDataset =
                    let
                        updatedDimRefs =
                            AppUtil.renameListItem oldDimRef changedDimRef curDataset.dimRefs
                    in
                    { curDataset | dimRefs = updatedDimRefs }

                updatedDatasets =
                    List.foldl
                        (\datasetRef acc ->
                            Dict.update datasetRef (Maybe.map updateDataset) acc
                        )
                        curXModel.datasets
                        affectedDatasets
                datasetRefsToRecalc =
                    List.append curXModel.datasetsToRecalc affectedDatasets
                dimChangesToProcess =
                    List.append curXModel.dimChanges [changeType]
            in
            { curXModel 
                | dims = passedDims
                , datasets = updatedDatasets
                , datasetsToRecalc = datasetRefsToRecalc 
                , dimChanges = dimChangesToProcess
                , updatedDimOrDArRef = Just changedDimRef
            }
        InsertDim dimName tray ->
            let
                -- no other affectedDatasets
                curDataset =
                    Dict.get curDatasetRef curXModel.datasets |> Maybe.withDefault emptyDataset
                coordName =
                    newCoordName dimName passedDims
                updatedDims =
                    Dict.insert dimName (Array.fromList [coordName]) passedDims
                updatedDimRefs =
                        List.append curDataset.dimRefs [dimName]
                updatedDataArrays =
                        Dict.map (\_ dataArray -> 
                                let
                                    resultDataArray =
                                        remapDataInDataArrayToNewDims 
                                            curXModel 
                                            (Ok dataArray) 
                                            (Just updatedDimRefs , updatedDims )
                                in
                                case resultDataArray of
                                    Ok dataArrayOk ->
                                        dataArrayOk

                                    Err _ ->
                                        dataArray
                            ) 
                            curDataset.dataArrays
                updatedDataset  =

                    { curDataset 
                        | dimRefs = updatedDimRefs 
                        , dataArrays = updatedDataArrays
                    }

                updatedDatasets =
                    Dict.map
                        (\datasetRef dataset ->
                            if datasetRef == curDataset.ref then
                                updatedDataset
                            else
                                dataset
                        )
                        curXModel.datasets
                        
                datasetRefsToRecalc =
                    List.append curXModel.datasetsToRecalc [updatedDataset.ref]
                dimChangesToProcess = curXModel.dimChanges
                    -- List.append curXModel.dimChanges [changeType]  -- disabled to avoid double view generations
            in
            { curXModel 
                | dims = updatedDims
                , datasets = updatedDatasets
                , datasetsToRecalc = datasetRefsToRecalc 
                , dimChanges = dimChangesToProcess
                , updatedDimOrDArRef = Just dimName
            }

        RemoveDim dimName ->
            let
                datasetsWithRemovedDim =
                    List.filter (\datasetRef -> AppUtil.intersectBool [dimName] (Dict.get datasetRef curXModel.datasets 
                        |> Maybe.withDefault emptyDataset).dimRefs) curXModel.datasetRefs
                affectedDatasets =
                    [curDatasetRef] 
                    

                updatedDims =
                    if List.length datasetsWithRemovedDim == 1 then
                        Dict.remove dimName passedDims
                    else    
                        passedDims
                updateDataset curDatasetArg =
                    let
                        updatedDimRefs =
                            List.filter (\dimRef -> dimRef /= dimName) curDatasetArg.dimRefs
                        updatedDataArrays =
                                Dict.map (\_ dataArray -> 
                                        let
                                            resultDataArray =
                                                remapDataInDataArrayToNewDims 
                                                    curXModel 
                                                    (Ok dataArray) 
                                                    (Just updatedDimRefs , updatedDims )
                                        in
                                        case resultDataArray of
                                            Ok dataArrayOk ->
                                                dataArrayOk

                                            Err _ ->
                                                dataArray
                                    ) 
                                    curDatasetArg.dataArrays
                        
                    in
                    { curDatasetArg 
                        | dimRefs = updatedDimRefs 
                        , dataArrays = updatedDataArrays}
                
                curDataset =
                    Dict.get curDatasetRef curXModel.datasets |> Maybe.withDefault emptyDataset
                    

                updatedDatasets =
                    List.foldl
                        (\datasetRef acc ->
                            Dict.update datasetRef (Maybe.map updateDataset) acc
                        )
                        curXModel.datasets
                        affectedDatasets
                datasetRefsToRecalc =
                    List.append curXModel.datasetsToRecalc affectedDatasets
                dimChangesToProcess =
                    List.append curXModel.dimChanges [changeType]
            in
            { curXModel 
                | dims = updatedDims
                , datasets = updatedDatasets
                , datasetsToRecalc = datasetRefsToRecalc 
                , dimChanges = dimChangesToProcess
               -- , updatedDimRef = updatedDimRef -- not here, tray not visible
            }
        LinkDim oldDimRef changedDimRef -> 
            let
                -- no other affectedDatasets
                -- just substitute the oldDimRef with the changedDimRef in the dataset dimRefs
                -- then get the updatedDims mapping the changed dimRefs
                -- from the existing dim in xModel and remap the dataArrays
                curDataset =
                    Dict.get curDatasetRef curXModel.datasets |> Maybe.withDefault emptyDataset
                updatedDimRefs =
                        AppUtil.renameListItem oldDimRef changedDimRef curDataset.dimRefs
                updatedDims =
                    dimsForDimRefs curXModel updatedDimRefs |> Maybe.withDefault curXModel.dims
                updatedDataArrays =
                        Dict.map (\_ dataArray -> 
                                let
                                    resultDataArray =
                                        remapDataInDataArrayToNewDims 
                                            curXModel 
                                            (Ok  dataArray)
                                            (Just updatedDimRefs , updatedDims )
                                in
                                case resultDataArray of
                                    Ok dataArrayOk ->
                                        dataArrayOk

                                    Err _ ->
                                        dataArray
                            ) 
                            curDataset.dataArrays
                updatedDataset  =
                    { curDataset 
                        | dimRefs = updatedDimRefs 
                        , dataArrays = updatedDataArrays
                    }

                updatedDatasets =
                    Dict.map
                        (\datasetRef dataset ->
                            if datasetRef == curDataset.ref then
                                updatedDataset
                            else
                                dataset
                        )
                        curXModel.datasets
                        
                datasetRefsToRecalc =
                    List.append curXModel.datasetsToRecalc [updatedDataset.ref]
                dimChangesToProcess = curXModel.dimChanges
                    -- List.append curXModel.dimChanges [changeType]  -- disabled to avoid double view generations
            in
            { curXModel 
                | dims = updatedDims
                , datasets = updatedDatasets
                , datasetsToRecalc = datasetRefsToRecalc 
                , dimChanges = dimChangesToProcess
            }
        DestroyDim dimName ->
            let
                datasetsWithRemovedDim =
                    List.filter (\datasetRef -> AppUtil.intersectBool [dimName] (Dict.get datasetRef curXModel.datasets 
                        |> Maybe.withDefault emptyDataset).dimRefs) curXModel.datasetRefs
                affectedDatasets =
                    datasetsWithRemovedDim
                    

                updatedDims =
                        Dict.remove dimName passedDims
                updateDataset curDataset =
                    let
                        updatedDimRefs =
                            List.filter (\dimRef -> dimRef /= dimName) curDataset.dimRefs
                        updatedDataArrays =
                                Dict.map (\_ dataArray -> 
                                        let
                                            resultDataArray =
                                                remapDataInDataArrayToNewDims 
                                                    curXModel 
                                                    (Ok dataArray) 
                                                    (Just updatedDimRefs , updatedDims )
                                        in
                                        case resultDataArray of
                                            Ok dataArrayOk ->
                                                dataArrayOk

                                            Err _ ->
                                                dataArray
                                    ) 
                                    curDataset.dataArrays
                        
                    in
                    { curDataset 
                        | dimRefs = updatedDimRefs 
                        , dataArrays = updatedDataArrays}

                updatedDatasets =
                    List.foldl
                        (\datasetRef acc ->
                            Dict.update datasetRef (Maybe.map updateDataset) acc
                        )
                        curXModel.datasets
                        affectedDatasets
                datasetRefsToRecalc =
                    List.append curXModel.datasetsToRecalc affectedDatasets
                dimChangesToProcess =
                    List.append curXModel.dimChanges [changeType]
            in
            { curXModel 
                | dims = updatedDims
                , datasets = updatedDatasets
                , datasetsToRecalc = datasetRefsToRecalc 
                , dimChanges = dimChangesToProcess
            }


updateDimsFromDimRefRename : Dims -> DimRef -> XValue -> Dims
updateDimsFromDimRefRename dims oldDimRef newDimRefValue =
    let
        maybeCoords =
            Dict.get oldDimRef dims
    in
    case (maybeCoords , newDimRefValue) of
        (Just coords , XString newDimRefString) ->
            let
                updatedDims =
                    Dict.insert newDimRefString coords (Dict.remove oldDimRef dims)
            in
            updatedDims

        _ ->
            dims
updateDimCoordValue : Dims -> DimOrDAr -> Coord -> XValue -> Dims
updateDimCoordValue dims dimVariant oldCoordValue newCoordValue =
    case ( dimVariant , newCoordValue )of
        ( CategDimRef dimRef , XString newStringValue )-> 
            Dict.update dimRef
                (Maybe.map (\coordsArray ->
                    let
                        updatedCoords =
                            Array.indexedMap (\index existingCoord ->
                                if existingCoord == oldCoordValue then
                                    newStringValue
                                else
                                    existingCoord
                            ) coordsArray
                    in
                    updatedCoords
                ))
                dims

        _ -> -- no update for DVarDimRef
            dims

renameDataArray : XModel -> DatasetRef -> DataArrayRef -> DataArrayRef -> XModel
renameDataArray xModel datasetRef oldRef newRef =
    let
        maybeDataset =
            getDatasetByRef datasetRef xModel.datasets
    in
    case maybeDataset of
        Just dataset ->
            let
                maybeDataArray =
                    getDataArrayByRef oldRef dataset.dataArrays
            in
            case maybeDataArray of
                Just dataArray ->
                    let
                        updatedDataArrays =
                            Dict.insert newRef { dataArray | ref = newRef } 
                                (Dict.remove oldRef dataset.dataArrays)
                        updatedDataArrayRefs =
                            List.map (\ref -> if ref == oldRef then newRef else ref) dataset.dataArrayRefs

                        updatedDataset =
                            { dataset 
                                | dataArrays = updatedDataArrays 
                                , dataArrayRefs = updatedDataArrayRefs
                                }

                        updatedDatasets =
                            Dict.insert datasetRef updatedDataset xModel.datasets
                    in
                    -- Debug.log ("updatedDataArrayRefs: " ++ Debug.toString updatedDataArrayRefs)
                    { xModel | datasets = updatedDatasets }


                Nothing ->
                    xModel

        Nothing ->
            xModel






-- === DataArray functions used in the interpreter ===
-- MAPPING DIMENSIONS AND OPERATIONS ON DATAARRAYS


type alias BinaryFuncFloat =
    Float -> Float -> Float


type alias UnaryFuncFloat =
    Float -> Float


type alias AggrUnaryFuncFloat =
    Array Float -> Float


type alias RankUnaryFuncFloat =
    Array Float -> Float -> Float


type alias AggrBinaryFuncFloat =
    Array Float -> Array Float -> Float


type alias BinaryFuncString =
    String -> String -> String


type alias UnaryFuncString =
    String -> String


type alias AggrFuncString =
    Array String -> String


-- 1-based rank, duplicates get the same rank
arRank : RankUnaryFuncFloat
arRank floatArray val =
    let
        sortedList =
            List.sort (Array.toList floatArray)

        n =
            List.length sortedList

        rank =
            List.Extra.elemIndex val sortedList |> Maybe.withDefault -1 |> toFloat
    in
    rank + 1


arSum : AggrUnaryFuncFloat
arSum floatArray =
    Array.foldl (+) 0 floatArray


arAverage : AggrUnaryFuncFloat
arAverage floatArray =
    let
        n =
            Array.length floatArray |> toFloat
    in
    arSum floatArray / n


arMedian : AggrUnaryFuncFloat
arMedian floatArray =
    let
        sortedList =
            List.sort (Array.toList floatArray)

        n =
            List.length sortedList

        mid =
            n // 2

        sortedArray =
            Array.fromList sortedList
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
                |> List.filter (\( _, count ) -> Just count == maxCount)
                |> List.map Tuple.first
    in
    if List.isEmpty modeList then
        0

    else
        List.head modeList |> Maybe.withDefault 0


arProduct : AggrUnaryFuncFloat
arProduct floatArray =
    Array.foldl (*) 1 floatArray



-- Generalized arMax function


arMax : AggrUnaryFuncFloat
arMax floatArray =
    let
        initValue =
            Array.get 0 floatArray

        foldFunction =
            \val acc -> max val acc
    in
    case initValue of
        Just val ->
            Array.foldl foldFunction val (Array.slice 1 (Array.length floatArray) floatArray)

        Nothing ->
            0 / 0



-- or another default value indicating an empty array
-- Generalized arMin function


arMin : AggrUnaryFuncFloat
arMin floatArray =
    let
        initValue =
            Array.get 0 floatArray

        foldFunction =
            \val acc -> min val acc
    in
    case initValue of
        Just val ->
            Array.foldl foldFunction val (Array.slice 1 (Array.length floatArray) floatArray)

        Nothing ->
            0 / 0



-- or another default value indicating an empty array


arVariance : AggrUnaryFuncFloat
arVariance floatArray =
    let
        n =
            Array.length floatArray |> toFloat

        mean =
            arSum floatArray / n

        sumSquares =
            Array.foldl (\val acc -> acc + (val - mean) ^ 2) 0 floatArray
    in
    sumSquares / n


arStdDev : AggrUnaryFuncFloat
arStdDev floatArray =
    let
        variance =
            arVariance floatArray
    in
    sqrt variance


arVarianceSample : AggrUnaryFuncFloat
arVarianceSample floatArray =
    let
        n =
            Array.length floatArray |> toFloat

        mean =
            arSum floatArray / n

        sumSquares =
            Array.foldl (\val acc -> acc + (val - mean) ^ 2) 0 floatArray
    in
    sumSquares / (n - 1)


arStdDevSample : AggrUnaryFuncFloat
arStdDevSample floatArray =
    let
        variance =
            arVarianceSample floatArray
    in
    sqrt variance


arPairSumProduct : AggrBinaryFuncFloat
arPairSumProduct floatArray1 floatArray2 =
    let
        floatArray =
            Array.Extra.map2 (*) floatArray1 floatArray2
    in
    Array.foldl (+) 0 floatArray


arPairCovariance : AggrBinaryFuncFloat
arPairCovariance floatArray1 floatArray2 =
    let
        n =
            Array.length floatArray1 |> toFloat

        mean1 =
            arSum floatArray1 / n

        mean2 =
            arSum floatArray2 / n

        sumProducts =
            Array.Extra.map2 (\val1 val2 -> (val1 - mean1) * (val2 - mean2)) floatArray1 floatArray2
    in
    Array.foldl (+) 0 sumProducts / n


arPairCorrelation : AggrBinaryFuncFloat
arPairCorrelation floatArray1 floatArray2 =
    let
        covariance =
            arPairCovariance floatArray1 floatArray2

        stdDev1 =
            arStdDev floatArray1

        stdDev2 =
            arStdDev floatArray2
    in
    covariance / (stdDev1 * stdDev2)


arPairCovarianceSample : AggrBinaryFuncFloat
arPairCovarianceSample floatArray1 floatArray2 =
    let
        n =
            Array.length floatArray1 |> toFloat

        mean1 =
            arSum floatArray1 / n

        mean2 =
            arSum floatArray2 / n

        sumProducts =
            Array.Extra.map2 (\val1 val2 -> (val1 - mean1) * (val2 - mean2)) floatArray1 floatArray2
    in
    Array.foldl (+) 0 sumProducts / (n - 1)


arPairCorrelationSample : AggrBinaryFuncFloat
arPairCorrelationSample floatArray1 floatArray2 =
    let
        covariance =
            arPairCovarianceSample floatArray1 floatArray2

        stdDev1 =
            arStdDevSample floatArray1

        stdDev2 =
            arStdDevSample floatArray2
    in
    covariance / (stdDev1 * stdDev2)


joinStringPair : String -> BinaryFuncString
joinStringPair separator str1 str2 =
    str1 ++ separator ++ str2


joinStringArray : String -> AggrFuncString
joinStringArray separator strArray =
    Array.foldl (\new acc -> acc ++ separator ++ new) "" strArray


taxMultiplier : UnaryFuncFloat
taxMultiplier val =
    val * 0.2


myId : UnaryFuncFloat
myId val =
    val


id : UnaryFuncFloat
id =
    identity


keepNotNaN : BinaryFuncFloat
keepNotNaN val1 val2 =
    if isNaN val1 then
        val2

    else
        val1



-- used to update with formula calculated values
-- provisional, to be replaced with calc status for each datum in whole data array
-- if the valPart is NaN returns the valWhole, otherwise the valPart that is the calculated value
-- so if not NaN the valPart overwrites the valWhole


refreshWithValPart : BinaryFuncFloat
refreshWithValPart valPart valWhole =
    if isNaN valPart then
        valWhole

    else
        valPart



-- Helper functions for array functions
-- given coordVecDest in the destination DataArray, returns the posVec in the source DataArray
-- used to get data in a formula from another DataArray with different dims (ex fron scenario to ce)


mapPosVec : Dims -> List DimRef -> List DimRef -> CoordVec -> Maybe PosVec
mapPosVec dimsSrc dimRefsSrc dimRefsDest coordVecDest =
    -- loop on dimRefsSrc
    List.foldl
        (\dimRef posVec ->
            let
                -- gett coords for the dimRef
                coordsSrc =
                    Dict.get dimRef dimsSrc |> Maybe.withDefault Array.empty

                -- create a dict of coordIndices by coords for the dimRef
                coordDictSrc =
                    coordDictFast coordsSrc
            in
            -- gets pos of current dimRef in dimRefsDest
            case List.Extra.elemIndex dimRef dimRefsDest of
                -- if the dimRef is in dimRefsDest tries to match the coord in coordVecDest
                Just destIndex ->
                    let
                        -- get the coord for the destIndex in coordVecDest passed to the function
                        maybeCoord =
                            List.Extra.getAt destIndex coordVecDest

                        srcIndex =
                            case maybeCoord of
                                -- if the coord is in the coordsSrc
                                Just coord ->
                                    -- gets the coordIndex for the coord in dimsSrc
                                    -- fi not found the dict is corrupted and returns -1
                                    Maybe.withDefault -1 (Dict.get coord coordDictSrc)

                                -- if the coord is not in the coordsSrc
                                Nothing ->
                                    -1
                    in
                    Maybe.map (\vec -> vec ++ [ srcIndex ]) posVec

                -- if the dimRef is only in srcDims, adds the 1st coordIndex in dimsSrc
                Nothing ->
                    Maybe.map (\vec -> vec ++ [ 0 ]) posVec
        )
        (Just [])
        dimRefsSrc



-- given an arDest consuming data in a formula from arSrc,
-- returns an array of posVecs in the src DataArray parallel to the posVecs in the dest DataArray
-- and the dims and dimRefs of the src DataArray


mapPosVecs : DataArray -> DataArray -> Maybe ( Array PosVec, Dims, List DimRef )
mapPosVecs arSrc arDest =
    case ( arSrc.localDims, arSrc.localDimRefs ) of
        ( Just dimsSrc, Just dimRefsSrc ) ->
            case ( arDest.localDims, arDest.localDimRefs ) of
                ( Just dimsDest, Just dimRefsDest ) ->
                    let
                        coordVecs = --myLog "coordVecs"
                            (itemCoordVecsForDims dimsDest dimRefsDest)

                        posVecs =
                            Array.map
                                (\coordVecDest ->
                                    case mapPosVec dimsSrc dimRefsSrc dimRefsDest coordVecDest of
                                        Just posVec ->
                                            posVec

                                        Nothing ->
                                            myLog "Invalid coordVecDest in mapPosVec"
                                            []
                                )
                                coordVecs
                    in
                    Just ( posVecs, dimsSrc, dimRefsSrc )

                _ ->
                    myLog "Invalid arDest dims in mapPosVecs" 
                    Nothing

        _ ->
            myLog "Invalid arSrc dims in mapPosVecs" 
            Nothing



-- returns an array of flatIndices in the src DataArray parallel to the 0..N flatIndices in the dest DataArray
-- used to get data from the src DataArray and consume them in the dest DataArray


mapFlatIndices : DataArray -> DataArray -> Array FlatIndex
mapFlatIndices arSrc arDest =
    -- arDest is the DataArray consuming data from arSrc
    let
        maybePosVecs =
            mapPosVecs arSrc arDest
    in
    case maybePosVecs of
        Just ( posVecs, dims, dimRefs ) ->
            calcFlatIndicesFast dims dimRefs posVecs

        Nothing ->
            let
                _ =
                    myLog "Invalid arSrc or arDest in mapFlatIndices" maybePosVecs
            in
            Array.empty



-- ==================== DataArray functions used by the interpreter ====================
-- unary calculation does not need dims, only the data, this func is a wrapper for the map


funcDataArrayUnary :
    Result String DataArray
    -> UnaryFuncFloat
    -> Result String DataArray
funcDataArrayUnary arSrc unaryFunc =
    case arSrc of
        Ok arJust ->
            let
                calcData =
                    Array.map unaryFunc arJust.data
            in
            Ok { arJust | data = calcData }

        Err err ->
            Err (err ++ " catched in funcDataArrayUnary")



-- test function used also in FuncDataArray but in interpreter not working


descrValue : Value -> String
descrValue value =
    case value of
        Record _ ->
            "Record"

        String _ ->
            "Text"

        Float _ ->
            "Number"

        _ ->
            "Other"



-- returns a new DataArray aggregating the data of the source DataArray by the given aggrDimRefs using the aggrFunc
-- aggrDimRefs must be a subset of the source DataArray dims, they are eliminated in the returned DataArray


aggrDataArrayUnary :
    XModel
    -> Result String DataArray
    -> AggrUnaryFuncFloat
    -> List DimRef
    -> DataArrayRef
    -> Result String DataArray
aggrDataArrayUnary xModel arSrc aggrFunc aggrDimRefs dataArrayRef =
    case arSrc of
        Ok arJust ->
            let
                ( srcDimRefs, srcDims ) =
                    dimInfoForDataArray xModel arJust
            in
            case ( srcDims, srcDimRefs ) of
                ( Just srcDimsJust, Just srcDimRefsJust ) ->
                    if List.all (\aggrDimRef -> List.member aggrDimRef srcDimRefsJust) aggrDimRefs then
                        let
                            keptDimRefs =
                                List.filter (\dimRef -> not (List.member dimRef aggrDimRefs)) srcDimRefsJust

                            keptDims =
                                Dict.filter (\key _ -> List.member key keptDimRefs) srcDimsJust

                            dataset =
                                datasetForDataArray xModel arJust |> Maybe.withDefault emptyDataset

                            aggrData =
                                Array.map
                                    (\coordVec ->
                                        let
                                            coordSpecifiers =
                                                List.map2 (\dimRef coord -> toSingleCoord dimRef coord) keptDimRefs coordVec

                                            coordVecDAr =
                                                loc srcDimsJust dataset arJust coordSpecifiers |> Maybe.withDefault emptyDataArray

                                            floatArray =
                                                coordVecDAr.data
                                        in
                                        aggrFunc floatArray
                                    )
                                    (itemCoordVecsForDims keptDims keptDimRefs)
                        in
                        Ok { arJust | ref = dataArrayRef, datasetRef = Just dataset.ref, data = aggrData, localDims = Just keptDims, localDimRefs = Just keptDimRefs }

                    else
                        Err "aggrDimRefs not in srcDimRefs"

                _ ->
                    Err "Error in aggrDataArrayUnary"

        Err err ->
            Err (err ++ " catched in funcDataArrayUnary")



-- used in elm-interpreter


aggrDataArrayUnaryAr :
    Result String DataArray
    -> AggrUnaryFuncFloat
    -> List DimRef
    -> DataArrayRef
    -> Result String DataArray
aggrDataArrayUnaryAr arSrc aggrFunc aggrDimRefs dataArrayRef =
    case arSrc of
        Ok arJust ->
            let
                ( srcDimRefs, srcDims ) =
                    ( arJust.localDimRefs, arJust.localDims )
            in
            case ( srcDims, srcDimRefs ) of
                ( Just srcDimsJust, Just srcDimRefsJust ) ->
                    if List.all (\aggrDimRef -> List.member aggrDimRef srcDimRefsJust) aggrDimRefs then
                        let
                            keptDimRefs =
                                List.filter (\dimRef -> not (List.member dimRef aggrDimRefs)) srcDimRefsJust

                            keptDims =
                                Dict.filter (\key _ -> List.member key keptDimRefs) srcDimsJust

                            aggrData =
                                Array.map
                                    (\coordVec ->
                                        let
                                            coordSpecifiers =
                                                List.map2 (\dimRef coord -> toSingleCoordAr dimRef coord) keptDimRefs coordVec

                                            coordVecDAr =
                                                locAr arJust coordSpecifiers |> Maybe.withDefault emptyDataArray

                                            floatArray =
                                                coordVecDAr.data
                                        in
                                        aggrFunc floatArray
                                    )
                                    (itemCoordVecsForDims keptDims keptDimRefs)
                        in
                        Ok { arJust | ref = dataArrayRef, data = aggrData, localDims = Just keptDims, localDimRefs = Just keptDimRefs }

                    else
                        Err "aggrDimRefs not in srcDimRefs"

                _ ->
                    Err "Error in aggrDataArrayUnary"

        Err err ->
            Err (err ++ " catched in funcDataArrayUnary")



-- applies a binary function to the two DataArrays data, assuming DataArrays are passed with localDims
-- if dims are different, matches the data by coords onto arDest .data
-- returns tuple (maybe DataArray, maybe pivotDimRef)


funcDataArrayPair : 
    XModel 
    -> DataArray 
    -> DataArray 
    -> BinaryFuncFloat 
    -> ( Maybe DataArray, Maybe DimRef )
funcDataArrayPair xModel arSrc arDest binaryFunc =
    let
        ( srcDimRefs, srcDims ) =
            dimInfoForDataArray xModel arSrc

        ( destDimRefs, destDims ) =
            dimInfoForDataArray xModel arDest
    in
    case ( srcDims, destDims ) of
        -- extend checks to localDimRefs
        ( Just dimsSrc, Just dimsDest ) ->
            case ( srcDimRefs, destDimRefs ) of
                ( Just dimRefsSrc, Just dimRefsDest ) ->
                    let
                        pivotRef =
                            pivotDimRefForDims dimsSrc dimsDest

                        arSrcdata =
                            -- brute force map calc on the whole data arrays with the same localDims
                            -- with single coord array mapping does not work
                            if
                                dimsSrc == dimsDest
                                    && dimRefsSrc == dimRefsDest
                                    && Array.length arSrc.data == Array.length arDest.data
                            then
                                -- if (Array.length arSrc.data) == (Array.length arDest.data) then
                                arSrc.data
                                -- if same dims except one with one coord, data are parallel and can be passed as is

                            else if pivotRef /= Nothing then
                                arSrc.data

                            else
                                -- map the data of the source DataArray to the destination DataArray by dim/coords
                                -- TODO to be re-checked
                                let
                                    arSrcWithDims =
                                        { arSrc | localDims = Just dimsSrc, localDimRefs = Just dimRefsSrc }

                                    arDestWithDims =
                                        { arDest | localDims = Just dimsDest, localDimRefs = Just dimRefsDest }

                                    mappedIndices =
                                        mapFlatIndices arSrcWithDims arDestWithDims

                                    -- mapping needs localDims
                                in
                                --Debug.log ("arSrc.localDims: " ++ Debug.toString(srcDims))
                                --Debug.log ("mappedIndices: " ++ Debug.toString(mappedIndices))
                                Array.map (\flIdx -> Array.get flIdx arSrc.data |> Maybe.withDefault (0 / 0)) mappedIndices

                        calcData =
                            Array.Extra.map2 binaryFunc arDest.data arSrcdata
                    in
                    -- Debug.log ("arSrcdata: " ++ Debug.toString(arSrcdata))
                    -- Debug.log ("arDest.data: " ++ Debug.toString(arDest.data))
                    -- Debug.log ("calcData: " ++ Debug.toString(calcData))
                    ( Just { arDest | data = calcData }, pivotRef )

                -- keeps dims and dimRefs, but changes data as calculated
                _ ->
                    ( Nothing, Nothing )

        _ ->
            ( Nothing, Nothing )



-- removed xModel from the signature, differemt from the homonime used in interpreter


funcDataArrayPairAr : 
    DataArray 
    -> DataArray 
    -> BinaryFuncFloat 
    -> ( Maybe DataArray, Maybe DimRef )
funcDataArrayPairAr arSrc arDest binaryFunc =
    let
        ( srcDimRefs, srcDims ) =
            ( arSrc.localDimRefs, arSrc.localDims )

        ( destDimRefs, destDims ) =
            ( arDest.localDimRefs, arDest.localDims )
    in
    case ( srcDims, destDims ) of
        -- extend checks to localDimRefs
        ( Just dimsSrc, Just dimsDest ) ->
            case ( srcDimRefs, destDimRefs ) of
                ( Just dimRefsSrc, Just dimRefsDest ) ->
                    let
                        pivotRef =
                            pivotDimRefForDims dimsSrc dimsDest

                        arSrcdata =
                            -- brute force map calc on the whole data arrays with the same localDims
                            -- with single coord array mapping does not work
                            if
                                dimsSrc
                                    == dimsDest
                                    && dimRefsSrc
                                    == dimRefsDest
                                    && Array.length arSrc.data
                                    == Array.length arDest.data
                            then
                                -- if (Array.length arSrc.data) == (Array.length arDest.data) then
                                arSrc.data
                                -- if same dims except one with one coord, data are parallel and can be passed as is

                            else if pivotRef /= Nothing then
                                arSrc.data

                            else
                                -- map the data of the source DataArray to the destination DataArray by dim/coords
                                -- TODO to be re-checked
                                let
                                    arSrcWithDims =
                                        { arSrc | localDims = Just dimsSrc, localDimRefs = Just dimRefsSrc }

                                    arDestWithDims =
                                        { arDest | localDims = Just dimsDest, localDimRefs = Just dimRefsDest }

                                    mappedIndices =
                                        mapFlatIndices arSrcWithDims arDestWithDims

                                    -- mapping needs localDims
                                in
                                --Debug.log ("arSrc.localDims: " ++ Debug.toString(srcDims))
                                --Debug.log ("mappedIndices: " ++ Debug.toString(mappedIndices))
                                Array.map (\flIdx -> Array.get flIdx arSrc.data |> Maybe.withDefault (0 / 0)) mappedIndices

                        calcData =
                            Array.Extra.map2 binaryFunc arSrcdata arDest.data
                    in
                    ( Just { arDest | data = calcData }, pivotRef )

                -- keeps dims and dimRefs, but changes data as calculated
                _ ->
                    ( Nothing, Nothing )

        _ ->
            ( Nothing, Nothing )



-- remaps the data array data to the newDims, dimRefs unchanged
-- used with structure changes
remapDataInDataArrayToNewDims :
    XModel
    -> Result String DataArray
    -> (Maybe (List DimRef) , Dims ) 
    -> Result String DataArray
remapDataInDataArrayToNewDims xModel arPre (newDimRefs , newDims) =
    case arPre of
        Ok arPreOk  ->
            let

                ( preDimRefs, preDims ) =
                    dimInfoForDataArray xModel arPreOk
                postDimRefs = 
                    case newDimRefs of
                        Just refs -> newDimRefs
                        Nothing -> preDimRefs
            in
            case preDims  of
                -- extend checks to localDimRefs
                Just dimsPre  ->
                    if newDims == dimsPre  then
                        Ok arPreOk
                    else
                    let
                        arPostOkWithDims =
                            { arPreOk | localDims = Just newDims, localDimRefs =  postDimRefs }


                        arPreOkWithDims =
                            { arPreOk | localDims = Just dimsPre, localDimRefs = preDimRefs }

                        mappedIndices = -- positions in arPreOk.data for each position in arPostOk.data
                            --myLog "mappedIndices" 
                                (mapFlatIndices arPreOkWithDims arPostOkWithDims )  
                        _ = logIf False -- (mappedIndices == Array.empty) 
                                "scenario dims" 
                                --(arPreOkWithDims.localDimRefs , arPostOkWithDims.localDimRefs)
                                (Dict.get "scenario" dimsPre , Dict.get "scenario" newDims)


                        arNewData =
                                if isDataArrayText arPreOk then
                                    Array.empty
                                else 
                                    -- get indices in whole DataArray for each coord in part DataArray and calc the data
                                    let
                                        extractedNewData =
                                            Array.map
                                                (\preFlIdx  ->
                                                    Array.get preFlIdx arPreOk.data |> Maybe.withDefault (0 / 0)
                                                    )
                                                mappedIndices
                                    in
                                    extractedNewData
                        
                        arNewText =
                                if isDataArrayText arPreOk then
                                    -- get indices in whole DataArray for each coord in part DataArray and calc the data
                                    Array.map
                                        (\preFlIdx  ->
                                            Array.get preFlIdx arPreOk.text |> Maybe.withDefault ""
                                            )
                                        mappedIndices

                                else
                                    Array.empty

                    in
                    -- removed localDims cannot be there when remapping assuming use of xModel.dims
                    Ok { arPreOk | data = arNewData , text = arNewText} --, localDims = Just newDims, localDimRefs =  preDimRefs}

                -- keeps dims and dimRefs, but changes data as calculated
                _ ->
                    Err "Error in remapDataInDataArrayToNewDims"

        _ ->
            -- Debug.log ("arWhole: " ++ Debug.toString(arWhole))
            -- Debug.log ("\narPart: " ++ Debug.toString(arPart))
            Err "Error in remapDataInDataArrayToNewDims >= WRONG Ok arPreOk"

-- equivalent to funcDataArrayPair wrapping DataArray in Result
updateDataArrayPair :
    XModel
    -> Result String DataArray
    -> Result String DataArray
    -> BinaryFuncFloat
    -> Result String DataArray
updateDataArrayPair xModel arWhole arPart binaryFunc =
    case ( arWhole, arPart ) of
        ( Ok arWholeOk, Ok arPartOk ) ->
            let
                ( wholeDimRefs, wholeDims ) =
                    dimInfoForDataArray xModel arWholeOk

                ( partDimRefs, partDims ) =
                    dimInfoForDataArray xModel arPartOk
            in
            case ( wholeDims, partDims ) of
                -- extend checks to localDimRefs
                ( Just dimsWhole, Just dimsPart ) ->
                    let
                        arWholedataTuples =
                            -- brute force map calc on the whole data arrays with the same localDims
                            if dimsWhole == dimsPart && Array.length arWholeOk.data == Array.length arPartOk.data then
                                let
                                    indexArray =
                                        Array.fromList (List.range 0 (Array.length arWholeOk.data))

                                    dataTuples =
                                        Array.Extra.zip arWholeOk.data arPartOk.data
                                in
                                Array.Extra.zip indexArray dataTuples

                            else
                                -- get indices in whole DataArray for each coord in part DataArray and calc the data
                                let
                                    arWholeOkWithDims =
                                        { arWholeOk | localDims = Just dimsWhole, localDimRefs = wholeDimRefs }

                                    arPartOkWithDims =
                                        { arPartOk | localDims = Just dimsPart, localDimRefs = partDimRefs }

                                    mappedIndices =
                                        mapFlatIndices arWholeOkWithDims arPartOkWithDims

                                    extractedWholeDataTuples =
                                        Array.Extra.map2
                                            (\wholeFlIdx partDatum ->
                                                ( wholeFlIdx
                                                , ( Array.get wholeFlIdx arWholeOk.data |> Maybe.withDefault (0 / 0)
                                                  , partDatum
                                                  )
                                                )
                                            )
                                            mappedIndices
                                            arPartOk.data
                                in
                                extractedWholeDataTuples

                        --calcDataOnPart = Array.Extra.map2 binaryFunc arPart.data arWholedata
                        updatatedWholeData =
                            Array.foldl
                                (\( flIdx, ( valWhole, valPart ) ) dataWhole ->
                                    let
                                        valCalc =
                                            binaryFunc valPart valWhole

                                        -- see binaryFunc's for the order of the values
                                    in
                                    Array.set flIdx valCalc dataWhole
                                )
                                arWholeOk.data
                                arWholedataTuples
                    in
                    -- Debug.log ("\n\nupdatatedWholeData: " ++ Debug.toString(updatatedWholeData))
                    -- Debug.log ("\narPart: " ++ Debug.toString(arPart))
                    Ok { arWholeOk | data = updatatedWholeData }

                -- keeps dims and dimRefs, but changes data as calculated
                _ ->
                    Err "Error in updateDataArrayPair"

        ( _, _ ) ->
            -- Debug.log ("arWhole: " ++ Debug.toString(arWhole))
            -- Debug.log ("\narPart: " ++ Debug.toString(arPart))
            Err "Error in updateDataArrayPair >= WRONG (Ok arWholeOk, Ok arPartOk)"



-- ============== RANGE FUNCTIONS ==================


getDefaultDataArrayRef : Dataset -> DataArrayRef
getDefaultDataArrayRef dataset =
    case dataset.defaultDataArrayRef of
        Just ref ->
            ref

        Nothing ->
            -- if no default, return the first one
            case dataset.dataArrayRefs of
                [] ->
                    ""

                ref :: _ ->
                    ref



-- used to check duplicated coord names in a Dims specification


type alias CoordSearchDict =
    Dict Coord (List DimRef)


makeCoordSearchDict : XModel -> DatasetRef -> List DimRef -> Bool -> CoordSearchDict
makeCoordSearchDict xModel datasetRef excludedDimRefs excludeDVars =
    let
        dataset =
            getDatasetByRef datasetRef xModel.datasets
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
                List.foldl
                    -- insert dVars and dVarIdentifier
                    (\dVarRef dVarAcc ->
                        Dict.insert dVarRef [ dVarIdentifier ] dVarAcc
                    )
                    Dict.empty
                    dataset.dataArrayRefs
    in
    Dict.foldl
        -- add coords and list of dimRefs
        (\dimRef coordArray outerAcc ->
            Array.foldl
                (\coord innerAcc ->
                    case Dict.get coord innerAcc of
                        Just dimRefs ->
                            Dict.insert coord (dimRef :: dimRefs) innerAcc

                        Nothing ->
                            Dict.insert coord [ dimRef ] innerAcc
                )
                outerAcc
                coordArray
        )
        dVarDict
        dims



-- conversion from RangeDef to a name used in formulas
-- NB check of duplication is relevant only for the same dataset, not for the whole model
-- so be careful not to pass the whole dims of the model, better the calculated localDims of a DataArray


rangeDefToName : XModel -> DatasetRef -> RangeDef -> String
rangeDefToName xModel curDatasetRef rangeDef =
    let
        curDataset =
            getDatasetByRef curDatasetRef xModel.datasets |> Maybe.withDefault emptyDataset

        dims =
            dimsForDataset xModel.dims curDataset

        searchDict =
            makeCoordSearchDict xModel curDatasetRef [] False

        getUniqueCoordDef : DimRef -> Coord -> Maybe String
        getUniqueCoordDef dimRef coord =
            case Dict.get coord searchDict of
                Just dimRefs ->
                    case dimRefs of
                        [ oneDimRef ] ->
                            Just coord

                        [] ->
                            Nothing

                        -- coord present without matching dim
                        _ ->
                            Just (dimRef ++ "DOT" ++ coord)

                Nothing ->
                    Nothing

        -- coord not present in searchDict
        coordSpecs : List ( DimRef, CoordSpecifier ) -> String
        coordSpecs dimCoords =
            List.foldl
                (\( dimRef, coordSpec ) accStr ->
                    case coordSpecifierToString coordSpec of
                        Just coordSpecJust ->
                            case getUniqueCoordDef dimRef coordSpecJust of
                                Just uniqueCoord ->
                                    accStr ++ "_" ++ uniqueCoord

                                Nothing ->
                                    accStr ++ "_ERR" ++ dimRef

                        Nothing ->
                            accStr
                )
                ""
                dimCoords

        defArrayRef =
            getDefaultDataArrayRef curDataset
    in
    case ( rangeDef.datasetRef, rangeDef.dataArrayRef, rangeDef.dimCoords ) of
        ( Nothing, Nothing, Nothing ) ->
            "undef__undef"

        ( Just ds, Nothing, Nothing ) ->
            FormulaParser.lowerFirst ds

        ( Nothing, Just da, Nothing ) ->
            FormulaParser.lowerFirst curDatasetRef ++ "__" ++ da

        ( Just ds, Just da, Nothing ) ->
            FormulaParser.lowerFirst ds ++ "__" ++ da

        ( Nothing, Nothing, Just dc ) ->
            FormulaParser.lowerFirst curDatasetRef ++ "__" ++ defArrayRef ++ coordSpecs dc

        ( Just ds, Nothing, Just dc ) ->
            FormulaParser.lowerFirst ds ++ "__" ++ defArrayRef ++ coordSpecs dc

        ( Nothing, Just da, Just dc ) ->
            FormulaParser.lowerFirst curDatasetRef ++ "__" ++ da ++ coordSpecs dc

        ( Just ds, Just da, Just dc ) ->
            FormulaParser.lowerFirst ds ++ "__" ++ da ++ coordSpecs dc



-- conversion from a name used in formulas to RangeDef
-- generalize treatment of trailing "_" to use both for prompts and for parsing in calculation


rangeNameToDef : XModel -> DatasetRef -> String -> Result String RangeDef
rangeNameToDef xModel curDatasetRef rangeName =
    -- rangeName without trailing "_"
    let
        trySplitDataset =
            String.split "__" rangeName

        firstTokenOrEmpty =
            List.head trySplitDataset |> Maybe.withDefault ""

        splitRest lst =
            -- list of strings separated by "_" in the head of lst
            case List.head lst of
                Just head ->
                    String.split "_" head

                Nothing ->
                    []

        searchDictDs =
            makeCoordSearchDict xModel curDatasetRef [] False

        isOnlyDatasetNotCoord =
            List.length trySplitDataset
                == 1
                -- only one segment
                && List.length (splitRest trySplitDataset)
                == 1
                -- no tokens after or in absence of "__ "
                && firstTokenOrEmpty
                /= ""
                -- there is a token before or in absence of "__"
                && Dict.get firstTokenOrEmpty searchDictDs
                == Nothing

        -- that token is not a coord or dVar
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
                    Ok ""
                -- no input dataset

            else
                Ok ""

        -- no input dataset
        ( ds, rest, errMsgDs ) =
            case inputDs of
                Ok "" ->
                    ( curDatasetRef
                    , trySplitDataset |> splitRest
                    , ""
                    )

                Ok inputDsStr ->
                    ( inputDsStr
                    , trySplitDataset |> List.drop 1 |> splitRest
                    , ""
                    )

                Err errMsgStr ->
                    ( "", [], errMsgStr )

        -- makes a dict of coords for the dataset in the range or passed as arg
        curDataset =
            case getDatasetByRef ds xModel.datasets of
                Just datasetJust ->
                    datasetJust

                Nothing ->
                    emptyDataset

        defArrayRef =
            getDefaultDataArrayRef curDataset

        searchDict =
            makeCoordSearchDict xModel ds [] False

        ( inputDa, coordSpecsInName, errMsg ) =
            -- scans the rest of the split for coords and dVar
            List.foldl
                (\token ( daAcc, coordAcc, errAcc ) ->
                    let
                        maybeCTuple =
                            getCoordSpec token
                    in
                    case maybeCTuple of
                        Just ( dimRef, CoordSingle coord ) ->
                            if dimRef == dVarIdentifier then
                                ( coord, coordAcc, errAcc )
                                -- dVar found signaled by dVarIdentifier

                            else
                                ( daAcc, coordAcc ++ [ ( dimRef, CoordSingle coord ) ], errAcc )

                        -- dimRef and coord found
                        _ ->
                            ( daAcc
                            , coordAcc
                            , errAcc ++ "token " ++ token ++ " undefined"
                            )
                 -- not a coord or dVar
                )
                ( "", [], errMsgDs )
                rest

        ( da, isExplDaRef ) =
            if inputDa == "" then
                ( defArrayRef, False )

            else
                ( inputDa, True )

        getCoordSpec : String -> Maybe ( DimRef, CoordSpecifier )
        getCoordSpec coordSpec =
            let
                splitCoordSpec =
                    String.split "DOT" coordSpec
            in
            case splitCoordSpec of
                [ coord ] ->
                    -- only coord passed in the name is checked against dims
                    case Dict.get coord searchDict of
                        Just dimRefs ->
                            case dimRefs of
                                [ dimRef ] ->
                                    Just ( dimRef, CoordSingle coord )

                                -- only one dimRef, ok
                                _ ->
                                    Nothing

                        -- more than one dimRef, error ambiguous definition
                        Nothing ->
                            Nothing

                -- error, coord not found in dims
                [ dimRef, coord ] ->
                    -- dimRef and coord passed in the name are checked against dims
                    case Dict.get coord searchDict of
                        Nothing ->
                            Nothing

                        Just foundDimRefsForCoord ->
                            if List.member dimRef foundDimRefsForCoord then
                                Just ( dimRef, CoordSingle coord )

                            else
                                Nothing

                _ ->
                    Nothing
    in
    if errMsg /= "" then
        Err errMsg

    else if ds == "" then
        Err "Dataset not found"

    else if da == "" then
        Err "DataArray not found"

    else
        Ok
            { datasetRef = Just ds
            , dataArrayRef = Just da
            , dimCoords = Just coordSpecsInName
            , isExplicitDataArrayRef = isExplDaRef
            }



-- used in codemirror to parse the range name being entered and propose the next possible tokens


promptCoordsForPartialRangeName : XModel -> DatasetRef -> String -> List { label : String }
promptCoordsForPartialRangeName xModel curDatasetRef partialName =
    let
        --  text after the last "_"
        getSubstrAndLastToken : String -> ( String, String )
        getSubstrAndLastToken partialNameArg =
            let
                -- Find the last token by splitting and reversing the string
                tokens =
                    String.split "_" partialNameArg

                lastTokenRet =
                    List.reverse tokens |> List.head |> Maybe.withDefault ""

                lastTokenLength =
                    String.length lastTokenRet

                -- Calculate the position to split the string
                prependedLength =
                    String.length partialNameArg - lastTokenLength

                -- Get the substring before the last token
                beforeLastToken =
                    if prependedLength > 0 then
                        String.left prependedLength partialNameArg

                    else
                        ""
            in
            ( beforeLastToken, lastTokenRet )

        ( prependedRangeName, lastToken ) =
            getSubstrAndLastToken partialName

        prependedRangeNameClean =
            AppUtil.removeTrailingUnderscores prependedRangeName

        partialRangeDef =
            rangeNameToDef xModel curDatasetRef prependedRangeNameClean

        -- updates the datasetRef as specified in the partialRangeDef
        ( dsRef, isExplDaRef ) =
            case partialRangeDef of
                Ok rangeDef ->
                    case rangeDef.datasetRef of
                        Just dsRefJust ->
                            ( dsRefJust, rangeDef.isExplicitDataArrayRef )

                        Nothing ->
                            ( "", False )

                --curDatasetRef -- if no datasetRef in the partialName, keep the current TODO check if this is correct
                Err _ ->
                    ( "", False )

        --curDatasetRef
        ds =
            case getDatasetByRef dsRef xModel.datasets of
                Just datasetJust ->
                    datasetJust

                Nothing ->
                    emptyDataset

        dims =
            dimsForDataset xModel.dims ds

        dimRefsInPartialName =
            case partialRangeDef of
                Ok rangeDef ->
                    case rangeDef.dimCoords of
                        Just dimCoords ->
                            List.map Tuple.first dimCoords

                        Nothing ->
                            []

                Err _ ->
                    []

        searchDict =
            makeCoordSearchDict xModel dsRef dimRefsInPartialName isExplDaRef

        -- filter for lastToken and fold available coords, if lastToken="*" fold all
        availableCoords : CoordSearchDict -> String -> List String
        availableCoords searchDictArg lastTokenArg =
            let
                -- Flatten the dict into a list of (compositeKey, originalCoord)
                flattenedList =
                    Dict.foldl
                        (\coord dimRefs acc ->
                            let
                                compositeKeys =
                                    -- makes a list of (compositeKey => to sort on, originalCoord)
                                    if String.startsWith lastTokenArg coord || lastTokenArg == "*" then
                                        List.map
                                            (\dimRef ->
                                                if List.length dimRefs > 1 then
                                                    ( dimRef ++ "." ++ coord, dimRef ++ "DOT" ++ coord )

                                                else
                                                    ( dimRef ++ "." ++ coord, coord )
                                            )
                                            dimRefs

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
                        |> List.sortWith (\( a, _ ) ( b, _ ) -> compare a b)

                -- Extract the original coords from the sorted list
                sortedCoords =
                    List.map Tuple.second sortedCompositeKeys
            in
            sortedCoords

        prompts =
            List.map (\r -> { label = prependedRangeName ++ r }) (availableCoords searchDict lastToken)
    in
    prompts



