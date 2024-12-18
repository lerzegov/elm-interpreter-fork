module TypesXModel exposing (..)

-- created to allow import in Types.elm for xModel in Env
-- imported also in XModel.elm
-- FastDict is a custom Dict implementation with faster updates by miniBill

import Array exposing (Array)
import FastDict as Dict exposing (Dict)
import List exposing (range)
import List.Extra as List
import Array.Extra




-- moved here to avoid circular dependencies
type Tray
    = RowTray
    | ColumnTray
    | PageTray

-- name of a cetegirical dimension
type alias DimRef =
    String



-- name of a data variable stored in a DataArray
type alias DVarRef =
    String



-- name of a coordinate in any dimension
type alias Coord =
    String



-- used in DatasetView to specify Trays
-- includes "categorical" DimRefs plus DVarDimRef to handle the DataArrays in the Dataset as coords
-- should not affect iloc and loc on Dataset
type DimOrDAr
    = CategDimRef DimRef
    -- container of DataArrayRefs used as pseudo-coords assuming dVarIdentifier as DimRef
    | DVarDimRef DVarRef -- set in current app to DVarIdentifier 



-- used to avoid type mismatch and no comparable in Dict using DimVariant
type alias DimOrDArRef =
    String


dimVariantToDimRefString : DimOrDAr -> DimRef
dimVariantToDimRefString dimVariantRef =
    case dimVariantRef of
        CategDimRef dimRef ->
            dimRef

        DVarDimRef dVarRef ->
            dVarRef



-- hard coded name of the dVarDim in a dataset
-- has dataArrayRefs as coords


dVarIdentifier : DVarRef
dVarIdentifier =
    "dato"



-- used to filter coordinates in XModel.loc


type CoordSpecifier
    = CoordSingle Coord
    | CoordRange ( Coord, Coord )
    | CoordList (Array Coord)
    | CoordNone


coordSpecifierToString : CoordSpecifier -> Maybe String
coordSpecifierToString coordSpec =
    case coordSpec of
        CoordSingle coord ->
            Just coord

        CoordRange ( start, end ) ->
            Just start

        -- provisional, just return the first coord
        -- CoordRange (start, end) -> ("FROM "++ start ++ "TO" ++ end) |> Just -- alternative
        CoordList coords ->
            Array.get 0 coords

        CoordNone ->
            Nothing



-- name of a Dataset, storing a multidimensional array made of DataArrays

type alias DatasetRef =
    String



-- name of a DataArray, storing a single array of data

type alias DataArrayRef =
    String


-- final store of dimension data for a DataArray or Dataset with shared dimensions

type alias Dims =
    Dict DimRef (Array Coord)


type alias DataArray =
    { ref : DataArrayRef -- name
    , datasetRef : Maybe DatasetRef -- name of the container dataset

    -- to avoid Variant type, we use for data a record with both number and text
    -- text may be used for numbers as well, for example to hold formatted numbers
    -- for string values, text should be the only field used
    , data : Array Float
    , text : Array String
    , localDims : Maybe Dims -- local dims for the DataArray, make it independent of the XModel dims
    , localDimRefs : Maybe (List DimRef) -- local dimRefs for the DataArray, matched to localDims
    , pointedFormulas : Dict FlatIndex String -- formulas pointed by the DataArray, for recalc cycles
    }



-- final store of array data in a Dataset

type alias DataArrays =
    Dict DataArrayRef DataArray



-- source code for a Dataset module formulas


type alias ModuleSource =
    String


type alias Dataset =
    { ref : DatasetRef -- name
    , dimRefs : List DimRef -- ordered list of dimRefs referring to XModel.dims
    , dataArrayRefs : List DataArrayRef -- ordered list of data arrays names (DVars)
    , dataArrays : DataArrays
    , formulas : ModuleSource
    , defaultDataArrayRef : Maybe DataArrayRef -- default value type for the dataset used in formula calc
    }


type alias Datasets =
    Dict DatasetRef Dataset



-- single source of truth for the data


type alias XModel =
    { modelRef : String
    , datasetRefs : List DatasetRef
    , datasets : Datasets
    , dims : Dims -- shared dimensions by datasets in the model
    , datasetsToRecalc : List DatasetRef
    , dimChanges : List DimChange -- temp storage for dim changes used in XModel update functions
    , updatedHeaderPath : Maybe HeaderPath -- temp storage of info for focus after structure update
    , updatedDimOrDArRef : Maybe DimOrDArRef -- temp storage of info about updated 
    }


-- init data
emptyDataArray : DataArray
emptyDataArray =
    { ref = ""
    , datasetRef = Nothing
    , data = Array.empty
    , text = Array.empty
    , localDims = Nothing
    , localDimRefs = Nothing
    , pointedFormulas = Dict.empty
    }

emptyDataset : Dataset
emptyDataset =
    { ref = ""
    , dimRefs = []
    , dataArrayRefs = []
    , dataArrays = Dict.empty
    , formulas = ""
    , defaultDataArrayRef = Nothing
    }


emptyXModel : XModel
emptyXModel =
    { modelRef = ""
    , datasetRefs = []
    , datasets = Dict.empty
    , dims = Dict.empty
    , datasetsToRecalc = []
    , dimChanges = []
    , updatedHeaderPath = Nothing
    , updatedDimOrDArRef = Nothing
    }
-- usable by loc iloc


type alias RangeDef =
    { datasetRef : Maybe String
    , dataArrayRef : Maybe String
    , dimCoords : Maybe (List ( DimRef, CoordSpecifier )) -- no DimVariantRef used in loc, check
    , isExplicitDataArrayRef : Bool
    }



-- custom types and basic helper functions for calcFlatIndex, iloc and loc


type alias Index =
    Int


type alias FlatIndex =
    Int


-- position of a coord in a dimension coord list
type alias CoordIndex =
    Int


type alias Range =
    ( Int, Int )

-- cum size of the coords in previous dimensions
type alias Stride =
    Int


type alias Size =
    Int

-- index coordinates of a data point in a multidimensional array mapped to a flat index
type alias PosVec =
    List CoordIndex

-- string coordinates of a data point
type alias CoordVec =
    List Coord

-- (stride for the dimension, size of the dimension)
type alias Dimension =
    ( Stride, Int )






type alias CoordDict =
    Dict Coord CoordIndex



-- used in XModel.iloc to filter the list of coord indices to include in a DataArray view


type IndexSpecifier
    = IndexSingle CoordIndex -- single coord index
    | IndexRange ( CoordIndex, CoordIndex ) -- contiguous range of coord indices
    | IndexList (Array CoordIndex) -- sparse list of coord indices
    | IndexNone -- no filter, all coord indices


-------------------------------------------------------------------------------
-- HELPER FUNCTIONS SpreadsheetUI structure handling
-- used to specify the cooord in a header cell
type alias HeaderCell =
    ( DimOrDArRef, Coord )

-- used in SpreadsheetUI to specify the path to a header cell
type alias HeaderPath =
    List HeaderCell

-- used in SpreadsheetUI to specify the tree structure of RowHeader and ColHeader
type HeaderTree
    = HeaderNode HeaderCell (List HeaderTree)
    | HeaderLeaf HeaderCell

-- dummy root added to a List HeaderTree to make it into a HeaderTree
rootCell : HeaderCell
rootCell =
    ("root", "root")

rootHeaderTreeFromList : List HeaderTree -> HeaderTree
rootHeaderTreeFromList treeList =
    HeaderNode rootCell treeList

getHeaderTreeWidth : HeaderTree -> Int
getHeaderTreeWidth tree =
    case tree of
        HeaderLeaf _ ->
            -- A single leaf has a width of 1
            1

        HeaderNode _ children ->
            -- The width of a node is the sum of the widths of its children
            List.foldl (\child acc -> acc + getHeaderTreeWidth child) 0 children

getHeaderLeafPathsFromTree : HeaderTree -> List HeaderPath
getHeaderLeafPathsFromTree tree =
    case tree of
        HeaderLeaf cell ->
            [ [ cell ] ]

        HeaderNode cell children ->
            List.concatMap (\child -> List.map (\path -> cell::path ) (getHeaderLeafPathsFromTree child)) children

getHeaderLeafPathsFromTreeList : List HeaderTree -> List HeaderPath
getHeaderLeafPathsFromTreeList treeList =
    List.concatMap getHeaderLeafPathsFromTree treeList

elemIndexOfHeaderLeafPath : HeaderPath -> List HeaderTree -> Maybe Int
elemIndexOfHeaderLeafPath headerPath treeList =
    let
        leafPaths = getHeaderLeafPathsFromTreeList treeList
    in
    List.indexedMap Tuple.pair leafPaths
        |> List.filterMap (\(index, path) -> if path == headerPath then Just index else Nothing)
        |> List.head


getFirstHeaderTreeWidth : List HeaderTree -> Int
getFirstHeaderTreeWidth treeList =
    case treeList of
        first::rest ->
            -- A single leaf has a width of 1
            getHeaderTreeWidth first * List.length treeList

        [] ->
            0

headerPathToString : HeaderPath -> String
headerPathToString headerPath =
    headerPath
        |> List.map (\(dimRef, coord) -> dimRef ++ "|" ++ coord)
        |> String.join "_"

-- NB don't confuse dimVariantRef that can be both a with dimVarRef!

headerPathToCurCell : HeaderPath -> Maybe HeaderCell
headerPathToCurCell headerPath =
    case List.reverse headerPath of
        (lastHeader :: _) ->
            Just lastHeader

        [] ->
            Nothing 

headerPathToCurCoord : HeaderPath -> Maybe Coord
headerPathToCurCoord headerPath =
    case headerPathToCurCell headerPath of
        Just (dimVariantRef, coord) ->
            Just  coord

        Nothing ->
            Nothing

headerPathToCurDimCoord : HeaderPath -> Maybe  (DimOrDArRef , Coord)
headerPathToCurDimCoord headerPath =
    case headerPathToCurCell headerPath of
        Just (dimVariantRef, coord) ->
            Just (dimVariantRef, coord)

        Nothing ->
            Nothing

-- loops on the first nodes of trees in tree list to find the tree with the headerPath root
headerTreeByHeaderPathRoot : HeaderPath -> List HeaderTree -> Maybe HeaderTree
headerTreeByHeaderPathRoot headerPath headerTrees =
    case headerPath of
        [] ->
            Nothing

        headPath :: restPath ->
            List.filter (\tree ->
                case tree of
                    HeaderNode name _ ->
                        headPath == name
                    _ ->
                        False
            )
            headerTrees
            |> List.head

-- returns a HeaderCell from maybeDimVariantRef (first if Nothing) and its first coord
startDimCoordByDimVariantRef : Maybe DimOrDArRef -> Dims -> List DimOrDArRef -> List DataArrayRef -> Maybe HeaderCell
startDimCoordByDimVariantRef maybeDimVariantRef dims dimVariantRefs dataArrayRefs =
    let
        curDimVariantRef = 
            case maybeDimVariantRef of
                Just dVarRef ->
                    Just dVarRef
                Nothing ->
                    List.head dimVariantRefs
        
        startCoord : Maybe String
        startCoord =
            if curDimVariantRef == Just dVarIdentifier then
                List.getAt 0 dataArrayRefs
            else
                    Dict.get (curDimVariantRef |> Maybe.withDefault "") dims
                        |> Maybe.withDefault Array.empty
                        |> Array.get 0
    in
    case (curDimVariantRef , startCoord ) of
        (Just dVRef , Just coord ) ->
            Just (dVRef, coord)
        _ ->
            Nothing
-- same as startDimCoordByDimVariantRef but for the last coord
endDimCoordByDimVariantRef : Maybe DimOrDArRef -> Dims -> List DimOrDArRef -> List DataArrayRef -> Maybe HeaderCell
endDimCoordByDimVariantRef maybeDimVariantRef dims dimVariantRefs dataArrayRefs =
    let
        curDimVariantRef = 
            case maybeDimVariantRef of
                Just dVarRef ->
                    Just dVarRef
                Nothing ->
                    List.head dimVariantRefs
        
        endCoord : Maybe String
        endCoord =
            if curDimVariantRef == Just dVarIdentifier then
                lastList dataArrayRefs
            else
                    Dict.get (curDimVariantRef |> Maybe.withDefault "") dims
                        |> Maybe.withDefault Array.empty
                        |> lastAr
    in
    case (curDimVariantRef , endCoord ) of
        (Just dVRef , Just coord ) ->
            Just (dVRef, coord)
        _ ->
            Nothing
-- builds a path on first coord of each dimVariantRef in the list
pathToStartDimCoordByDimVariantRef : DimOrDArRef -> Dims -> List DimOrDArRef -> List DataArrayRef -> Maybe HeaderPath
pathToStartDimCoordByDimVariantRef dimVariantRef dims dimVariantRefs dataArrayRefs =
    let
        dimVariantRefIndex = elemIndexList dimVariantRef dimVariantRefs 
        dimVariantRefSequence = 
            case dimVariantRefIndex of
                Just index ->
                    List.take (index + 1) dimVariantRefs
                Nothing ->
                    []
        returnedPath =
            case dimVariantRefSequence of
                [] ->
                    Nothing
                pathJust ->
                    List.foldl (\dimVariantRefArg acc -> 
                        acc ++ [startDimCoordByDimVariantRef (Just dimVariantRefArg) dims dimVariantRefs dataArrayRefs]
                    ) [] pathJust
                    |> listSequence
    in
    returnedPath
-- same as pathToStartDimCoordByDimVariantRef but for the last coord
pathToEndDimCoordByDimVariantRef : DimOrDArRef -> Dims -> List DimOrDArRef -> List DataArrayRef -> Maybe HeaderPath
pathToEndDimCoordByDimVariantRef dimVariantRef dims dimVariantRefs dataArrayRefs =
    let
        dimVariantRefIndex = elemIndexList dimVariantRef dimVariantRefs 
        dimVariantRefSequence = 
            case dimVariantRefIndex of
                Just index ->
                    List.take (index + 1) dimVariantRefs
                Nothing ->
                    []
        returnedPath =
            case dimVariantRefSequence of
                [] ->
                    Nothing
                pathJust ->
                    List.foldl (\dimVariantRefArg acc -> 
                        acc ++ [endDimCoordByDimVariantRef (Just dimVariantRefArg) dims dimVariantRefs dataArrayRefs]
                    ) [] pathJust
                    |> listSequence
    in
    returnedPath

-- returns a path to the first child of a headerPath in a headerTree
-- coded by ChatGpt uses recursive tree navigation
firstChildByHeaderPath : HeaderPath -> HeaderTree -> Maybe HeaderPath
firstChildByHeaderPath headerPath headerTree =
    let
        searchPath currentPath remainingPath tree =
            case (remainingPath, tree) of
                ([], _) ->
                    Nothing

                (headPath :: restPath, HeaderNode name children) ->
                    if headPath == name then
                        case restPath of
                            [] ->
                                case children of
                                    [] ->
                                        Nothing

                                    child :: _ ->
                                        case child of
                                            HeaderNode childName _ ->
                                                Just (currentPath ++ [name, childName])

                                            HeaderLeaf childName ->
                                                Just (currentPath ++ [name, childName])

                            _ ->
                                List.foldl
                                    (\child acc ->
                                        case acc of
                                            Just _ ->
                                                acc

                                            Nothing ->
                                                searchPath (currentPath ++ [name]) restPath child
                                    )
                                    Nothing
                                    children

                    else
                        Nothing

                (_, HeaderLeaf _) ->
                    Nothing
    in
    searchPath [] headerPath headerTree
-- returns a path to the first same level sibling of a headerPath in a headerTree
-- coded by me uses info from Dims and DataArrayRefs to assemble the path to the next sibling
nextSiblingByHeaderPath : HeaderPath -> Dims -> List DataArrayRef -> Maybe HeaderPath
nextSiblingByHeaderPath headerPath dims dataArrayRefs =
    case headerPathToCurCell headerPath of
        Just (dimVariantRef, coord) ->
            let
                parentPath = parentHeaderPath headerPath
                curCoords : Array String
                curCoords = 
                    if dimVariantRef == dVarIdentifier then
                        Array.fromList dataArrayRefs
                    else
                        Dict.get dimVariantRef dims
                            |> Maybe.withDefault (Array.empty)
                curIndex = elemIndexAr coord curCoords
                coordLength = Array.length curCoords
                nextIndex =
                    case curIndex of
                        Just index ->
                            if index + 1 == coordLength then
                                0
                            else
                                index + 1 -- zero only on boundary crossing

                        Nothing ->
                            -1
                nextCoord = Array.get nextIndex curCoords
                parentPathWithCrossingCheck = 
                    if nextIndex == 0 then
                        nextSiblingByHeaderPath parentPath dims dataArrayRefs
                    else if nextIndex == -1 then
                        Nothing
                    else
                        Just parentPath
            in 
            case (parentPathWithCrossingCheck, nextCoord) of 
                (Just parentPathJust , Just nextCoordJust) ->
                    parentPathJust ++ [(dimVariantRef, nextCoordJust)] |> Just
                _ ->
                    Nothing
        Nothing ->
            Nothing

-- same as nextSiblingByHeaderPath but for the previous sibling
prevSiblingByHeaderPath : HeaderPath -> Dims -> List DataArrayRef -> Maybe HeaderPath
prevSiblingByHeaderPath headerPath dims dataArrayRefs =
    case headerPathToCurCell headerPath of
        Just (dimVariantRef, coord) ->
            let
                parentPath = parentHeaderPath headerPath
                curCoords : Array String
                curCoords = 
                    if dimVariantRef == dVarIdentifier then
                        Array.fromList dataArrayRefs
                    else
                        Dict.get dimVariantRef dims
                            |> Maybe.withDefault (Array.empty)
                curIndex = elemIndexAr coord curCoords
                coordLength = Array.length curCoords
                prevIndex =
                    case curIndex of
                        Just index ->
                            if index == 0 then -- zero only on boundary crossing
                                coordLength - 1
                            else
                                index - 1 


                        Nothing ->
                            -1
                nextCoord = Array.get prevIndex curCoords
                parentPathWithCrossingCheck = 
                    if prevIndex + 1 == coordLength then
                        prevSiblingByHeaderPath parentPath dims dataArrayRefs
                    else if prevIndex == -1 then
                        Nothing
                    else
                        Just parentPath
            in 
            case (parentPathWithCrossingCheck, nextCoord) of 
                (Just parentPathJust , Just nextCoordJust) ->
                    parentPathJust ++ [(dimVariantRef, nextCoordJust)] |> Just
                _ ->
                    Nothing
        Nothing ->
            Nothing

-- True if headerCell contains a coord that is the first coord in a Dim 
-- or the first DataArrayRef in the Dataset's DataArrayRefs
isStartCoord : HeaderCell -> Dims -> List DataArrayRef -> Bool
isStartCoord headerCell dims dataArrayRefs =
    let
        (dimVariantRef, coord) = headerCell
        curCoords : Array String
        curCoords = 
            if dimVariantRef == dVarIdentifier then
                Array.fromList dataArrayRefs
            else
                Dict.get dimVariantRef dims
                    |> Maybe.withDefault (Array.empty)
        curIndex = elemIndexAr coord curCoords
    in 
    case curIndex of 
        Just index ->
            index == 0
        Nothing ->
            False

-- used in cross header navigation to check if the current headerPath is at the top left boundary
isTopLeftBoundary : HeaderPath -> Dims -> List DataArrayRef -> Bool
isTopLeftBoundary headerPath dims dataArrayRefs =
    List.all (\headerCell -> isStartCoord headerCell dims dataArrayRefs) headerPath

isEndCoord : HeaderCell -> Dims -> List DataArrayRef -> Bool
isEndCoord headerCell dims dataArrayRefs =
    let
        (dimVariantRef, coord) = headerCell
        curCoords : Array String
        curCoords = 
            if dimVariantRef == dVarIdentifier then
                Array.fromList dataArrayRefs
            else
                Dict.get dimVariantRef dims
                    |> Maybe.withDefault (Array.empty)
        curIndex = elemIndexAr coord curCoords
    in
    case curIndex of 
        Just index ->
            index == (Array.length curCoords) - 1
        Nothing ->
            False

isTopRightBoundary : HeaderPath -> Dims -> List DataArrayRef -> Bool
isTopRightBoundary headerPath dims dataArrayRefs =
    List.all (\headerCell -> isEndCoord headerCell dims dataArrayRefs) headerPath

parentHeaderPath : HeaderPath -> HeaderPath
parentHeaderPath headerPath =
    (List.drop 1 (List.reverse headerPath)|>List.reverse)

-- types used in case .. of in XModel structure update functions
type CoordChange 
    = RenameCoord
    | InsertRemoveCoord

type DimChange 
    = RenameDim DimRef DimRef
    | InsertDim DimRef Tray
    | RemoveDim DimRef
    | LinkDim DimRef DimRef
    | DestroyDim DimRef

dimChangeToString : DimChange -> String
dimChangeToString dimChange =
    case dimChange of
        RenameDim oldDimRef newDimRef ->
            "rename " ++ oldDimRef ++ " " ++ newDimRef

        InsertDim dimRef tray ->
            "insert " ++ dimRef ++ " " ++ (case tray of
                RowTray -> "RowTray"
                ColumnTray -> "ColumnTray"
                PageTray -> "PageTray"
            )

        RemoveDim dimRef ->
            "remove " ++ dimRef

        LinkDim dimRef1 dimRef2 ->
            "link " ++ dimRef1 ++ " " ++ dimRef2

        DestroyDim dimRef ->
             dimRef ++ "__destroy"

-------------------------------------------------------------------------------
--- DUPLICATED GENERAL FUNCTIONS TO AVOID MODULE ERR IN THE INTERPRETER
-- copied from List.Extra because elm-interpreter not recognizing them at runtime


{-| Returns `Just` the element at the given index in the list,
or `Nothing` if the index is out of range.
-}
lastList : List a -> Maybe a
lastList list =
    case List.reverse list of
        x :: _ ->
            Just x

        [] ->
            Nothing

lastAr : Array a -> Maybe a
lastAr arr =
    case Array.length arr of
        0 ->
            Nothing

        _ ->
            Array.get (Array.length arr - 1) arr

getAtList : Int -> List a -> Maybe a
getAtList idx xs =
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


elemIndexAr : a -> Array a -> Maybe Int
elemIndexAr x arr =
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
        |> List.map (\index -> getAtList index arr)
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
        updateAtIndex ( index, value ) arr =
            if index >= 0 && index < Array.length arr then
                Array.set index value arr

            else
                arr

        tuples =
            Array.Extra.zip updatedIndices updatedValues
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
            case ( maybeElem, acc ) of
                ( Just elem, Just elems ) ->
                    Just (Array.push elem elems)

                _ ->
                    Nothing
        )
        (Just Array.empty)
        (Array.Extra.reverse maybeArray)


