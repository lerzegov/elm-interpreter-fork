module TypesXModel exposing (..)

-- created to allow import in Types.elm for xModel in Env
-- imported also in XModel.elm
-- FastDict is a custom Dict implementation with faster updates by miniBill

import Array exposing (Array)
import FastDict as Dict exposing (Dict)
import FormulaParser
import List exposing (range)



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


type DimVariant
    = CategDimRef DimRef
    | DVarDimRef DVarRef -- container of DataArrayRefs used as pseudo-coords



-- used to avoid type mismatch and no comparable in Dict using DimVariant


type alias DimVariantRef =
    String

type alias HeaderCell =
    ( DimVariantRef, Coord )

type alias HeaderPath =
    List HeaderCell


type HeaderTree
    = HeaderNode HeaderCell (List HeaderTree)
    | HeaderLeaf HeaderCell

getHeaderTreeWidth : HeaderTree -> Int
getHeaderTreeWidth tree =
    case tree of
        HeaderLeaf _ ->
            -- A single leaf has a width of 1
            1

        HeaderNode _ children ->
            -- The width of a node is the sum of the widths of its children
            List.foldl (\child acc -> acc + getHeaderTreeWidth child) 0 children

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

-- NB don't confuse dimVariantRef with dimVarRef!

headerPathToCurCell : HeaderPath -> HeaderCell
headerPathToCurCell headerPath =
    case List.reverse headerPath of
        (lastHeader :: _) ->
            lastHeader

        [] ->
            Debug.todo "Empty headerPath"

headerPathToCurCoord : HeaderPath -> Coord
headerPathToCurCoord headerPath =
    case headerPathToCurCell headerPath of
        (dimVariantRef, coord) ->
            coord

dimVariantToDimVariantRef : DimVariant -> DimRef
dimVariantToDimVariantRef dimVariantRef =
    case dimVariantRef of
        CategDimRef dimRef ->
            dimRef

        DVarDimRef dVarRef ->
            dVarRef



-- hard coded name of the dVarDim in a dataset
-- has dataArrayRefs as coords


dVarIdentifier : DVarRef
dVarIdentifier =
    "dVar"



-- used to filter coordinates in XModel.loc


type CoordSpecifier
    = SingleCoord Coord
    | CoordRange ( Coord, Coord )
    | CoordList (Array Coord)
    | CoordNone


coordSpecToString : CoordSpecifier -> Maybe String
coordSpecToString coordSpec =
    case coordSpec of
        SingleCoord coord ->
            Just coord

        CoordRange ( start, end ) ->
            Just start

        -- provisional, just return the first coord
        -- CoordRange (start, end) -> ("FROM "++ start ++ "TO" ++ end) |> Just -- alternative
        CoordList coords ->
            Array.get 0 coords

        -- provisional, just return the first coord
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
    , localDims : Maybe Dims -- local dims for the DataArray, make it independent of the Dataset dims
    , localDimRefs : Maybe (List DimRef) -- local dimRefs for the DataArray, parallel to localDims
    , pointedFormulas : Dict FlatIndex String -- formulas pointed by the DataArray, for recalc cycles
    }



-- final store of array data


type alias DataArrays =
    Dict DataArrayRef DataArray



-- source code for a module formulas


type alias ModuleSource =
    String


type alias Dataset =
    { ref : DatasetRef -- name
    , dimRefs : List DimRef -- ordered list of dimRefs referring to XModel.dims
    , dataArrayRefs : List DataArrayRef -- ordered list of data arrays names (DVars)
    , dataArrays : DataArrays
    , formulas : ModuleSource
    , defaultDataArrayRef : Maybe DataArrayRef
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


type alias CoordIndex =
    Int



-- position of a coord in a dimension coord list


type alias Range =
    ( Int, Int )


type alias Stride =
    Int



-- cum size of the coords in previous dimensions


type alias Size =
    Int


type alias PosVec =
    List CoordIndex



-- indices of a data cell in a multidimensional array


type alias CoordVec =
    List Coord



-- coordinates of a data cell in a multidimensional array


type alias Dimension =
    ( Stride, Int )



-- (stride for the dimension, size of the dimension)


type alias CoordDict =
    Dict Coord CoordIndex



-- used in XModel.iloc to filter the list of coord indices to include in a DataArray view


type IndexSpecifier
    = SingleIndex CoordIndex -- single coord index
    | IndexRange ( CoordIndex, CoordIndex ) -- contiguous range of coord indices
    | IndexList (Array CoordIndex) -- sparse list of coord indices
    | IndexNone -- no filter, all coord indices
