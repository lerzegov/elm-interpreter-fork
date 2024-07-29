module TypesXModel exposing (..)
-- created to allow import in Types.elm for xModel in Env
-- imported also in XModel.elm
import Array exposing (Array)
import FastDict as Dict exposing (Dict)
import FormulaParser
import List exposing (range)






type alias DimRef = String

type alias Coord = String

type alias DVarRef = String

-- used in DatasetView to specify Trays
-- includes "categorical" DimRefs plus DVarDimRef to handle the DataArrays in the Dataset as coords
-- should not affect iloc and loc on Dataset
type DimVariantRef
    = CategDimRef DimRef
    | DVarDimRef DVarRef -- container of DataArrayRefs used as pseudo-coords
-- hard coded name of the dVarDim
dVarIdentifier : DVarRef
dVarIdentifier = "dVar"

type CoordSpecifier
    = SingleCoord Coord
    | CoordRange (Coord, Coord)
    | CoordList (Array Coord)
    | CoordNone

coordSpecToString : CoordSpecifier -> Maybe String
coordSpecToString coordSpec = 
    case coordSpec of
        SingleCoord coord -> Just coord
        CoordRange (start, end) -> Just start -- provisional, just return the first coord
        -- CoordRange (start, end) -> ("FROM "++ start ++ "TO" ++ end) |> Just -- alternative
        CoordList coords -> Array.get 0 coords -- provisional, just return the first coord
        CoordNone -> Nothing

type alias DatasetRef = String

type alias DataArrayRef = String
-- to avoid Variant type, we use a record with both number and text
-- text may be used for numbers as well, for example to hold formatted numbers
-- for string values, text should be the only field used

-- final store of dimension data
type alias Dims = Dict DimRef (Array Coord)

type alias DataArray = 
    { ref : DataArrayRef -- name
    , datasetRef : Maybe DatasetRef -- name of the container dataset
    , data : Array Float
    , text : Array String
    , localDims : Maybe Dims -- local dims for the DataArray
    , localDimRefs : Maybe (List DimRef) -- local dimRefs for the DataArray
    , pointedFormulas : Dict FlatIndex String -- formulas pointed by the DataArray
    }

-- final store of array data
type alias DataArrays = Dict DataArrayRef DataArray
type alias ModuleSource = String

type alias Dataset = 
    { ref : DatasetRef -- name
    , dimRefs : List DimRef -- ordered list of dims
    , dataArrayRefs : List DataArrayRef -- ordered list of data arrays names (DVars)
    , dataArrays : DataArrays
    , formulas : ModuleSource
    , defaultDataArrayRef : Maybe DataArrayRef
    }

type alias Datasets = Dict DatasetRef Dataset
-- single source of truth for the data
type alias XModel = 
    { modelRef : String
    , datasetRefs : List DatasetRef
    , datasets : Datasets
    , dims : Dims
    , datasetsToRecalc : List DatasetRef
    }

-- usable by loc iloc
type alias RangeDef = 
    { datasetRef : Maybe String
    , dataArrayRef : Maybe String
    , dimCoords : Maybe (List (DimRef, CoordSpecifier)) -- no DimVariantRef used in loc, check
    , isExplicitDataArrayRef : Bool 
    }




-- custom types and basic helper functions for calcFlatIndex, iloc and loc
type alias Index = Int
type alias FlatIndex = Int
type alias CoordIndex = Int
type alias Range = (Int, Int)
type alias Stride = Int
type alias Size = Int
type alias PosVec = List CoordIndex
type alias CoordVec = List Coord
type alias Dimension = (Stride, Int) -- (stride for the dimension, size of the dimension)
type alias CoordDict = Dict Coord CoordIndex


-- used to filter the list of coord indices to include in a DataArray view
type IndexSpecifier
    = SingleIndex CoordIndex -- single coord index
    | IndexRange (CoordIndex, CoordIndex) -- contiguous range of coord indices
    | IndexList (Array CoordIndex) -- sparse list of coord indices
    | IndexNone -- no filter, all coord indices
