module XModelHelpers exposing (..)

import TypesXModel exposing (..)
import Json.Encode exposing (Value, list, string)
import FastDict as Dict exposing (Dict)

datasetRefsToJson : XModel -> Value
datasetRefsToJson xModel =
    list string xModel.datasetRefs

dataArrayRefsToJson : DatasetRef -> XModel -> Value
dataArrayRefsToJson datasetRef xModel =
    let
        maybeDataset = Dict.get datasetRef xModel.datasets
    in
    case maybeDataset of
        Just dataset ->
            list string dataset.dataArrayRefs
        Nothing ->
            list string []

dimRefsToJson : DatasetRef -> DataArrayRef -> XModel -> Value
dimRefsToJson datasetRef dataArrayRef xModel =
    let
        maybeDataset = Dict.get datasetRef xModel.datasets
        maybeDataArray =
            case maybeDataset of
                Just dataset ->
                    Dict.get dataArrayRef dataset.dataArrays
                Nothing ->
                    Nothing
    in
    case maybeDataArray of
        Just dataArray ->
            case dataArray.localDimRefs of
                Just dimRefs ->
                    list string dimRefs
                Nothing ->
                    list string []
        Nothing ->
            list string []
