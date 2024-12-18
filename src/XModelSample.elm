module XModelSample exposing (..)

import FastDict as Dict exposing (Dict)
import TypesXModel exposing (..)
import Array exposing (Array)
import Types exposing (Value(..))
-- ============== SAMPLE DATA ==================
-- tried to move to separate module but imported types not recognized there
-- ==Dims==


azienda : DimRef
azienda =
    "azienda"


anno : DimRef
anno =
    "anno"


voce : DimRef
voce =
    "voce"


scenario : DimRef
scenario =
    "scenario"


grChar : DimRef
grChar =
    "grChar"


cigar : DimRef
cigar =
    "cigar"


aziendaCoords : Array Coord



-- deCapitalized to make coord names usable as variables, problem with numbers

aziendaCoords =
    Array.fromList [ "alfa", "beta"]
-- aziendaCoords =
--     Array.fromList [ "alfa", "beta", "gamma", "delta" , "epsilon", "zeta", "eta", "theta" 
--         , "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi"
--         , "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega" ]


annoCoords : Array Coord
annoCoords =
    --Array.fromList [ "2021", "2022", "2023" ]
    Array.fromList [ "a1", "a2", "a3"]



-- , "2024", "2025", "2026", "2027", "2028", "2029"]


voceCoords : Array Coord
voceCoords =
    Array.fromList [ "ricavi", "costoVen", "margContrib", "speseVGA", "ebitda", "amm", "ebit", "tax", "unlevNetIncome" ]


scenarioCoords : Array Coord
scenarioCoords =
    Array.fromList [ "base", "worst", "best", "forecast" ]


caratteriGreciCoords : Array Coord
caratteriGreciCoords =
    Array.fromList [ "alfa", "beta", "gamma", "delta" ]


cigarCoords : Array Coord
cigarCoords =
    Array.fromList [ "alfa", "nazionali" ]


myDims : Dims
myDims =
    Dict.fromList
        [ ( azienda, aziendaCoords )
        , ( anno, annoCoords )
        , ( voce, voceCoords )
        , ( scenario, scenarioCoords )
        , ( grChar, caratteriGreciCoords )
        , ( cigar, cigarCoords )
        ]



-- == ce Dataset ==


ce : DatasetRef
ce =
    "Ce"



-- used as module name for formulas must be capitalized


valore : DataArrayRef
valore =
    "valore"



-- used as default dataArrayRef


note : DataArrayRef
note =
    "note"


calcValore : DataArrayRef
calcValore =
    "calcValore"


calcNote : DataArrayRef
calcNote =
    "calcNote"


valoreListSeed : List Float
valoreListSeed =
    [ 100
    , 0 / 0 -- for NaN
    , 0 / 0
    , 6
    , 0 / 0
    , 12
    , 0 / 0
    , 0 / 0
    , 0 / 0 -- 2021 Alfa
    , 1007
    , 0 / 0
    , 0 / 0
    , 67
    , 0 / 0
    , 127
    , 0 / 0
    , 0 / 0
    , 0 / 0 -- 2021 Beta
    , 209
    , 0 / 0
    , 0 / 0
    , 17
    , 0 / 0
    , 29
    , 0 / 0
    , 0 / 0
    , 0 / 0 -- 2022 Alfa
    , 2007
    , 0 / 0
    , 0 / 0
    , 127
    , 0 / 0
    , 247
    , 0 / 0
    , 0 / 0
    , 0 / 0 -- 2022 Beta
    , 300
    , 0 / 0
    , 0 / 0
    , 18
    , 0 / 0
    , 16
    , 0 / 0
    , 0 / 0
    , 0 / 0 -- 2023 Alfa
    , 3007
    , 0 / 0
    , 0 / 0
    , 187
    , 0 / 0
    , 167
    , 0 / 0
    , 0 / 0
    , 0 / 0 -- 2023 Beta
    ]

valoreList : List Float
valoreList =
    List.repeat 1 valoreListSeed
        |> List.concat

valoreArray : Array Float
valoreArray =
    Array.fromList valoreList



-- ++ valoreList ++ valoreList)


ceDataArrays : DataArrays
ceDataArrays =
    Dict.fromList
        [ ( valore
          , { ref = valore
            , datasetRef = Just ce
            , data = valoreArray
            , text = Array.empty
            , localDims = Nothing
            , localDimRefs = Nothing
            , pointedFormulas = Dict.empty
            }
          )
        ]


ceFormulas : ModuleSource
ceFormulas =
    """module Ce exposing (..)
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
macroFormulas =
    """module Macro exposing (..)
macro__inflazione_forecast = macro__inflazione_base * 1.02
macro__cambioUsdEur_forecast = macro__cambioUsdEur_base * 3.07
macro__cambioCalc = macro__cambioUsdEur * 5 -- calc on dataArrays not yet implemented
dependentDatasetRefs = ["Ce"]

"""


azFormulas : ModuleSource
azFormulas =
    """module Az exposing (..)
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
az =
    "Az"


nome : DataArrayRef
nome =
    "nome"


settore : DataArrayRef
settore =
    "settore"


nrDip : DataArrayRef
nrDip =
    "nrDip"


nomeArray : Array String
nomeArray =
    Array.fromList
        [ "Alfa SpA"
        , "Beta SpA"
        , "Alfa SpA"
        , "Beta SpA"
        , "Alfa SpA"
        , "Beta SpA"
        ]


settoreArray : Array String
settoreArray =
    Array.fromList
        [ "Industria"
        , "Commercio" -- per anno
        , "Industria"
        , "Commercio"
        , "Industria"
        , "Commercio"
        ]


nrDipArray : Array Float
nrDipArray =
    Array.fromList
        [ 10
        , 0 / 0
        , 12
        , 0 / 0
        , 20
        , 0 / 0
        ]


azDataArrays : DataArrays
azDataArrays =
    Dict.fromList
        [ ( nome
          , { ref = nome
            , datasetRef = Just az
            , data = Array.empty
            , text = nomeArray
            , localDims = Nothing
            , localDimRefs = Nothing
            , pointedFormulas = Dict.empty
            }
          )
        , ( settore
          , { ref = settore
            , datasetRef = Just az
            , data = Array.empty
            , text = settoreArray
            , localDims = Nothing
            , localDimRefs = Nothing
            , pointedFormulas = Dict.empty
            }
          )
        , ( nrDip
          , { ref = nrDip
            , datasetRef = Just az
            , data = nrDipArray
            , text = Array.empty
            , localDims = Nothing
            , localDimRefs = Nothing
            , pointedFormulas = Dict.empty
            }
          )
        ]



-- == macro Dataset ==


macro : DatasetRef
macro =
    "Macro"


inflazione : DataArrayRef
inflazione =
    "inflazione"


cambioUsdEur : DataArrayRef
cambioUsdEur =
    "cambioUsdEur"


cambioCalc : DataArrayRef
cambioCalc =
    "cambioCalc"


inflazioneArray : Array Float
inflazioneArray =
    Array.fromList
        [ 2.3
        , 10.5
        , 1.5
        , 0 / 0 -- 2021
        , 3.1
        , 14
        , 2.4
        , 0 / 0
        , 4.6
        , 20
        , 3.1
        , 0 / 0
        ]


cambioUsdEurArray : Array Float
cambioUsdEurArray =
    Array.fromList
        [ 1.1
        , 1.2
        , 1.3
        , 0 / 0 -- 2021
        , 1.0
        , 0.9
        , 0.8
        , 0 / 0
        , 1.5
        , 1.6
        , 1.7
        , 0 / 0
        ]


cambioCalcArray : Array Float
cambioCalcArray =
    Array.fromList
        [ 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        , 0 / 0
        ]


macroDataArrays : DataArrays
macroDataArrays =
    Dict.fromList
        [ ( inflazione
          , { ref = inflazione
            , datasetRef = Just macro
            , data = inflazioneArray
            , text = Array.empty
            , localDims = Nothing
            , localDimRefs = Nothing
            , pointedFormulas = Dict.empty
            }
          )
        , ( cambioUsdEur
          , { ref = cambioUsdEur
            , datasetRef = Just macro
            , data = cambioUsdEurArray
            , text = Array.empty
            , localDims = Nothing
            , localDimRefs = Nothing
            , pointedFormulas = Dict.empty
            }
          )
        , ( cambioCalc
          , { ref = cambioCalc
            , datasetRef = Just macro
            , data = cambioCalcArray
            , text = Array.empty
            , localDims = Nothing
            , localDimRefs = Nothing
            , pointedFormulas = Dict.empty
            }
          )
        ]



-- === Greche dataset to test duplicate coord names ===


greche : DatasetRef
greche =
    "Greche"


greekDAr : DataArrayRef
greekDAr =
    "caratteriGreci"


caratteriGreciArrayText : Array String
caratteriGreciArrayText =
    Array.fromList [ "α", "β", "γ", "δ", "ε", "ζ", "η", "θ" ]


caratteriGreciArrayFloat : Array Float
caratteriGreciArrayFloat =
    Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7 ]


myDatasets : Dict DatasetRef { ref : DatasetRef, dimRefs : List DimRef, dataArrayRefs : List DataArrayRef, dataArrays : DataArrays, formulas : ModuleSource, defaultDataArrayRef : Maybe DataArrayRef }
myDatasets =
    Dict.fromList
        [ ( ce
          , { ref = ce

            -- order changed, now seems ok
            , dimRefs = [ anno, azienda, voce ] -- ordered list of dims
            , dataArrayRefs = [ valore ] -- [valore, note, calcValore, calcNote]
            , dataArrays = ceDataArrays
            , formulas = ceFormulas
            , defaultDataArrayRef = Just valore
            }
          )
        , ( az
          , { ref = az
            , dimRefs = [ anno, azienda ] -- ordered list of dims
            , dataArrayRefs = [ nome, settore, nrDip ]
            , dataArrays = azDataArrays
            , formulas = azFormulas
            , defaultDataArrayRef = Nothing
            }
          )
        , ( macro
          , { ref = macro
            , dimRefs = [ anno, scenario ] -- ordered list of dims
            , dataArrayRefs = [ inflazione, cambioUsdEur, cambioCalc ]
            , dataArrays = macroDataArrays
            , formulas = macroFormulas
            , defaultDataArrayRef = Nothing
            }
          )
        , ( greche
          , { ref = greche
            , dimRefs = [ grChar, cigar ] -- ordered list of dims
            , dataArrayRefs = [ greekDAr ]
            , dataArrays =
                Dict.fromList
                    [ ( greekDAr
                      , { ref = greekDAr
                        , datasetRef = Just greche
                        , data = caratteriGreciArrayFloat
                        , text = caratteriGreciArrayText
                        , localDims = Nothing
                        , localDimRefs = Nothing
                        , pointedFormulas = Dict.empty
                        }
                      )
                    ]
            , formulas = ""
            , defaultDataArrayRef = Nothing
            }
          )
        ]


myXModel : XModel
myXModel =
    { modelRef = "finPlan"
    , datasetRefs = [ ce, az, macro ]
    , datasets = myDatasets
    , dims = myDims -- arrays are hyerarchicallly stored in datasets
    , datasetsToRecalc = []
    , dimChanges = []
    , updatedHeaderPath = Nothing
    , updatedDimOrDArRef = Nothing
    }



-- == test values for elm-interpreter ==
-- added by Luca in first atempts, no longer relevant
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
        [-- ( "intValue", Int 42 )
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
