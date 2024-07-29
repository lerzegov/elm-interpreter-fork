module Financial exposing (..)
-- put both in main /src folder and in /codegen/Elm folder
myE: Float
myE = 2.718281828459045

myLog : Float -> Float
myLog x = logBase myE x -- e defined in Basics, here replaced with myE for precision

myExp : Float -> Float
myExp x = myE^x -- defined in basics

pvZcb : Float -> Float -> Float -> Float
pvZcb cashFlow maturity rate =
    cashFlow / (1+rate)^maturity

ytmZcb : Float -> Float -> Float -> Float
ytmZcb cashFlow maturity presentValue =
    (cashFlow / presentValue) ^ (1/maturity) - 1

pvBond : Float -> Float -> Float -> Float -> Float
pvBond cpn maturity faceValue rate =
    (cpn/rate) * ( 1 - 1/(1+rate)^maturity) + faceValue / (1+rate)^maturity

type alias BondAnag = { coupon : Float, maturity : Int, faceValue : Float }
pvBondAnag : BondAnag -> Int -> Float -> Float
pvBondAnag bondAnag evalDate rate =
    pvBond bondAnag.coupon (toFloat (bondAnag.maturity - evalDate)) bondAnag.faceValue rate

ytmBondAnag : BondAnag -> Int -> Float -> Float
ytmBondAnag bondAnag evalDate price =
    ytmBond bondAnag.coupon (toFloat (bondAnag.maturity - evalDate)) bondAnag.faceValue price

ytmBond : Float -> Float -> Float -> Float -> Float
ytmBond coupon maturity faceValue price =
    let
        npvLoc r = pvBond coupon maturity faceValue r - price
        derivative r =  (npvLoc (r + 0.000001) - npvLoc r) / 0.000001
        startRate = 0.1
        nextGuess r = r - npvLoc r / derivative r
        iterate guess =
            let
                newGuess = nextGuess guess
            in
            if abs (npvLoc newGuess) < 0.0000001 then
                newGuess
            else
                iterate newGuess
    in
    iterate startRate -- Starting guess for ytm

npv : Float -> List Float -> Float -> Float 
npv initialFlow expectedFlows rate =
    initialFlow + List.sum (List.indexedMap (\t cf 
                    ->  let tFlt = toFloat t in
                        cf / (1 + rate) ^ (tFlt+1)) expectedFlows) 

irr : Float -> List Float -> Float
irr initialFlow expectedFlows  =
    let
        guessNpv r = npv initialFlow expectedFlows r
        derivative r = (guessNpv (r + 0.000001) - guessNpv r) / 0.000001
        startRate = 0.1
        nextGuess r = r - guessNpv r / derivative r
        iterate guess =
            let
                newGuess = nextGuess guess
            in
            if abs (guessNpv newGuess) < 0.0000001 then
                newGuess
            else
                iterate newGuess
    in
    iterate startRate -- Starting guess for irr


-- converted from Haskell https://cseweb.ucsd.edu/~goguen/courses/130/SayBlackScholes.html
cdf : Float -> Float
cdf x =
    let
        w = 1.0 - 1.0 / sqrt (2.0 * pi) * myExp(-l * l / 2.0) * poly k
        k = 1.0 / (1.0 + 0.2316419 * l)
        l = abs x
        poly = horner coeff
        coeff = [0.0, 0.31938153, -0.356563782, 1.781477937, -1.821255978, 1.330274429]
    in
    if x < 0 then
        1.0 - w
    else
        w

horner : List Float -> Float -> Float
horner coeff base =
    let
        multAdd x y = y * base + x
    in
    List.foldr multAdd 0 coeff



-- black scholes call option price
callPriceBS : Float -> Float -> Float -> Float -> Float -> Float 
callPriceBS s k r t v =
    let
        d1 = (myLog (s / k) + (r + v * v / 2) * t) / (v * sqrt t)
        d2 = d1 - v * sqrt t
    in
    s * cdf d1 - k * myExp (-r * t) * cdf d2

-- garman kohlhagen call option price
callPriceGK : Float -> Float -> Float -> Float -> Float -> Float -> Float 
callPriceGK s k r d t v =
    let
        d1 = (myLog (s / k) + (r - d + v * v / 2) * t) / (v * sqrt t)
        d2 = d1 - v * sqrt t
    in
    s * myExp (-d * t) * cdf d1 - k * myExp (-r * t) * cdf d2
