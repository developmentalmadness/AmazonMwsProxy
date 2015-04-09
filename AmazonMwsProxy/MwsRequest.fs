namespace dvMENTALmadness.amazon.client

module Example =
    
    type A =
        | First of string
        | Second of string
    
    type B =
        | First of string
        | Second of string
        | Third of string

    let createA f s =
        [A.First f; A.Second s;]

    let createB f s t =
        [B.First f; B.Second s; B.Third t]

    let toParamA elem =
        match elem with
        | A.First f -> "First", f
        | A.Second s -> "Second", s

    let toParamB elem =
        match elem with
        | B.First f -> "First", f
        | B.Second s -> "Second", s
        | B.Third t -> "Third", t

    let rec toParam f state args =
        match args with
        | [] -> state
        | head::tail -> 
            let state' = (f head)::state
            toParam f state' tail

    let toA args =
        toParam toParamA [] args

    let toB args =
        toParam toParamB [] args

    let argA = createA "one" "two"
    let argB = createB "one" "two" "three"

    let resultA = argA |> toA
    let resultB = argB |> toB




    type IdType = ASIN | GCID | SellerSKU | UPC | EAN | ISBN | JAN

    let rec toIdParams (prefix:string) target (idx:int) (ids:string list) =
        match ids with
        | [] -> target
        | head::tail -> 
            let p = (System.String.Concat(prefix, '.', idx), head)::target
            let nidx = idx + 1
            toIdParams prefix p nidx tail

    // GetCompetitivePricingForASINRequest
    type P =
        | MarketplaceId of string
        | SellerId of string
        | ASINList of string list
        | MWSAuthToken of string
    
    let getCompetitivePricingForASINRequest marketplaceId sellerId asinList mwsAuthToken =
        [P.MarketplaceId marketplaceId; P.SellerId sellerId; P.ASINList asinList; P.MWSAuthToken mwsAuthToken]

    let toParamP elem state = 
        match elem with
            | P.MarketplaceId m -> [("MarketplaceId", m)]
            | P.SellerId s -> [("SellerId", s)]
            | P.ASINList ids -> ids |> toIdParams "ASINList.ASIN" state 1
            | P.MWSAuthToken t -> [("MWSAuthToken", t)]

    // GetMatchingProductForIdRequest
    type M =
        | MarketplaceId of string
        | SellerId of string
        | IdType of IdType
        | IdList of string list
        | MWSAuthToken of string

    let getMatchingProductForIdRequest marketplaceId sellerId idType idList mwsAuthToken =
        [M.MarketplaceId marketplaceId; M.SellerId sellerId; M.IdType idType; M.IdList idList; M.MWSAuthToken mwsAuthToken]

    let toParamM elem state = 
        match elem with
            | M.MarketplaceId m -> [("MarketplaceId", m)]
            | M.SellerId s -> [("SellerId", s)]
            | M.IdType t -> [("IdType", sprintf "%A" t)]
            | M.IdList ids -> ids |> toIdParams "ListId.Id" state 1
            | M.MWSAuthToken t -> [("MWSAuthToken", t)]
        
    let argM = getMatchingProductForIdRequest "mpid" "sId" ASIN ["Key1"; "Key2"; "Key3"; "Key4"; "Key5"] "mwstoken"
    let argP = getCompetitivePricingForASINRequest "mpid" "sId" ["Key1"; "Key2"; "Key3"; "Key4"; "Key5"] "mwstoken"

    let rec toParamList toParam state args =
        match args with
        | [] -> state
        | head::tail -> 
            let state' = (toParam head state)
            let state'' = List.fold (fun acc elem -> elem::acc) state' state
            toParamList toParam state'' tail
    
    let toM args =
        toParamList toParamM [] argM
    let toP args =
        toParamList toParamP [] argP
        
    let test = toParamList toParamM [] argM
    let test' = toParamList toParamP [] argP

