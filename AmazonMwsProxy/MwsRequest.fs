namespace dvMENTALmadness.amazon.client

module MwsRequest =
    
    type RequestParameter =
        | Parameter of string * string
        | ListParameter of string * string list

    type IdType = ASIN | GCID | SellerSKU | UPC | EAN | ISBN | JAN

//    type GetMatchingProductForIdRequest = {
//        MarketplaceId: string;
//        SellerId: string;
//        IdType: IdType;
//        IdList: string list;
//        MWSAuthToken: string;
//    }

// using a DU, I would match over a list of parameters
    type GetMatchingProductForIdRequest' =
        | MarketplaceId of string
        | SellerId of string
        | IdType of IdType
        | IdList of string list
        | MWSAuthToken of string

    let arg = [MarketplaceId "mpid"; SellerId "sId"; IdType ASIN; IdList ["Key1"; "Key2"; "Key3"; "Key4"; "Key5"]; MWSAuthToken "mwstoken"]

    let rec toIdParams (prefix:string) target (idx:int) (ids:string list) =
        match ids with
        | [] -> target
        | head::tail -> 
            let p = (System.String.Concat(prefix, idx), head)::target
            let nidx = idx + 1
            toIdParams prefix p nidx tail
             
    let rec toParams state args =
        match args with
        | [] -> state
        | head::tail -> 
            let state' = match head with
                            | MarketplaceId m -> ("MarketplaceId", m)::state
                            | SellerId s -> ("SellerId", s)::state
                            | IdType t -> ("IdType", sprintf "%A" t)::state
                            | IdList ids -> ids |> toIdParams "ListId.Id" state 1
                            | MWSAuthToken t -> ("MWSAuthToken", t)::state
            toParams state' tail
        
        
    let test = toParams [] arg
