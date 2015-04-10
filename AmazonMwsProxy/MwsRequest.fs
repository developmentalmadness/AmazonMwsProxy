namespace dvMENTALmadness.amazon.client

module Binding =
    let strToInt str =
        match System.Int32.TryParse(str) with
        | (true, i) -> Some i
        | (false, _) -> None

    // using bind
    let bind (m, f) =
        Option.bind f m
    
    let return' x = Some x

    let strToIntWorkflow x y z =
        bind (x |> strToInt, fun a ->
        bind (y |> strToInt, fun b ->
        bind (z |> strToInt, fun c ->
            let d = a + b + c
            return' d
        )))

    let good = strToIntWorkflow "1" "2" "3"
    let bad = strToIntWorkflow "1" "two" "3"

    // using computation expression
    type MaybeBuilder() =
        member this.Bind(m, f) = Option.bind f m
        member this.Return(x) = Some x

    let maybe = new MaybeBuilder()
    
    let strToIntWorkflow' x y z =
        maybe {
            let! a = strToInt x
            let! b = strToInt y
            let! c = strToInt z
            return a + b + c
        }

    let good' = strToIntWorkflow' "1" "2" "3"
    let bad' = strToIntWorkflow' "1" "two" "3"

    // using infix operator
    let (>>=) m f = Option.bind f m

    let strAdd str i =
        match strToInt str with
        | None -> None
        | Some i' -> Some(i' + i)

    let strToIntWorkflow'' x y z =
        x |> strToInt >>= strAdd y >>= strAdd z

    let good'' = strToIntWorkflow'' "1" "2" "3"
    let bad'' = strToIntWorkflow'' "1" "two" "3"

module Request =
    open System
    open System.Text
    open System.Collections.Generic
    open System.Net
    open System.Globalization
    open System.IO
    open log4net
    open dvMENTALmadness.Core.Logging
    
    let log = LogManager.GetLogger("Request")

    [<Literal>]
    let APPLICATION_NAME = "AppName"
    [<Literal>]
    let APPLICATION_VERSION = "AppVersion"
    [<Literal>]
    let SERVICE_VERSION = "ServiceVersion"
    [<Literal>]
    let SIGNATURE_METHOD = "SignatureMethod"
    [<Literal>]
    let SIGNATURE_VERSION = "SignatureVersion"
    [<Literal>]
    let ENDPOINT = "Endpoint"
    [<Literal>]
    let SERVICE_PATH = "ServicePath"
    [<Literal>]
    let MAX_ERROR_RETRY = "MaxErrorRetry"
    [<Literal>]
    let PROGRAMMING_LANGUAGE = "F#"
    [<Literal>]
    let AWS_ACCESS_KEY = "AwsAccessKey"
    [<Literal>]
    let TIMESTAMP = "Timestamp"
    [<Literal>]
    let SIGNATURE = "Signature"

    type responseHeaderMetadata = {
        RequestId:string;
        ResponseContext:string list;
        Timestamp:string;
        QuotaMax:int option;
        QuotaRemaining:int option;
        QuotaResetsAt:DateTime option
    }

    let createConfig (appName:string) (appVersion:string) (serviceVersion:string) (signatureMethod:string) (signatureVersion:string) (endpoint:string) (servicePath:string) (awsAccessKey:string) =
        [|(APPLICATION_NAME, appName); (APPLICATION_VERSION, appVersion); (SERVICE_VERSION, serviceVersion); 
            (SIGNATURE_METHOD, signatureMethod); (SIGNATURE_VERSION, signatureVersion); (ENDPOINT, endpoint); 
            (SERVICE_PATH, servicePath); (AWS_ACCESS_KEY, awsAccessKey)|] |> dict

    let getServiceUrl (config:IDictionary<string, string>) =
        Uri(Uri(config.[ENDPOINT]), config.[SERVICE_PATH])

    let getUserAgent (config:IDictionary<string, string>) =
        let data = StringBuilder()
        data.Append(config.[APPLICATION_NAME])
            .Append("/")
            .Append(config.[APPLICATION_VERSION])
            .Append(" (")
            .Append("Language=")
            .Append(PROGRAMMING_LANGUAGE)
            .Append(")")
            .ToString()

    let getHttpClient config =
        let url = getServiceUrl config
        let request = WebRequest.Create(url) :?> HttpWebRequest
        //TODO: add support for proxy here (after I add it to the config)
        request.UserAgent <- getUserAgent config
        request.Method <- "POST"
        request.Timeout <- 50000 // TODO: make timetout cnofigurable
        request.ContentType <- "application/x-www-form-urlencoded; charset=utf-8"
        //TODO: add support for headers (loop through configured headers and set request.Headers[key] <- value
        request

    let addRequiredParameters config request =
        let serviceUri = getServiceUrl config
        let request' = (TIMESTAMP,System.DateTimeOffset.UtcNow.ToString("o"))::(AWS_ACCESS_KEY, config.[AWS_ACCESS_KEY])::request
        let signature = MwsUtil.signParameters serviceUri config.[SIGNATURE_VERSION] config.[SIGNATURE_METHOD] request' config.[AWS_ACCESS_KEY]
        match signature with
        | Some x -> (SIGNATURE, x)::request'
        | None -> failwith "Failed to sign request parameters for request."

    let getParametersAsString request =
        let data = StringBuilder()
        let build ((k:string), (v:string)) =
            data.Append(k)
                .Append('=')
                .Append(MwsUtil.urlEncode v false)
                .Append("&") |> ignore

        request 
            |> List.sortBy fst
            |> List.iter (fun x -> build x)
        
        data.Remove(data.Length - 2, 1).ToString()

    let getResponseHeaderMetadata (response:HttpWebResponse) =
        let requestId = response.GetResponseHeader("x-mws-request-id")
        let timestamp = response.GetResponseHeader("x-mws-timestamp")
        let ctxStr = response.GetResponseHeader("x-mws-response-context")
        let ctx = ctxStr.Split(',')

        let qmax = 
            match Double.TryParse(response.GetResponseHeader("x-mws-quota-max")) with
            | (false, _) -> None
            | (true, x) -> Some(Convert.ToInt32(x))
        let qleft =
            match Double.TryParse(response.GetResponseHeader("x-mws-quota-remaining")) with
            | (false, _) -> None
            | (true, x) -> Some(Convert.ToInt32(x))
        let qreset =
            match DateTime.TryParse(response.GetResponseHeader("x-mws-quota-resetsOn"), CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal) with
            | (false, _) -> None
            | (true, x) -> Some x

        { 
            RequestId = requestId;
            ResponseContext = ctx |> Array.toList;
            Timestamp = timestamp;
            QuotaMax = qmax;
            QuotaRemaining = qleft;
            QuotaResetsAt = qreset; 
        }

    let executeRequest config request =
        let queryString = request
                            |> addRequiredParameters config
                            |> getParametersAsString
        
        let buffer = Encoding.UTF8.GetBytes(queryString)

        try
            let http = getHttpClient config
            use client = http.GetRequestStream()
            client.Write(buffer, 0, buffer.Length)

            use response = http.GetResponse() :?> HttpWebResponse
            let message = response.StatusDescription
            let meta = getResponseHeaderMetadata response
            use rdr = new StreamReader(response.GetResponseStream(), Encoding.UTF8)

            //TODO: parse response before returning
            response.StatusCode, rdr.ReadToEnd()

            //TODO: add retry loop when status code is InternalServerError (500)

            // throw custom exception with statusCode and body
        with 
        | :? WebException as ex -> 
        (
            error log ex "An error occurred during executeRequest"
            
            use errResponse = ex.Response :?> HttpWebResponse
            let errStatus, errBody = match errResponse with
                                        | null -> reraise()
                                        | er -> 
                
                                            use errRdr = new StreamReader(er.GetResponseStream(), Encoding.UTF8)
                                            er.StatusCode, errRdr.ReadToEnd()

            // retry ?

            // throw custom exeception with errStatus and errBody
            reraise()
        )
        | ex -> (
                error log ex "An error occurred during executeRequest"
                reraise()
        )

    let unmapList prefix (ids:string list) =
        List.mapi (fun i e -> sprintf "%s.%d" prefix i, e) ids


module Products =
    [<Literal>]
    let SERVICE_VERSION = "2011-10-01"
    [<Literal>]
    let DEFAULT_SERVICE_PATH = "Products/2011-10-01"
        
    type IdType = ASIN | GCID | SellerSKU | UPC | EAN | ISBN | JAN

    // GetCompetitivePricingForASINRequest
    type P =
        | MarketplaceId of string
        | SellerId of string
        | ASINList of string list
        | MWSAuthToken of string
        | Action of string
        | Version of string

    let unmapP list state = 
        let um = function
            | P.MarketplaceId m -> [("MarketplaceId", m)]
            | P.SellerId s -> [("SellerId", s)]
            | P.ASINList ids -> ids |> Request.unmapList "ASINList.ASIN"
            | P.MWSAuthToken t -> [("MWSAuthToken", t)]
            | P.Action a -> [("Action", a)]
            | P.Version v -> [("Version", v)]
        List.map um list

    let getCompetitivePricingForASINRequest marketplaceId sellerId asinList mwsAuthToken =
        [P.MarketplaceId marketplaceId; P.SellerId sellerId; P.ASINList asinList; 
            P.MWSAuthToken mwsAuthToken; P.Action "GetCompetitivePricingForASIN"; P.Version SERVICE_VERSION]

    // GetMatchingProductForIdRequest
    type M =
        | MarketplaceId of string
        | SellerId of string
        | IdType of IdType
        | IdList of string list
        | MWSAuthToken of string
        | Action of string
        | Version of string

    let unmapM args state = 
        let um = function
            | M.MarketplaceId m -> [("MarketplaceId", m)]
            | M.SellerId s -> [("SellerId", s)]
            | M.IdType t -> [("IdType", sprintf "%A" t)]
            | M.IdList ids -> ids |> Request.unmapList "ListId.Id"
            | M.MWSAuthToken t -> [("MWSAuthToken", t)]
            | M.Action a -> [("Action", a)]
            | M.Version v -> [("Version", v)]
        List.map um args

    let getMatchingProductForIdRequest marketplaceId sellerId idType idList mwsAuthToken =
        [M.MarketplaceId marketplaceId; M.SellerId sellerId; M.IdType idType; M.IdList idList; 
            M.MWSAuthToken mwsAuthToken; M.Action "GetMatchingProductForId"; M.Version SERVICE_VERSION]
(*
    // for additional improvements see: http://stackoverflow.com/questions/29545727/f-higher-order-functions-on-discrete-unions/29567324#29567324
    let argM = getMatchingProductForIdRequest "mpid" "sId" ASIN ["Key1"; "Key2"; "Key3"; "Key4"; "Key5"] "mwstoken"
    let argP = getCompetitivePricingForASINRequest "mpid" "sId" ["Key1"; "Key2"; "Key3"; "Key4"; "Key5"] "mwstoken"

    let test = unmapM argM []
    let test' = unmapP argP []
*)