namespace dvMENTALmadness.amazon.client

// adapted from: http://viralfsharp.com/2012/03/17/retry-monad-an-implementation/
module Retry =
    open System.Threading
    open System

    type RetryParams = {
        maxRetries : int; waitBetweenRetries : int
        }

    let defaultRetryParams = {maxRetries = 3; waitBetweenRetries = 1000}

    type RetryMonad<'a> = RetryParams -> 'a
    let rm<'a> (f : RetryParams -> 'a) : RetryMonad<'a> = f

    let internal retryFunc<'a> (f : RetryMonad<'a>) =
        rm (fun retryParams -> 
            let rec execWithRetry f i e =
                match i with
                | n when n = retryParams.maxRetries -> raise e
                | _ -> 
                    try
                        f retryParams
                    with 
                    | e -> Thread.Sleep(retryParams.waitBetweenRetries); 
                            execWithRetry f (i + 1) e
            execWithRetry f 0 (Exception())
            ) 

    
    type RetryBuilder() =
        
        member this.Bind (p : RetryMonad<'a>, f : 'a -> RetryMonad<'b>)  =
            rm (fun retryParams -> 
                let value = retryFunc p retryParams
                f value retryParams                
            )

        member this.Return (x : 'a) = fun defaultRetryParams -> x

        member this.Run(m : RetryMonad<'a>) = m

        member this.Delay(f : unit -> RetryMonad<'a>) = f ()

        member this.ReturnFrom(x : 'a) = x

        member private this.TryFinally (computation, compensation) =
            fun retryParams ->
                try
                    this.ReturnFrom(computation retryParams)
                finally
                    compensation()

        member this.Using (disposable:#System.IDisposable, body) =
            let body' = fun retryParams -> body disposable
            rm (this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose()))

        member this.Zero() = this.Return

    let retry = RetryBuilder()

    let fn1 (x:float) (y:float) = rm (fun rp -> x * y)
    let fn2 (x:float) (y:float) = rm (fun rp -> if y = 0. then raise (invalidArg "y" "cannot be 0") else x / y)

    let retryParams = {maxRetries = 3; waitBetweenRetries = 100}

    let run =
        try
            let ym = 
                retry {
                    let! a = fn1 7. 5.
                    let! b = fn1 a a
                    let! c = fn2 b 0.
                    return c
                }
 
            sprintf "%A" (ym retryParams)
        with 
            e -> e.Message
        
    let r = run    

// consider posting on: http://codereview.stackexchange.com/
module MwsUtil =
    open System
    open System.Text
    open System.Collections.Generic
    open System.Security.Cryptography
    open log4net
    open dvMENTALmadness.Core.Logging

    let log = LogManager.GetLogger("MwsUtil")
    
    let debug msg = Printf.kprintf log.Debug
    let debugFormat format = Printf.kprintf log.DebugFormat format

    let cleanWS (str:string) =
        let data = StringBuilder()
        str.Trim() |> String.iteri (fun idx ch ->
                        match (idx, ch) with
                        | (0, c) -> data.Append(c) |> ignore
                        | (i, ' ') -> if data.[data.Length - 1] <> ' ' then data.Append(' ') |> ignore
                        | (_, c) -> data.Append(c) |> ignore
                    )
        data.ToString()
                    
    let caseInsensitiveReplace pat rep str =
        let data = StringBuilder()
        
        let rec replace (p:string) (r:string) (s:string) =
            let idx = s.IndexOf(p, StringComparison.InvariantCultureIgnoreCase)
            match idx with
            | -1 -> data.Append(s) |> ignore
            | 0 -> 
                data.Append(r) |> ignore
                s.Substring(p.Length) |> replace p r
            | i ->
                data.Append(s, 0, i)
                    .Append(r) |> ignore
                s.Substring(i + p.Length) |> replace p r

        str |> replace pat rep

        data.ToString()

    let escapeAppName (str:string) =
        StringBuilder(str)
            .Replace("\\", "\\\\")
            .Replace("/", "\\/")
            .ToString() |> cleanWS

    let escapeAppVersion (str:string) =
        StringBuilder(str)
            .Replace("\\", "\\\\")
            .Replace("(", "\\(")
            .ToString() |> cleanWS

    let escapeAttributeName (str:string) =
        StringBuilder(str)
            .Replace("\\", "\\\\")
            .Replace("=", "\\=")
            .ToString() |> cleanWS

    let escapeAttributeValue (str:string) = 
        StringBuilder(str)
            .Replace("\\", "\\\\")
            .Replace(";", "\\;")
            .Replace(")", "\\)")
            .ToString() |> cleanWS

(*
    HttpUtility.UrlEncode returns lower case values though it does not escape tilda
    Therefore using EscapeDataString since it encodes to Utf-8 and also returns escaped values in upper case, i.e., %3A vs %3a for :
    ARS only supports uppercase and RFC 3986 says it should be upper case.
    Highly unlikely but should the default encoding ever change, this will need change
*)
    let urlEncode value path =
        let result = StringBuilder(Uri.EscapeDataString(value) |> caseInsensitiveReplace "%7e" "~")
                        .Replace("*", "%2A")
                        .Replace("'", "%27")
                        .Replace("!", "%21")
        
        if path then 
            result.ToString() |> caseInsensitiveReplace "%2f" "/"
        else
            result.ToString()

    let (|UriSchemeAndPort|) (u:Uri) = (u.Scheme,u.Port)

    let calculateStringToSignV2 (serviceUri:Uri) parameters = 
        let data = StringBuilder()
        
        data.Append("POST\n")
            .Append(serviceUri.Host.ToLower()) |> ignore
        
        match serviceUri with
        | UriSchemeAndPort("https",443) -> data.Append("\n") |> ignore
        | UriSchemeAndPort("http",80) -> data.Append("\n") |> ignore
        | UriSchemeAndPort(_,port) -> data.AppendFormat(":{0}\n", port) |> ignore

        let startIdx = data.Length
        let rec buildParams p =
            match p with
            | [] -> data |> ignore
            | head::tail -> 
                match head with
                | (k, v) ->
                    if data.Length > startIdx then
                        data.Append("&") |> ignore
                    data.Append(urlEncode k false)
                        .Append("=")
                        .Append(urlEncode v false) |> ignore
                    buildParams tail

        // pass in sorted parameters        
        buildParams (parameters |> List.sortBy (fst))

        data.ToString()

    let sign (data:string) (key:string) (algorithm:string) =
        try
            let encoding = Encoding.UTF8;
            let alg = KeyedHashAlgorithm.Create(algorithm.ToUpper()) : KeyedHashAlgorithm
            alg.Key <- encoding.GetBytes(key)
            Convert.ToBase64String(alg.ComputeHash(encoding.GetBytes(data.ToCharArray())))
        with
        | ex -> ( 
                error log ex "An error occured signing request"
                reraise() 
            )

        (*
         Computes RFC 2104-compliant HMAC signature for request parameters
         Implements AWS Signature, as per following spec:
         
         If Signature Version is 0, it signs concatenated Action and Timestamp
         
         If Signature Version is 1, it performs the following:
         
         Sorts all parameters (including SignatureVersion and excluding Signature,
         the value of which is being created), ignoring case.
         
         Iterate over the sorted list and append the parameter name (in original
         case) and then its value. It will not URL-encode the parameter values
         before constructing this string. There are no separators.
         
         If Signature Version is 2, string to sign is based on following:
         
         1. The HTTP Request Method followed by an ASCII newline (%0A) 
         2. The HTTP Host header in the form of lowercase host, followed by an ASCII newline.
         3. The URL encoded HTTP absolute path component of the URI (up to but not
         including the query string parameters); if this is empty use a forward
         '/'. This parameter is followed by an ASCII newline. 
         4. The concatenation of all query string components (names and values) 
         as UTF-8 characters which are URL encoded as per RFC 3986 (hex characters 
         MUST be uppercase), sorted using lexicographic byte ordering. Parameter names 
         are separated from their values by the '=' character (ASCII character 61), even if the
         value is empty. Pairs of parameter and values are separated by the '&'
         character (ASCII code 38).
         
         @param serviceUri
                      Including host, port, api name, and api version
         @param parameters
         @param signatureVersion
         @param signatureMethod
         @param awsSecretKey
         
         @return The base64 encoding of the signature.
         *)
    let signParameters (serviceUri:Uri) (signatureVersion:string) (signatureMethod:string) parameters (awsSecretKey:string) =
        let defaultAlgorithm = "HmacSHA1"

        let defaultParams = ("SignatureVersion", signatureVersion)::parameters

        let stringToSign = 
            match signatureVersion with
            //TODO: implement v0, v1 signatures
            | "0" -> None, defaultAlgorithm
            | "1" -> None, defaultAlgorithm
            | "2" -> 
                let v2Parameters = ("SignatureMethod", signatureMethod)::defaultParams
                Some(calculateStringToSignV2 serviceUri v2Parameters), signatureMethod
            | _ -> None, defaultAlgorithm

        match stringToSign with
        | Some(x), alg -> Some(sign x awsSecretKey alg)
        | _, _ -> None
