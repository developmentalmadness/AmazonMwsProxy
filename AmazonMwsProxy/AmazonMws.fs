namespace dvMENTALmadness.amazon.client

module AmazonMws = 
    open System
    open System.Collections.Generic

    type ResponseHeaderMetadata = {
        RequestId: string
        ResponseContext: string
        Timestamp: string
        QuotaMax: int option
        QuotaRemaining: int option
        QuotaResetsAt: DateTime option
    }

    type ProxyConfig = {
        ServicePath: Uri
        ServiceVersion: string
        ApplicationName: string
        ApplicationVersion: string
        LibraryVersion: string
        UserAgent: string
        AwsAccessKeyId: string
        AwsSecretKeyId: string
        SignatureVersion: string
        SignatureMethod: string
        ConnectionTimeout: int
        MaxErrorRetry: int
        ProxyHost: string option
        ProxyPort: string option
        ProxyUsername: string option
        ProxyPassword: string option
    }

    type MwsRequest =
        { OperationName: string }  

    let getProxyConfig endpoint applicationName applicationVersion awsAccessKeyId awsSecretKeyId (servicePath:string) serviceVersion = 
        // TODO: create a nested function that takes a tuple (string * string) list parameter of user-agent strings to append in the format "(key=value;key=value)"
        { 
            SignatureVersion = "2";
            SignatureMethod = "HmacSHA256";
            ConnectionTimeout = 50000;
            MaxErrorRetry = 3;
            LibraryVersion = "2015-04-06";
            ApplicationName = applicationName;
            ApplicationVersion = applicationVersion;
            AwsAccessKeyId = awsAccessKeyId;
            AwsSecretKeyId = awsSecretKeyId;
            UserAgent = sprintf "%s/%s (Language=F#)" applicationName applicationVersion;
            ServicePath = Uri(Uri(endpoint), servicePath);
            ServiceVersion = serviceVersion;
            ProxyHost = None;
            ProxyPort = None;
            ProxyUsername = None;
            ProxyPassword = None;
          }

    let getProductsProxyConfig endpoint applicationName applicationVersion awsAccessKeyId awsSecretKeyId =
        getProxyConfig endpoint applicationName applicationVersion awsAccessKeyId awsSecretKeyId "Products/2011-10-01" "2011-10-01"

    let getOrdersProxyConfig endpoint applicationName applicationVersion awsAccessKeyId awsSecretKeyId =
        getProxyConfig endpoint applicationName applicationVersion awsAccessKeyId awsSecretKeyId "Orders/2013-09-01" "2013-09-01"

    let getSellersProxyConfig endpoint applicationName applicationVersion awsAccessKeyId awsSecretKeyId =
        getProxyConfig endpoint applicationName applicationVersion awsAccessKeyId awsSecretKeyId "Sellers/2011-07-01" "2011-07-01"
