namespace dvMENTALmadness.AmazonMwsProxy.Tests

module MwsUtilFixture =
    open System
    open Xunit
    open Swensen.Unquote
    open dvMENTALmadness.amazon.client.MwsUtil

    //*****************************
    //     escapAppName
    //*****************************

    [<Fact>]
    let ``escapeAppName escapes backslash`` () =
        let target = "before\\middle\\after"
        let actual = target |> escapeAppName
        test <@ "before\\\\middle\\\\after" = actual @>

    [<Fact>]
    let ``escapeAppName escapes forward slash`` () =
        let target = "before/middle/after"
        let actual = target |> escapeAppName
        test <@ "before\\/middle\\/after" = actual @>

    [<Theory>]
    [<InlineData(" 1  2 3    4           5              ", "1 2 3 4 5")>]
    [<InlineData("1 2 3 4 5", "1 2 3 4 5")>]
    [<InlineData(" 1 2 3 4 5 ", "1 2 3 4 5")>]
    let ``escapeAppName calls trim and removes duplicate spaces`` (target:string) (expected:string) =
        let actual = target |> escapeAppVersion
        test <@ expected = actual @>

    //*****************************
    //     escapAppVersion
    //*****************************

    [<Fact>]
    let ``escapeAppVersion escapes backslash`` () =
        let target = "before\\middle\\after"
        let actual = target |> escapeAppName
        test <@ "before\\\\middle\\\\after" = actual @>

    [<Fact>]
    let ``escapeAppVersion escapes open paren`` () =
        let target = "before/middle/after ("
        let actual = target |> escapeAppVersion
        test <@ "before/middle/after \\(" = actual @>

    [<Theory>]
    [<InlineData(" 1  2 3    4           5              ", "1 2 3 4 5")>]
    [<InlineData("1 2 3 4 5", "1 2 3 4 5")>]
    [<InlineData(" 1 2 3 4 5 ", "1 2 3 4 5")>]
    let ``escapeAppVersion calls trim and removes duplicate spaces`` (target:string) (expected:string) =
        let actual = target |> escapeAppVersion
        test <@ expected = actual @>

    //*****************************
    //     escapeAttributeName
    //*****************************

    [<Fact>]
    let ``escapeAttributeName escapes backslash`` () =
        let target = "before\\middle\\after"
        let actual = target |> escapeAttributeName
        test <@ "before\\\\middle\\\\after" = actual @>

    [<Fact>]
    let ``escapeAttributeName escapes equals`` () =
        let target = "before/middle/after="
        let actual = target |> escapeAttributeName
        test <@ "before/middle/after\\=" = actual @>

    [<Theory>]
    [<InlineData(" 1  2 3    4           5              ", "1 2 3 4 5")>]
    [<InlineData("1 2 3 4 5", "1 2 3 4 5")>]
    [<InlineData(" 1 2 3 4 5 ", "1 2 3 4 5")>]
    let ``escapeAttributeName calls trim and removes duplicate spaces`` (target:string) (expected:string) =
        let actual = target |> escapeAttributeName
        test <@ expected = actual @>

    //*****************************
    //     escapeAttributeValue
    //*****************************

    [<Fact>]
    let ``escapeAttributeValue escapes backslash`` () =
        let target = "before\\middle\\after"
        let actual = target |> escapeAttributeValue
        test <@ "before\\\\middle\\\\after" = actual @>

    [<Fact>]
    let ``escapeAttributeValue escapes semicolon`` () =
        let target = "before/middle/after;"
        let actual = target |> escapeAttributeValue
        test <@ "before/middle/after\\;" = actual @>

    [<Fact>]
    let ``escapeAttributeValue escapes close paren`` () =
        let target = "before/middle/after)"
        let actual = target |> escapeAttributeValue
        test <@ "before/middle/after\\)" = actual @>

    [<Theory>]
    [<InlineData(" 1  2 3    4           5              ", "1 2 3 4 5")>]
    [<InlineData("1 2 3 4 5", "1 2 3 4 5")>]
    [<InlineData(" 1 2 3 4 5 ", "1 2 3 4 5")>]
    let ``escapeAttributeValue calls trim and removes duplicate spaces`` (target:string) (expected:string) =
        let actual = target |> escapeAttributeValue
        test <@ expected = actual @>

    //*****************************
    //     urlEncode
    //*****************************
    
    [<Theory>]
    [<InlineData("http://www.amazon.com/?wierdStuff=~*'!", "http%3A%2F%2Fwww.amazon.com%2F%3FwierdStuff%3D~%2A%27%21")>]
    let ``urlEncode parsing matches aws library`` (target:string) (expected:string) =
        let actual = urlEncode target false
        test <@ expected = actual @>
    
    [<Theory>]
    [<InlineData("http://www.amazon.com/?wierdStuff=~*'!", "http%3A//www.amazon.com/%3FwierdStuff%3D~%2A%27%21")>]
    let ``urlEncode path parsing matches aws library`` (target:string) (expected:string) =
        let actual = urlEncode target true
        test <@ expected = actual @>

    //*****************************
    //   signParameters
    //*****************************

    let ``signParameters creates API V2 string`` () =
        let uri = Uri("https://mws.amazonservices.com/Products/2011-10-01")
        let version = "2"
        let signatureMethod = "HmacSHA256"
        let secret = "foo_Bbar+baZToxeo9EYlwsmUa0EZTosQp1MJ7-XK"
        let actual = signParameters uri 
                                    version
                                    signatureMethod 
                                    [("AWSAccessKeyId","A1B2C3D4E5F6G7H8I9J0"); 
                                    ("MarketplaceId","ATVPDKIKX0DER"); 
                                    ("SellerId","A1B2C3D4E5F6G"); 
                                    ("Timestamp","2015-04-09T00:07:57Z"); 
                                    ("Action","GetMatchingProductForId"); 
                                    ("IdList.Id.1","B000HDP3MA"); 
                                    ("IdList.Id.2","B000X63K7G"); 
                                    ("IdList.Id.3","B009VGDVYQ"); 
                                    ("IdList.Id.4","B00LIP5MEK"); 
                                    ("IdList.Id.5","B004EY27X6"); 
                                    ("IdType","ASIN"); 
                                    ("MWSAuthToken","A1B2C3D4E5F6G7H8I9J0"); 
                                    ("Version","2011-10-01"); ]
                                    secret

        test <@ "cEwITdwUQQTpJ4xDx4Ud9Hx2iCF4d094LFcdZffT1Jw=" = actual.Value @>
