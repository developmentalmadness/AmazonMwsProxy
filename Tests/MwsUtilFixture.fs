namespace dvMENTALmadness.AmazonMwsProxy.Tests

module MwsUtilFixture =
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

