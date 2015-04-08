namespace dvMENTALmadness.AmazonMwsProxy.Tests

module MwsUtilFixture =
    open Xunit
    open Swensen.Unquote

    [<Fact>]
    let ``Test Name`` () =
        let expected = "a"
        let actual = "a"
        test <@ expected = actual @>
