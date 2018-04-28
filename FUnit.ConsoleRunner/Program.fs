type Result = Pass | Fail | Exception

let should predicate expected actual =
    try
        match predicate expected actual with
        | true -> Pass
        | false -> Fail
    with
    | Failure _ -> Exception

let equal expected =
    expected.Equals

let ``This is an example of a test`` () =
    10 |> should equal 10

let ``This is another test`` () =
    "string" |> should equal "string"

let Find _ =
    seq {
        yield ``This is an example of a test``
        yield ``This is another test``
    }

let Execute tests =
    let ExecuteTest test =
        match test () with
        | Pass  -> "PASS"
        | Fail -> "FAIL"
        | Exception -> "UNHANDLED EXCEPTION"

    Seq.map ExecuteTest tests

type IReportResults =
    abstract member Report: string -> unit

type ConsoleReporter () =

    interface IReportResults with
        member this.Report result =
            printfn "%s" result

let Report =
    let reporter = new ConsoleReporter () :> IReportResults
    Seq.iter reporter.Report

[<EntryPoint>]
let main argv =
    Find argv |> Execute |> Report
    0