type Result = PASS | FAIL | EXCEPTION

let Is predicate expected actual =
    try
        match predicate expected actual with
        | true  -> PASS
        | false -> FAIL
    with
    | Failure _ -> EXCEPTION

let ToResultString = function
    | PASS  -> "PASS"
    | FAIL -> "FAIL"
    | EXCEPTION -> "UNHANDLED EXCEPTION"

type IReportResults =
    abstract member Report: string -> unit

type ConsoleReporter() =

    interface IReportResults with
        member this.Report result = printfn "%s" result

let ReportResults (reporter:IReportResults) expected actual =
    Is (fun expected actual -> expected.Equals actual) expected actual
    |> ToResultString
    |> reporter.Report

[<EntryPoint>]
let main argv =
    let areEqual = new ConsoleReporter() |> ReportResults
    areEqual 10 10
    areEqual true true
    areEqual "some" "fail"
    0
