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

let ReportResults (reporter:IReportResults) =
    Is (fun expected actual -> expected.Equals actual) 10 10
    |> ToResultString
    |> reporter.Report

[<EntryPoint>]
let main argv =
    new ConsoleReporter()
    |> ReportResults 
    0
