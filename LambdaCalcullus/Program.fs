// Learn more about F# at http://fsharp.org
open Domain
open SamplePayrollScheme
open NBB.Core.Effects.FSharp

let mainEff _argv = 
    effect {
        let! result = eval salariuNet (ContractId 1) (YearMonth (2020, 7))
        printf "salariuNet = %A" result
        return 0
    }

[<EntryPoint>]
let main argv =
    let interpreter = Interpreter.createInterpreter ()
    mainEff argv
        |> Effect.interpret interpreter
        |> Async.RunSynchronously


