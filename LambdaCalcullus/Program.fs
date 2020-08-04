// Learn more about F# at http://fsharp.org
open Core
open SamplePayrollScheme
open NBB.Core.Effects.FSharp

let mainEff _argv = 
    effect {
        let! impoziteleNerotunjitePeToateContractele = eval impoziteleNerotunjitePeToateContractele (ContractId 1) (YearMonth (2020, 7))
        printfn "impoziteleNerotunjitePeToateContractele = %A" impoziteleNerotunjitePeToateContractele

        let! impozitelePeToateContractele = eval impozitelePeToateContractele (ContractId 1) (YearMonth (2020, 7))
        printfn "impozitelePeToateContractele = %A" impozitelePeToateContractele

        let! sumaImpozitelorPeToateContractele = eval sumaImpozitelorPeToateContractele (ContractId 1) (YearMonth (2020, 7))
        printfn "sumaImpozitelorPeToateContractele = %A" sumaImpozitelorPeToateContractele

        let! salariuNet = eval salariuNet (ContractId 1) (YearMonth (2020, 7))
        printfn "salariuNet = %A" salariuNet

        let! diferentaNetFataDeLunaTrecuta = eval diferentaNetFataDeLunaTrecuta (ContractId 1) (YearMonth (2020, 7))
        printfn "diferentaNetFataDeLunaTrecuta = %A" diferentaNetFataDeLunaTrecuta


        return 0
    }

[<EntryPoint>]
let main argv =
    let interpreter = Interpreter.createInterpreter ()
    mainEff argv
        |> Effect.interpret interpreter
        |> Async.RunSynchronously


