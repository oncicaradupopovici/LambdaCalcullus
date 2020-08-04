module HrAdmin

open Core
open NBB.Core.Effects.FSharp
open System.Collections


type HrInfo = {
    salariuBrut: decimal
    esteContractPrincipal: bool
}

type HrDb = Generic.IDictionary<YearMonth,Generic.IDictionary<ContractId, HrInfo>>

let hrDb = dict [
    YearMonth(2020, 7), dict [
        ContractId 1, {salariuBrut=5000.00m; esteContractPrincipal=true}
        ContractId 11, {salariuBrut=1000.00m; esteContractPrincipal=false}
        ContractId 101, {salariuBrut=2000.00m; esteContractPrincipal=false}
    ]
    YearMonth(2020, 6), dict [
        ContractId 1, {salariuBrut=4000.00m; esteContractPrincipal=true}
        ContractId 11, {salariuBrut=1000.00m; esteContractPrincipal=false}
        ContractId 101, {salariuBrut=2000.00m; esteContractPrincipal=false}
    ]
]

let readFromDb<'a> (code:string) : PayrollElem<'a> =
    let cast (value:'b) = 
        if typeof<'a> = typeof<'b> then Ok (box value :?> 'a) else Error "Invalid elem type"

    fun contractId yearMonth ->
        effect {
            let hrInfo = (hrDb.Item yearMonth).Item contractId
            match code with
            | "salariuBrut" -> return cast hrInfo.salariuBrut
            | "esteContractPrincipal" -> return cast hrInfo.esteContractPrincipal
            | _ -> return Error "Elem not found"
        }

let getOtherContracts (ContractId contractId): Effect<ContractId list> = 
    effect {
        return [
            ContractId (contractId + 10)
            ContractId (contractId + 100)
        ]
    }

let getAllContracts (ContractId contractId): Effect<ContractId list> = 
    effect {
        return [
            ContractId contractId
            ContractId (contractId + 10)
            ContractId (contractId + 100)
        ]
    }

