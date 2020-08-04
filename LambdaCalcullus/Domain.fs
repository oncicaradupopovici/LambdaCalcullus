module Domain

open NBB.Core.Effects.FSharp
open System

type ContractId = | ContractId of int
type YearMonth = | YearMonth of year:int * month:int

type PayrollElemResult<'a> = Effect<Result<'a, string>>
type PayrollElem<'a> = ContractId -> YearMonth -> PayrollElemResult<'a>

module YearMonth = 
 let lastMonth (YearMonth (year, month)) = YearMonth (year, month-1)

module PayrollElemResult = 
    let map (func: 'a -> 'b) (eff: PayrollElemResult<'a>) : PayrollElemResult<'b> = 
        eff |> Effect.map (fun result -> result |> Result.map func)

    let bind (func: 'a -> PayrollElemResult<'b>) (eff: PayrollElemResult<'a>): PayrollElemResult<'b> = 
        effect {
            let! r = eff
            match r with
                | Ok x -> return! func x
                | Error err -> return Error err
        }

    let apply (func:PayrollElemResult<'a->'b>) (eff:PayrollElemResult<'a>) = 
        bind (fun a -> func |> map (fun fn -> fn a)) eff

    let return' x = x |> Ok |> Effect.pure'

    let composeK f g x = bind g (f x)

    let lift2 f = map f >> apply

    let flatten eff = bind id eff


module PayrollElem =
    let map (func: 'a -> 'b) (eff: PayrollElem<'a>) : PayrollElem<'b> = 
        fun contractId yearMonth ->
            eff contractId yearMonth |> PayrollElemResult.map func

    let bind (func: 'a -> PayrollElem<'b>) (eff: PayrollElem<'a>): PayrollElem<'b> = 
        fun contractId yearMonth ->
            eff contractId yearMonth |> PayrollElemResult.bind (fun a -> func a contractId yearMonth)

    let apply (func:PayrollElem<'a->'b>) (eff:PayrollElem<'a>) = 
        bind (fun a -> func |> map (fun fn -> fn a)) eff

    let return' (x:'a): PayrollElem<'a> =
        fun _contractId _yearMonth -> PayrollElemResult.return' x

    let composeK f g x = bind g (f x)
    
    let lift2 f = map f >> apply
    
    let flatten eff = bind id eff

module PayrollElemBuilder =
    type PayrollElemBuilder() =
        member _.Bind(eff, func) = PayrollElem.bind func eff
        member _.Return(value) = PayrollElem.return' value
        member _.ReturnFrom(value) = value
        member _.Combine(eff1, eff2) = PayrollElem.bind (fun _ -> eff2) eff1
        member _.Zero() = PayrollElem.return' ()

[<AutoOpen>]
module PayrollElems =
    let elem = new PayrollElemBuilder.PayrollElemBuilder()

    let (<!>) = PayrollElem.map
    let (<*>) = PayrollElem.apply
    let (>>=) eff func = PayrollElem.bind func eff
    let (>=>) = PayrollElem.composeK


module HrAdmin = 
    let readFromDb (_code:string) : PayrollElem<'a> = 
        fun _contractId _yearMonth ->
            effect {
                return Error "Not implemented yet"
            }

    let getOtherContracts (ContractId contractId): Effect<ContractId list> = 
        effect {
            return [
                ContractId contractId
                ContractId (contractId + 10)
                ContractId (contractId + 100)
            ]
        }

[<AutoOpen>]
module DecimalPayrollElems = 
    let (*) = PayrollElem.lift2 (fun (a:decimal) b -> a * b)
    let (+) = PayrollElem.lift2 (fun (a:decimal) b -> a + b)
    let (-) = PayrollElem.lift2 (fun (a:decimal) b -> a - b)

    let ceiling = PayrollElem.map (fun (a:decimal) -> Math.Ceiling a)


[<AutoOpen>]
module BooleanPayrollElems = 
    let When (cond:PayrollElem<bool>) (e1:PayrollElem<'a>) (e2:PayrollElem<'a>) = 
        elem {
            let! cond' = cond
            if cond' then return! e1 else return! e2
        }


[<RequireQualifiedAccess>]
module List =
    let traversePayrollElem f list =
        let cons head tail = head :: tail      
        let initState = PayrollElem.return' []
        let folder head tail = PayrollElem.return' cons <*> (f head) <*> tail
        List.foldBack folder list initState

    let sequencePayrollElem list = traversePayrollElem id list

    let traversePayrollElemResult f list =
        let (<*>) = PayrollElemResult.apply
        let cons head tail = head :: tail      
        let initState = PayrollElemResult.return' []
        let folder head tail = PayrollElemResult.return' cons <*> (f head) <*> tail
        List.foldBack folder list initState

    let sequencePayrollElemResult list = traversePayrollElemResult id list



[<AutoOpen>]
module PayrollFns =
    let lastMonth (elem: PayrollElem<'a>): PayrollElem<'a> = 
        fun contractId yearMonth -> elem contractId (YearMonth.lastMonth yearMonth)

    let otherContracts (elem: PayrollElem<'a>): PayrollElem<'a list> =
        fun contractId yearMonth ->
            effect {
                let! otherContracts = HrAdmin.getOtherContracts contractId
                let otherContractsElemResults = 
                    otherContracts 
                    |> List.map (fun otherContractId -> elem contractId yearMonth)
                    |> List.sequencePayrollElemResult

                return! otherContractsElemResults
            }


let eval (elem:PayrollElem<'a>) contractId yearMonth = 
    effect {
        let! result =  elem contractId yearMonth
        match result with
        | Ok a -> return sprintf "%A" a
        | Error err -> return sprintf "Eroare: %s" err
    }
    













