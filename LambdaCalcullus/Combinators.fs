module Combinators

open Core
open System
open NBB.Core.Effects.FSharp

[<AutoOpen>]
module HrCombinators =
    let lastMonth (elem: PayrollElem<'a>): PayrollElem<'a> = 
        fun contractId yearMonth -> elem contractId (YearMonth.lastMonth yearMonth)

    let nMonthsAgo (n:int) (elem: PayrollElem<'a>): PayrollElem<'a> = 
        fun contractId yearMonth -> elem contractId (YearMonth.subStractMonth n yearMonth)

    let last_N_Months (n:int) (elem: PayrollElem<'a>): PayrollElem<'a list> = 
        fun contractId yearMonth -> 
            [0..n-1] 
            |> List.map (fun x -> YearMonth.subStractMonth x yearMonth)
            |> List.map (fun ym -> elem contractId ym)
            |> List.sequencePayrollElemResult
        

    let otherContracts (elem: PayrollElem<'a>): PayrollElem<'a list> =
        fun contractId yearMonth ->
            effect {
                let! otherContracts = HrAdmin.getOtherContracts contractId
                let otherContractsElemResults = 
                    otherContracts 
                    |> List.map (fun otherContractId -> elem otherContractId yearMonth)
                    |> List.sequencePayrollElemResult

                return! otherContractsElemResults
            }

    let allContracts (elem: PayrollElem<'a>): PayrollElem<'a list> =
        fun contractId yearMonth ->
            effect {
                let! otherContracts = HrAdmin.getAllContracts contractId
                let otherContractsElemResults = 
                    otherContracts 
                    |> List.map (fun otherContractId -> elem otherContractId yearMonth)
                    |> List.sequencePayrollElemResult

                return! otherContractsElemResults
            }

[<AutoOpen>]
module DecimalCombinators = 
    let (+) = PayrollElem.lift2 (fun (a:decimal) b -> a + b)
    let (-) = PayrollElem.lift2 (fun (a:decimal) b -> a - b)
    let (*) = PayrollElem.lift2 (fun (a:decimal) b -> a * b)
    let (/) = PayrollElem.lift2 (fun (a:decimal) b -> a / b)
    
    let ceiling = PayrollElem.map (fun (a:decimal) -> Math.Ceiling a)
    let sum (xs:PayrollElem<decimal list>)=  xs |> PayrollElem.map List.sum
    let avg (xs:PayrollElem<decimal list>)=  xs |> PayrollElem.map List.average


[<AutoOpen>]
module BooleanCombinators = 
    let When (cond:PayrollElem<bool>) (e1:PayrollElem<'a>) (e2:PayrollElem<'a>) = 
        elem {
            let! cond' = cond
            if cond' then return! e1 else return! e2
        }

    let all (xs:PayrollElem<bool list>) = xs |> PayrollElem.map (List.reduce (&&))
    let any (xs:PayrollElem<bool list>) = xs |> PayrollElem.map (List.reduce (||))

    let (&&) = PayrollElem.lift2 (&&)
    let (||) = PayrollElem.lift2 (||)
    let (not) = PayrollElem.map (fun (x:bool) -> not x)

[<AutoOpen>]
module UtilityCombinators = 
    let log elemCode (elem:PayrollElem<'a>) = 
        fun (ContractId contractId) (YearMonth (year, month)) ->
            effect {
                printfn "*** LOG *** evaluating elem %A on contractId:%A year:%A month:%A " elemCode contractId year month
                return! elem (ContractId contractId) (YearMonth (year, month))
            }

    let memoize (elem:PayrollElem<'a>) = 
        fun (ContractId contractId) (YearMonth (year, month)) ->
            
            effect {
                let! cachedValue = ElemCache.get elem (ContractId contractId) (YearMonth (year, month))
                if cachedValue.IsSome 
                then 
                    return cachedValue.Value
                else 
                    let! value = elem (ContractId contractId) (YearMonth (year, month))
                    do! ElemCache.set elem (ContractId contractId) (YearMonth (year, month)) value
                    return value
            }
    





