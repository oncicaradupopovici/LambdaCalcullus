module Combinators

open Core
open System
open NBB.Core.Effects.FSharp

[<AutoOpen>]
module HrAlgebra =
    let lastMonth (elem: PayrollElem<'a>): PayrollElem<'a> = 
        fun contractId yearMonth -> elem contractId (YearMonth.lastMonth yearMonth)

    let last_N_Months (n:int)(elem: PayrollElem<'a>): PayrollElem<'a list> = 
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
module DecimalAlgebra = 
    let (+) = PayrollElem.lift2 (fun (a:decimal) b -> a + b)
    let (-) = PayrollElem.lift2 (fun (a:decimal) b -> a - b)
    let (*) = PayrollElem.lift2 (fun (a:decimal) b -> a * b)
    let (/) = PayrollElem.lift2 (fun (a:decimal) b -> a / b)
    
    let ceiling = PayrollElem.map (fun (a:decimal) -> Math.Ceiling a)
    let sum (xs:PayrollElem<decimal list>)=  xs |> PayrollElem.map List.sum
    let avg (xs:PayrollElem<decimal list>)=  xs |> PayrollElem.map List.average


[<AutoOpen>]
module BooleanAlgebra = 
    let When (cond:PayrollElem<bool>) (e1:PayrollElem<'a>) (e2:PayrollElem<'a>) = 
        elem {
            let! cond' = cond
            if cond' then return! e1 else return! e2
        }





