module Combinators

open Core
open System
open NBB.Core.Effects.FSharp


[<AutoOpen>]
module DecimalAlgebra = 
    let (+) = PayrollElem.lift2 (fun (a:decimal) b -> a + b)
    let (-) = PayrollElem.lift2 (fun (a:decimal) b -> a - b)
    let (*) = PayrollElem.lift2 (fun (a:decimal) b -> a * b)
    let (/) = PayrollElem.lift2 (fun (a:decimal) b -> a / b)
    
    let ceiling = PayrollElem.map (fun (a:decimal) -> Math.Ceiling a)
    let sum (xs:PayrollElem<decimal list>)=  xs |> PayrollElem.map List.sum


[<AutoOpen>]
module BooleanAlgebra = 
    let When (cond:PayrollElem<bool>) (e1:PayrollElem<'a>) (e2:PayrollElem<'a>) = 
        elem {
            let! cond' = cond
            if cond' then return! e1 else return! e2
        }

[<AutoOpen>]
module HrAlgebra =
    let lastMonth (elem: PayrollElem<'a>): PayrollElem<'a> = 
        fun contractId yearMonth -> elem contractId (YearMonth.lastMonth yearMonth)

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



