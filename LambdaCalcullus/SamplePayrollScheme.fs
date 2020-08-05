﻿module SamplePayrollScheme

open Core
open Combinators

//HrAdmin elems
let salariuBrut = HrAdmin.readFromDb<decimal> "salariuBrut"
let esteContractPrincipal = HrAdmin.readFromDb<bool> "esteContractPrincipal"

//payroll constants
let procentImpozit = Payroll.constant 0.23456m

//Formula elems
let impozitNerotunjit = procentImpozit * salariuBrut
let sumaImpozitelorNerotunjitePeToateContractele = impozitNerotunjit |> allContracts |> sum
let sumaImpozitelorNerotunjitePeContracteleSecundare = 
    (When esteContractPrincipal (constant 0m) impozitNerotunjit) |> allContracts |> sum
let impozit = 
    When esteContractPrincipal
        (ceiling sumaImpozitelorNerotunjitePeToateContractele - sumaImpozitelorNerotunjitePeContracteleSecundare)
        impozitNerotunjit

let impoziteleNerotunjitePeToateContractele = impozitNerotunjit |> allContracts
let impozitelePeToateContractele = impozit |> allContracts
let sumaImpozitelorPeToateContractele = impozit |> allContracts |> sum



let salariuNet = salariuBrut - impozit
let diferentaNetFataDeLunaTrecuta = salariuNet - (salariuNet |> lastMonth)
let mediaSalariuluiNetPeUltimele3Luni = salariuNet |> last_N_Months 3 |> avg




