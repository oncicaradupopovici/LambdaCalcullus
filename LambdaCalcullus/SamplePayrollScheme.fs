module SamplePayrollScheme

open Core
open Combinators

//HrAdmin elems
let salariuBrut = HrAdmin.readFromDb<decimal> "salariuBrut"
let esteContractPrincipal = HrAdmin.readFromDb<bool> "esteContractPrincipal"
let esteActiv = HrAdmin.readFromDb<bool> "esteActiv"


//payroll constants
let procentImpozit = Payroll.constant 0.23456m

//Formula elems
let nuEsteActiv = not esteActiv
let esteContractPrincipalSiEsteActiv = esteContractPrincipal && esteActiv
let esteContractPrincipalSiNuEsteActiv = esteContractPrincipal && not esteActiv
let esteContractPrincipalSiEsteActivLunaTrecuta = esteContractPrincipal && esteActiv |> lastMonth
let esteContractPrincipalSiEsteActivAcum2Luni = esteContractPrincipal && esteActiv |> lastMonth |> lastMonth
let esteContractPrincipalSiNuEsteActivAcum2Luni = esteContractPrincipal && not esteActiv |> lastMonth |> lastMonth
let esteContractPrincipalSiNuEsteActivAcum3Luni = esteContractPrincipal && not esteActiv |> nMonthsAgo 3
let esteContractPrincipalSiAreToateContracteleActive = esteContractPrincipal && esteActiv |> allContracts |> all
let esteContractPrincipalSiAreVreunContractInactivLunaTrecuta = esteContractPrincipal && not esteActiv |> lastMonth |> allContracts |> any
let esteActivUltimele3Luni = esteActiv |> last_N_Months 3 |> all



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
let sumaImpozitelorPeToateContractele' = sum (allContracts impozit)



let salariuNet = salariuBrut - impozit
let diferentaNetFataDeLunaTrecuta = salariuNet - (salariuNet |> lastMonth)
let mediaSalariuluiNetPeUltimele3Luni = salariuNet |> last_N_Months 3 |> avg




