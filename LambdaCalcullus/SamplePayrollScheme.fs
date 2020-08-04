module SamplePayrollScheme

open Domain

//HrAdmin elems
let salariuBrut: PayrollElem<decimal> = HrAdmin.readFromDb "salariuBrut"
let procentImpozit: PayrollElem<decimal> = HrAdmin.readFromDb "procentImpozit"
let esteContractPrincipal: PayrollElem<bool> = HrAdmin.readFromDb "esteContractPrincipal"


//Formula elems
let impozitNerotunjit = procentImpozit * salariuBrut
let impoziteleNerotunjiteAleCelorlalteContracte = impozitNerotunjit |> otherContracts
let sumaImpozitelorNerotunjiteAleCelorlalteContracte = List.sum <!> impoziteleNerotunjiteAleCelorlalteContracte
let impozit = 
    When esteContractPrincipal
        (ceiling (impozitNerotunjit + sumaImpozitelorNerotunjiteAleCelorlalteContracte) - sumaImpozitelorNerotunjiteAleCelorlalteContracte)
        impozitNerotunjit


let salariuNet = salariuBrut - impozit
let diferentaNetFataDeLunaTrecuta = (salariuNet |> lastMonth) - salariuNet




