type Disease = (Name, Virus,Symptons,Quarantine)
type Patient = (Name,Symptons,Date)

type Name = String
type Virus = String
type Symptons = [String]
type Quarantine = String
type Date = String

list_diseases :: [Disease]
list_diseases = [("hepatiteA", "picorna",["icterıcia","fadiga","febre","mialgia"], "no"),
                ("covid_19","corona", ["tosse","fadiga","febre","dispneia"], "yes"),
                ("sarampo", "paramyxo",["manchas","erup¸c~oes", "tosse", "febre"], "yes")]

list_patients :: [Patient]
list_patients = [("Joao",["tosse", "dispneia"],"2020,4,2"),
                ("Ana",["icter´ıcia","mialgia"],"2020,4,8")]

main =
    print(list_diseases)