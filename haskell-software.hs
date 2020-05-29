type Disease = (Name, Virus,Symptons,Quarantine)
type Patient = (Name,Symptons,Date)

type Name = String
type Virus = String
type Symptons = [String]
type Quarantine = String
type Date = String

list_diseases :: [Disease]
list_diseases = []

list_patients :: [Patient]
list_patients = []

insert_dise :: IO()
insert_dise = do putStr "Name: "
                 n <- getLine
                 putStr "Virus: "
                 c <- getLine
                 putStr "Symptons: "
                 d <- getLine
                 putStr "Quarantine: "
                 p <- getLine
                 appendFile "diseases.txt" (n ++ "\t" ++ c ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                 putStr "Insert another one?"
                 resp <- getLine
                 if (resp=="s" || resp=="S") then insert_dise else return()


insert_pati :: IO()
insert_pati = do putStr "Name: "
                 n <- getLine
                 putStr "Symptons: "
                 d <- getLine
                 putStr "Date: "
                 p <- getLine
                 appendFile "patients.txt" (n ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                 putStr "Insert another one?"
                 resp <- getLine
                 if (resp=="s" || resp=="S") then insert_pati else return()

main :: IO()
main = do putStr "1-Insert new Disease\n"
          putStr "2-Insert new Patient\n"
          putStr "Op: "
          resp <- getLine
          if (resp=="1") then do insert_dise
                              
          else if (resp=="2") then do insert_pati
                                 
          else error "Opcao nao encontrada"

          putStr "Deseja continuar (s/n)?"
          resp <- getLine
          if (resp=="s" || resp=="S") then main else return()