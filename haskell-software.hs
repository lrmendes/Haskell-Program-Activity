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
                 putStr "Insert another one? : "
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
                 putStr "Insert another one? : "
                 resp <- getLine
                 if (resp=="s" || resp=="S") then insert_pati else return()

loadTab_patients = do s <-readFile "patients.txt"
                      return (gerlist_pat (map words (lines s)))

gerlist_pat [] = []
gerlist_pat ([n,d,p]:xs) = (n,d,p):(gerlist_pat xs)

print_lst_pat [] =""
print_lst_pat ((n,d,p):xs) = "Patient- name=" ++ n ++ ", Symptons= " ++ d ++ ", date= " ++ p ++ "\n" ++ (print_lst_pat xs)

loadTab_diseases = do s <-readFile "diseases.txt"
                      return (gerlist_dis (map words (lines s)))

gerlist_dis [] = []
gerlist_dis ([n,c,d,p]:xs) = (n,c,d,p):(gerlist_dis xs)

print_lst_dis [] =""
print_lst_dis ((n,c,d,p):xs) = "Disease- name=" ++ n ++ ", Virus =" ++ c ++ ", Symptons= " ++ d ++ ", Quarantine= " ++ p ++ "\n" ++ (print_lst_dis xs)

main :: IO()
main = do list_patients <- loadTab_patients
          list_diseases <- loadTab_diseases
          putStr "1-Insert new Disease\n"
          putStr "2-Insert new Patient\n"
          putStr "3-View Disease List\n"
          putStr "4-View Patient List\n"
          putStr "Op: "
          resp <- getLine
          if (resp=="1") then do insert_dise
                              
          else if (resp=="2") then do insert_pati

          else if (resp=="3") then do putStr(print_lst_dis list_diseases)

          else if (resp=="4") then do putStr(print_lst_pat list_patients)
                                 
          else error "Opcao nao encontrada"

          putStr "Deseja continuar (s/n)? : "
          resp <- getLine
          if (resp=="s" || resp=="S") then main else return()