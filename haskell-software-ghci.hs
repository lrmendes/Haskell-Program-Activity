import Data.List
import Data.Time
import Data.Time.Format
import qualified Data.Char as Foo

type Disease = (Name, Virus,Symptons,Quarantine)
type Patient = (Name,Symptons,Date)
type PatientQuarantine = (Name,Date,Exit)

type PatientQuarantine2 = (Name,Symptons,Chance,Quarantine,Virus)

type PatientVirus = (Name,Virus,Chance)

type Name = String
type Virus = String
type Symptons = [String]
type Symptons2 = String
type Quarantine = String
type Date = String
type Exit = String
type DiseaseName = String
type Chance = Integer

current_data :: Day
current_data = parseDay "2020-06-02"

list_diseases :: [Disease]
list_diseases = []

list_patients :: [Patient]
list_patients = []

list_patients_quarantine :: [PatientQuarantine]
list_patients_quarantine = []

checkIfContains :: [String] -> String -> Integer
checkIfContains x y = case elemIndex y x of
                    Nothing -> 0
                    Just n  -> 1

checkIfContainsList :: [String] -> [String] -> Integer
checkIfContainsList _ [] = 0
checkIfContainsList [] _ = 0
checkIfContainsList x (y:ys) = if (checkIfContains x y >= 1) then 1 + checkIfContainsList x ys else checkIfContainsList x ys

compareSymptonsAll :: Patient -> [Disease] -> [PatientVirus]
compareSymptonsAll _ [] = []
compareSymptonsAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::Name,yc::Virus,checkIfContainsList xd yd::Chance)::PatientVirus):compareSymptonsAll (xn,xd,xp) ys

compareSymptonsAll3 :: String -> [Patient] -> [Disease] -> [PatientVirus]
compareSymptonsAll3 name [] [] = []
compareSymptonsAll3 name [] _ = []
compareSymptonsAll3 name ((xn,xd,xp):xs) y = if (xn == name) then (compareSymptonsAll (xn,xd,xp) y) ++ (compareSymptonsAll3 name xs y) else (compareSymptonsAll3 name xs y) 

max_list :: [PatientVirus] -> String
max_list [(n,v,c)] = "Virus: " ++ v
max_list ((x1,v1,c1):(x2,v2,c2):xs) = if (c1 > c2) then max_list ((x1,v1,c1):xs) else max_list((x2,v2,c2):xs)

-- #############################################################################################################################################################

quaratineCompareSymptonsAll :: Patient -> [Disease] -> [PatientQuarantine2]
quaratineCompareSymptonsAll _ [] = []
quaratineCompareSymptonsAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::Name,xd::Symptons,checkIfContainsList xd yd::Chance, yp::Quarantine, yc::Virus)::PatientQuarantine2):quaratineCompareSymptonsAll (xn,xd,xp) ys

quarantineCompareSymptonsAll3 :: String -> [Patient] -> [Disease] -> [PatientQuarantine2]
quarantineCompareSymptonsAll3 name [] [] = []
quarantineCompareSymptonsAll3 name [] _ = []
quarantineCompareSymptonsAll3 name _ [] = []
quarantineCompareSymptonsAll3 name ((xn,xd,xp):xs) y = if (xn == name) then (quaratineCompareSymptonsAll (xn,xd,xp) y) ++ (quarantineCompareSymptonsAll3 name xs y) else (quarantineCompareSymptonsAll3 name xs y) 

quarantineMax_list :: [PatientQuarantine2] -> String
quarantineMax_list [(n,s,c,q,v)] = q
quarantineMax_list ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then quarantineMax_list ((n1,s1,c1,q1,v1):xs) else quarantineMax_list((n2,s2,c2,q2,v2):xs)

--parseTimeM String :: Maybe UniversalTime
--parseTimeM False defaultTimeLocale "%Y-%m-%d" "2016-10-20" :: Maybe UniversalTime

parseDay :: String -> Day
parseDay s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s

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
                 putStr "Symptons (s1,s2,s3): "
                 d <- getLine
                 putStr "Date (yyyy-mm-dd): "
                 p <- getLine
                 print("Cadastrando...")
                 appendFile "patients.txt" (n ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                 print("Sucesso!")
                 putStr "Insert another one? : "
                 resp <- getLine
                 if (resp=="s" || resp=="S") then insert_pati else return()

loadTab_patients = do s <-readFile "patients.txt"
                      return (gerlist_pat (map words (lines s)))

gerlist_pat [] = []
gerlist_pat ([n,d,p]:xs) = ((n::Name,split d::Symptons,p::Date)::Patient):(gerlist_pat xs)

print_lst_pat [] =""
print_lst_pat ((n,d,p):xs) = "Patient- Name = " ++ n ++ ", Symptons = [ " ++ print_symptons_each2 d ++ "], Date = " ++ p ++ "\n"  ++ (print_lst_pat xs) 

gerlist_qua [] = []
gerlist_qua ([n,d,p]:xs) = ((n::Name,split d::Symptons,p::Date)::Patient):(gerlist_pat xs)

isQuarantine :: [Patient] -> [Disease] -> [PatientQuarantine]
isQuarantine [] [] = []
isQuarantine _ [] = []
isQuarantine [] _ = []
isQuarantine ((xn,xd,xp):xs) y = if (quarantineMax_list (quarantineCompareSymptonsAll3 xn ((xn,xd,xp):xs) y) == "yes") then return ((xn::Name,xp::Date,showGregorian (addDays 40 (parseDay xp))::Exit)::PatientQuarantine) ++ isQuarantine xs y else isQuarantine xs y
                                                  

loadTab_quarantine p d = do return (isQuarantine p d)

loadTab_diseases = do s <-readFile "diseases.txt"
                      return (gerlist_dis (map words (lines s)))

gerlist_dis [] = []
gerlist_dis ([n,c,d,p]:xs) = ((n::Name,c::Virus,split d::Symptons,p::Quarantine)::Disease):(gerlist_dis xs)

print_lst_dis [] =""
print_lst_dis ((n,c,d,p):xs) = "Doenca- Name= " ++ n ++ ", Virus = " ++ c ++ ", Symptons = [ " ++ print_symptons_each2 d ++ "], Quarantine = " ++ p ++ "\n" ++ (print_lst_dis xs)

print_lst_qua date [] =""
print_lst_qua date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) < 0) then "Paciente- Name= " ++ n ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua date xs) else (print_lst_qua date xs)

print_lst_qua_newDate date [] =""
print_lst_qua_newDate date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) < 0) then "Paciente- Name= " ++ n ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua_newDate date xs) else (print_lst_qua_newDate date xs)

print_lst_qua_newDate2 date [] =""
print_lst_qua_newDate2 date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) >= 0) then "Paciente- Name= " ++ n ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua_newDate2 date xs) else (print_lst_qua_newDate2 date xs)

print_lst_qua2 [] =""
print_lst_qua2 ((n,c,e):xs) = "Paciente- Name= " ++ n ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua2 xs)


count_quarantine [] = 0
count_quarantine (x:xs) = 1 + count_quarantine xs

split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]

print_symptons_each2 [] = ""
print_symptons_each2 (x:xs) = x ++ ", " ++ print_symptons_each2 xs

load_date = do s <-readFile "systemdate.txt"
               return (parseDay s)

main :: IO()
main = do list_patients <- loadTab_patients
          list_diseases <- loadTab_diseases
          current_data <- load_date
          list_patients_quarantine <- loadTab_quarantine list_patients list_diseases
          print("...")
          putStr ("[ Data Atual do Sistema: " ++ showGregorian current_data ++  " ]\n")
          putStr "[1] Inserir Doenca\n"
          putStr "[2] Inserir Paciente\n"
          putStr "[3] Listar Todas Doencas\n"
          putStr "[4] Listar Todos Pacientes\n"
          putStr "[5] Listar Pacientes em Quarentena\n"
          putStr "[6] Contar Pacientes em Quarentena\n"
          putStr "[7] Buscar Virus por Nome de Paciente\n"
          putStr "[8] Definir Data do Sistema para atualizar quarentena\n"
          putStr "[9] Grafico\n"
          putStr "Op: "
          resp <- getLine
          if (resp=="1") then do insert_dise
                              
          else if (resp=="2") then do insert_pati

          else if (resp=="3") then do putStr(print_lst_dis list_diseases)

          else if (resp=="4") then do putStr(print_lst_pat list_patients)
          
          else if (resp=="5") then do let resul = print_lst_qua current_data list_patients_quarantine
                                      if (resul == "") then putStr("\n Nenhum paciente encontra-se em quarentena!\n") else putStr(resul)

          else if (resp=="6") then do putStr("Total de Pacientes em Quarentena: ")
                                      print(count_quarantine list_patients_quarantine)

          else if (resp=="7") then do putStr "Buscar Nome: "
                                      name <- getLine
                                      let resultado = compareSymptonsAll3 name list_patients list_diseases
                                      if (resultado == []) then print("Usuario Nao Encontrado") else print(max_list resultado)

          else if (resp=="8") then do putStr "Definir Data (yyyy-mm-dd): "
                                      newdata <- getLine
                                      let newdate = parseDay newdata
                                      putStr "\nLista de Quarentena Atualizada [Pacientes que PERMANECERAM em quarentena com a nova data]:\n"
                                      writeFile "systemdate.txt" (showGregorian newdate)
                                      let newlist = print_lst_qua_newDate newdate list_patients_quarantine
                                      if (newlist == "") then putStr(" - Nenhum paciente permaneceu na quarentena.\n") else putStr(newlist)
                                      
                                      putStr "\nLista de Quarentena Atualizada [Pacientes que SAIRAM da quarentena com a nova data]:\n"
                                      let newlist = print_lst_qua_newDate2 newdate list_patients_quarantine
                                      if (newlist == "") then putStr(" - Nenhum paciente saiu da quarentena.\n") else putStr(newlist)

          else if (resp=="9") then do print("Grafico!")

          else error "Opcao nao encontrada"

          putStr "\nDeseja continuar (s/n)? : "
          resp <- getLine
          if (resp=="s" || resp=="S") then main else return()