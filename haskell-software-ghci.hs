import Data.List
import Data.Time
import Data.Time.Format
import qualified Data.Char as Foo

type Disease = (Name, Virus,Symptons,Quarantine)
type Patient = (Name,Symptons,Date)
type PatientQuarantine = (Name,Date,Exit)
type PatientQuarantineByName = (Name,Symptons,Chance,Quarantine,Virus)

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

-- #############################################################################################################################################################
-- Verifica e Conta quantos valores em duas listas sao iguais

checkIfContains :: [String] -> String -> Integer
checkIfContains x y = case elemIndex y x of
                    Nothing -> 0
                    Just n  -> 1

checkIfContainsList :: [String] -> [String] -> Integer
checkIfContainsList _ [] = 0
checkIfContainsList [] _ = 0
checkIfContainsList x (y:ys) = if (checkIfContains x y >= 1) then 1 + checkIfContainsList x ys else checkIfContainsList x ys

-- #############################################################################################################################################################
-- Realiza comparacoes entre o paciente e todas as doencas para descobrir a doenca mais provavel ( na busca por nome -> doenca | retorna se a doenca eh de quarentena ou nao )

compareSymptonsAll :: Patient -> [Disease] -> [PatientVirus]
compareSymptonsAll _ [] = []
compareSymptonsAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::Name,yc::Virus,checkIfContainsList xd yd::Chance)::PatientVirus):compareSymptonsAll (xn,xd,xp) ys

compareSymptonsAllByName :: String -> [Patient] -> [Disease] -> [PatientVirus]
compareSymptonsAllByName name [] [] = []
compareSymptonsAllByName name [] _ = []
compareSymptonsAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (compareSymptonsAll (xn,xd,xp) y) ++ (compareSymptonsAllByName name xs y) else (compareSymptonsAllByName name xs y) 

max_list :: [PatientVirus] -> String
max_list [(n,v,c)] = "Virus: " ++ v
max_list ((x1,v1,c1):(x2,v2,c2):xs) = if (c1 > c2) then max_list ((x1,v1,c1):xs) else max_list((x2,v2,c2):xs)

-- #############################################################################################################################################################
-- Realiza comparacoes entre todos os paciente e todas as doencas para descobrir a doenca mais provavel de cada um ( retorna uma lista de pacientes e suas doencas )

quaratineCompareSymptonsAll :: Patient -> [Disease] -> [PatientQuarantineByName]
quaratineCompareSymptonsAll _ [] = []
quaratineCompareSymptonsAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::Name,xd::Symptons,checkIfContainsList xd yd::Chance, yp::Quarantine, yc::Virus)::PatientQuarantineByName):quaratineCompareSymptonsAll (xn,xd,xp) ys

quarantinecompareSymptonsAllByName :: String -> [Patient] -> [Disease] -> [PatientQuarantineByName]
quarantinecompareSymptonsAllByName name [] [] = []
quarantinecompareSymptonsAllByName name [] _ = []
quarantinecompareSymptonsAllByName name _ [] = []
quarantinecompareSymptonsAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (quaratineCompareSymptonsAll (xn,xd,xp) y) ++ (quarantinecompareSymptonsAllByName name xs y) else (quarantinecompareSymptonsAllByName name xs y) 

quarantineMax_list :: [PatientQuarantineByName] -> String
quarantineMax_list [(n,s,c,q,v)] = q
quarantineMax_list ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then quarantineMax_list ((n1,s1,c1,q1,v1):xs) else quarantineMax_list((n2,s2,c2,q2,v2):xs)

quarantineMax_listGraph :: [PatientQuarantineByName] -> String
quarantineMax_listGraph [(n,s,c,q,v)] = "\n  " ++ n  ++ " -> " ++ v ++ ";" 
quarantineMax_listGraph ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then quarantineMax_listGraph ((n1,s1,c1,q1,v1):xs) else quarantineMax_listGraph((n2,s2,c2,q2,v2):xs)

-- #############################################################################################################################################################
-- Funcao Que converte DATA-STRING para DATA-DAY

parseDay :: String -> Day
parseDay s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s

-- #############################################################################################################################################################
-- Funcoes que inserem Diseases e Patients

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

-- #############################################################################################################################################################
-- Funcoes que carregam a  tabela de PACIENTES e DISEASES ( a partir de arquivos TXT )

loadTab_patients = do s <-readFile "patients.txt"
                      return (gerlist_pat (map words (lines s)))

gerlist_pat [] = []
gerlist_pat ([n,d,p]:xs) = ((n::Name,split d::Symptons,p::Date)::Patient):(gerlist_pat xs)

print_lst_pat [] =""
print_lst_pat ((n,d,p):xs) = "Patient- Name = " ++ n ++ ", Symptons = [ " ++ print_symptons_each d ++ "], Date = " ++ p ++ "\n"  ++ (print_lst_pat xs) 

gerlist_qua [] = []
gerlist_qua ([n,d,p]:xs) = ((n::Name,split d::Symptons,p::Date)::Patient):(gerlist_pat xs)

loadTab_diseases = do s <-readFile "diseases.txt"
                      return (gerlist_dis (map words (lines s)))

gerlist_dis [] = []
gerlist_dis ([n,c,d,p]:xs) = ((n::Name,c::Virus,split d::Symptons,p::Quarantine)::Disease):(gerlist_dis xs)

-- #############################################################################################################################################################
-- Funcao que carrega a  tabela de PACIENTES EM QUARENTENA -> Baseia na tabela de pacientes e preenche a tabela de quarentena.

loadTab_quarantine p d = do return (isQuarantine p d)

isQuarantine :: [Patient] -> [Disease] -> [PatientQuarantine]
isQuarantine [] [] = []
isQuarantine _ [] = []
isQuarantine [] _ = []
isQuarantine ((xn,xd,xp):xs) y = if (quarantineMax_list (quarantinecompareSymptonsAllByName xn ((xn,xd,xp):xs) y) == "yes") then return ((xn::Name,xp::Date,showGregorian (addDays 40 (parseDay xp))::Exit)::PatientQuarantine) ++ isQuarantine xs y else isQuarantine xs y

-- #############################################################################################################################################################
-- Funcoes que IMPRIMEM as tabelas de PACIENTES / DOENCAS / PACIENTES EM QUARENTENA

print_lst_dis [] =""
print_lst_dis ((n,c,d,p):xs) = "Doenca- Name= " ++ n ++ ", Virus = " ++ c ++ ", Symptons = [ " ++ print_symptons_each d ++ "], Quarantine = " ++ p ++ "\n" ++ (print_lst_dis xs)

print_lst_qua date [] =""
print_lst_qua date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) < 0) then "Paciente- Name= " ++ n ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua date xs) else (print_lst_qua date xs)

print_lst_qua_newDate_still date [] =""
print_lst_qua_newDate_still date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) < 0) then "Paciente- Name= " ++ n ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua_newDate_still date xs) else (print_lst_qua_newDate_still date xs)

print_lst_qua_newDate_out date [] =""
print_lst_qua_newDate_out date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) >= 0) then "Paciente- Name= " ++ n ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua_newDate_out date xs) else (print_lst_qua_newDate_out date xs)

-- #############################################################################################################################################################
-- Funcao que conta o numero de pacientes em quarentena

count_quarantine [] = 0
count_quarantine (x:xs) = 1 + count_quarantine xs

-- #############################################################################################################################################################
-- Funcao que converte uma STRING para [STRING] (inserindo sintomas separados por virgulas em uma LISTA)

split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]

print_symptons_each [] = ""
print_symptons_each (x:xs) = x ++ ", " ++ print_symptons_each xs

-- #############################################################################################################################################################
-- Funcao que carrega o TXT com a data atual do sistema

load_date = do s <-readFile "systemdate.txt"
               return (parseDay s)

-- #############################################################################################################################################################
-- Funcao que gera um grafico de ( Paciente -> Virus )

graph :: [Patient] -> [Disease] -> String
graph [] [] = ""
graph _ [] = ""
graph [] _ = ""
graph ((xn,xd,xp):xs) y = quarantineMax_listGraph (quarantinecompareSymptonsAllByName xn ((xn,xd,xp):xs) y) ++ graph xs y

-- #############################################################################################################################################################
-- Funcao main que contem todas as acoes do programa

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
                                      let resultado = compareSymptonsAllByName name list_patients list_diseases
                                      if (resultado == []) then print("Usuario Nao Encontrado") else print(max_list resultado)

          else if (resp=="8") then do putStr "Definir Data (yyyy-mm-dd): "
                                      newdata <- getLine
                                      let newdate = parseDay newdata
                                      putStr "\nLista de Quarentena Atualizada [Pacientes que PERMANECERAM em quarentena com a nova data]:\n"
                                      writeFile "systemdate.txt" (showGregorian newdate)
                                      let newlist = print_lst_qua_newDate_still newdate list_patients_quarantine
                                      if (newlist == "") then putStr(" - Nenhum paciente permaneceu na quarentena.\n") else putStr(newlist)
                                      
                                      putStr "\nLista de Quarentena Atualizada [Pacientes que SAIRAM da quarentena com a nova data]:\n"
                                      let newlist = print_lst_qua_newDate_out newdate list_patients_quarantine
                                      if (newlist == "") then putStr(" - Nenhum paciente saiu da quarentena.\n") else putStr(newlist)

          else if (resp=="9") then do putStr("\nDigraph {" ++ graph list_patients list_diseases ++"\n}\n")
                                          
          else error "Opcao nao encontrada"

          putStr "\nDeseja continuar (s/n)? : "
          resp <- getLine
          if (resp=="s" || resp=="S") then main else return()