import Data.List
import qualified Data.Char as Foo

type Disease = (Name, Virus,Symptons,Quarantine)
type Patient = (Name,Symptons,Date)
type PatientQuarantine = (Name,Symptons,Date,Quarantine,Exit)

type PatientQuarantine2 = (Name,Symptons,Chance,Quarantine,Virus)

type PatientVirus = (Name,Virus,Chance)

--type Disease2 = (Name, Virus,Symptons2,Quarantine)

type Name = String
type Virus = String
type Symptons = [String]
type Symptons2 = String
type Quarantine = String
type Date = String
type Exit = String
type DiseaseName = String
type Chance = Integer

--list_diseases2 :: [Disease2]
--list_diseases2 = []

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
checkIfContainsList [] [] = 0
checkIfContainsList x (y:ys) = if (checkIfContains x y >= 1) then 1 + checkIfContainsList x ys else checkIfContainsList x ys

--compareSymptons :: Patient -> [Disease] -> [PatientVirus]
--compareSymptons _ [] = []
--compareSymptons _ _ = []
--compareSymptons (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::Name,yc::Virus,checkIfContainsList xd yd::Chance)::PatientVirus):compareSymptons (xn,xd,xp) ys

--maxChanceVirus :: [PatientVirus] -> Integer -> String
--maxChanceVirus [] aux = ""
--maxChanceVirus ((xn,xc,xd):xs) aux = if(xd > aux) then xc ++ maxChanceVirus xs xd 
--                                                       else maxChanceVirus xs aux

--compareSymptonsAll2 :: [Patient] -> [Disease] -> [PatientVirus]
--compareSymptonsAll2 [] [] = []
--compareSymptonsAll2 ((xn,xd,xp):xs) ((yn,yc,yd,yp):ys) = ((xn::Name,yc::Virus,checkIfContainsList xd yd::Chance)::PatientVirus):compareSymptonsAll2 xs ys

--compareSymptonsAll2 :: [Patient] -> [Disease] -> [PatientVirus]
--compareSymptonsAll2 [] [] = []
--compareSymptonsAll2 [] _ = []
--compareSymptonsAll2 ((xn,xd,xp):xs) y = (compareSymptonsAll (xn,xd,xp) y) ++ (compareSymptonsAll2 xs y)

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

-- type PatientQuarantine2 = (Name,Symptons,Chance,Quarantine,Virus)

quaratineCompareSymptonsAll :: Patient -> [Disease] -> [PatientQuarantine2]
quaratineCompareSymptonsAll _ [] = []
quaratineCompareSymptonsAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::Name,xd::Symptons,checkIfContainsList xd yd::Chance, yp::Quarantine, yc::Virus)::PatientQuarantine2):quaratineCompareSymptonsAll (xn,xd,xp) ys

quarantineCompareSymptonsAll3 :: String -> [Patient] -> [Disease] -> [PatientQuarantine2]
quarantineCompareSymptonsAll3 name [] [] = []
quarantineCompareSymptonsAll3 name [] _ = []
quarantineCompareSymptonsAll3 name ((xn,xd,xp):xs) y = if (xn == name) then (quaratineCompareSymptonsAll (xn,xd,xp) y) ++ (quarantineCompareSymptonsAll3 name xs y) else (quarantineCompareSymptonsAll3 name xs y) 

quarantineMax_list :: [PatientQuarantine2] -> String
quarantineMax_list [(n,s,c,q,v)] = q
quarantineMax_list ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then quarantineMax_list ((n1,s1,c1,q1,v1):xs) else quarantineMax_list((n2,s2,c2,q2,v2):xs)


--maxChanceVirus3 :: [PatientVirus] -> Integer -> String
--maxChanceVirus3 [] aux = ""
--maxChanceVirus3 ((xn,xc,xd):xs) aux = if(xd > aux) then do return maxChanceVirus3 xs xd else do return maxChanceVirus3 xs aux

--loadTab_patientVirus p d = do return (compareSymptons p d)


--compareSymptons x ((_,_,d,_):xs) = if (checkIfContainsList x d >= 1) then checkIfContainsList x d + compareSymptons x xs else compareSymptons x xs


--loadTab_diseases = do s <-readFile "diseases.txt"
--                     return (gerlist_dis (map words (lines s)))

--gerlist_dis [] = []
--gerlist_dis ([n,c,d,p]:xs) = ((n::Name,c::Virus,split d::Symptons,p::Quarantine)::Disease):(gerlist_dis xs)

--compareSymptons :: [String] -> [Disease] -> Integer
--compareSymptons _ [] = 0
--compareSymptons [] _ = 0
--compareSymptons [] [] = 0
--compareSymptons x ((_,_,d,_):xs) = if (checkIfContainsList x d >= 1) then checkIfContainsList x d + compareSymptons x xs else compareSymptons x xs

    -- "Disease- name=" ++ n ++ ", Virus =" ++ c ++ ", Symptons= " ++ d ++ ", Quarantine= " ++ p ++ "\n" ++ (print_lst_dis xs)

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
gerlist_pat ([n,d,p]:xs) = ((n::Name,split d::Symptons,p::Date)::Patient):(gerlist_pat xs)

print_lst_pat [] =""
print_lst_pat ((n,d,p):xs) = "Patient- Name = " ++ n ++ ", Symptons = [ " ++ print_symptons_each2 d ++ "], Date = " ++ p ++ "\n"  ++ (print_lst_pat xs) 

isQuarantine :: [Patient] -> [Disease] -> [PatientQuarantine]
isQuarantine _ [] = []
isQuarantine [] _ = []
isQuarantine [] [] = [] -- 
isQuarantine ((xn,xd,xp):xs) ((yn,yc,yd,yp):ys) = do let resultado = quarantineCompareSymptonsAll3 xn ((xn,xd,xp):xs) ((yn,yc,yd,yp):ys)
                                                     if (resultado == []) then isQuarantine xs ys else if (quarantineMax_list resultado == "yes") then ((xn::Name,xd::Symptons,xp::Date,yn::Quarantine,xp::Exit)::PatientQuarantine):isQuarantine xs ys else isQuarantine xs ys 

loadTab_quarantine p d = do return (isQuarantine p d)

--gerlist_qua [] [] = []
--gerlist_qua ((xn,xd,xp):xs) ((yn,yc,yd,yp):ys) = if (checkIfContainsList xd yd >= 1 && yp == "yes") then ((xn::Name,xd::Symptons,xp::Date,yp::Quarantine,xp::Exit)::PatientQuarantine):(gerlist_qua xs ys)


loadTab_diseases = do s <-readFile "diseases.txt"
                      return (gerlist_dis (map words (lines s)))

gerlist_dis [] = []
gerlist_dis ([n,c,d,p]:xs) = ((n::Name,c::Virus,split d::Symptons,p::Quarantine)::Disease):(gerlist_dis xs)

print_lst_dis [] =""
print_lst_dis ((n,c,d,p):xs) = "Doenca- Name= " ++ n ++ ", Virus = " ++ c ++ ", Symptons = [ " ++ print_symptons_each2 d ++ "], Quarantine = " ++ p ++ "\n" ++ (print_lst_dis xs)

print_lst_qua [] =""
print_lst_qua ((n,d,c,p,e):xs) = "Paciente- Name= " ++ n ++ ", Symptons = [ " ++ print_symptons_each2 d ++ "], Doenca = " ++ p ++ ", DateEnter = " ++ c ++ ", DateExit = " ++ e ++ "\n" ++ (print_lst_qua xs)


count_quarantine [] = 0
count_quarantine (x:xs) = 1 + count_quarantine xs

split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]

--print_symptons :: [Disease2] -> String
--print_symptons [] = ""
--print_symptons ((_,_,d,_):xs) = print_symptons_each d ++ "\n\n" ++ (print_symptons xs)

--print_symptons_each d = print_symptons_each2(split d)

print_symptons_each2 [] = ""
print_symptons_each2 (x:xs) = x ++ ", " ++ print_symptons_each2 xs

--searchName :: String -> [Patient] -> [Disease] -> String
--searchName name [] y = "Nome Nao Encontrado\n"
--searchName name ((xn,xd,xp):xs) y = if (name == xn) then "Paciente = " ++ xn ++ " | Virus == " ++ xp ++ "\n" else searchName name xs y
--searchName name ((xn,xd,xp):xs) y = if (name == xn) then "Paciente = " ++ xn ++ " | Virus == " ++ (maxChanceVirus (compareSymptons (xn,xd,xp) y) 0)  ++ "\n" else searchName name xs y


main :: IO()
main = do list_patients <- loadTab_patients
          list_diseases <- loadTab_diseases
          list_patients_quarantine <- loadTab_quarantine list_patients list_diseases
          putStr "\n[1] Inserir Doenca\n"
          putStr "[2] Inserir Paciente\n"
          putStr "[3] Listar Todas Doencas\n"
          putStr "[4] Listar Todos Pacientes\n"
          putStr "[5] Listar Pacientes em Quarentena\n"
          putStr "[6] Contar Pacientes em Quarentena\n"
          putStr "[7] Buscar Virus por Nome de Paciente\n"
          putStr "[8] Grafico\n"
          putStr "Op: "
          resp <- getLine
          if (resp=="1") then do insert_dise
                              
          else if (resp=="2") then do insert_pati

          else if (resp=="3") then do putStr(print_lst_dis list_diseases)

          else if (resp=="4") then do putStr(print_lst_pat list_patients)
          
          else if (resp=="5") then do putStr(print_lst_qua list_patients_quarantine)

          else if (resp=="6") then do putStr("Total de Pacientes em Quarentena: ")
                                      print(count_quarantine list_patients_quarantine)

          else if (resp=="7") then do putStr "Buscar Nome: "
                                      name <- getLine
                                      let resultado = compareSymptonsAll3 name list_patients list_diseases
                                      if (resultado == []) then print("Usuario Nao Encontrado") else print(max_list resultado)

          --else if (resp=="8") then do putStr "Buscar Nome: "
          --                            name2 <- getLine
          --                            let resultado2 = quarantineCompareSymptonsAll3 name2 list_patients list_diseases
          --                            if (resultado2 == []) then print("Usuario Nao Encontrado") else print(quarantineMax_list resultado2)

          --else if (resp=="11") then do putStr "Buscar Nome: "
          --                             nam <- getLine
          --                             putStr(searchName nam list_patients list_diseases)
          -- Essa funcao indica que existem elementos iguais em ambas as listas (verificacao booleana)
          --else if (resp=="5") then do if (checkIfContainsList ["febre","dor"] ["coriza","febre"] >= 1) then print("Existe elementos iguais") else print("Nao existem elementos iguais")

          -- Essa funcao retorna quantos elementos iguais foram encontrados em duas listas ( medidor que identifica se o paciente se encaixa em doenca de quarentena ou nao )
          --else if (resp=="8") then do print(compareSymptons ["febre","dor","caxumba","fraqueza"] list_diseases)

          --else if (resp=="7") then do putStr(print_symptons list_diseases2)

          else if (resp=="9") then do print(print_lst_qua list_patients_quarantine)           

          --else if (resp=="10") then do print(compareSymptonsAll2 list_patients list_diseases)      

          else error "Opcao nao encontrada"

          putStr "\nDeseja continuar (s/n)? : "
          resp <- getLine
          if (resp=="s" || resp=="S") then main else return()