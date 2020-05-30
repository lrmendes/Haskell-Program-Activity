import Data.List
import qualified Data.Char as Foo

type Disease = (Name, Virus,Symptons,Quarantine)
type Patient = (Name,Symptons,Date)
type PatientQuarantine = (Name,Symptons,Date,Quarantine,Exit)

type Disease2 = (Name, Virus,Symptons2,Quarantine)

type Name = String
type Virus = String
type Symptons = [String]
type Symptons2 = String
type Quarantine = String
type Date = String
type Exit = String

list_diseases2 :: [Disease2]
list_diseases2 = []

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

compareSymptons :: [String] -> [Disease] -> Integer
compareSymptons _ [] = 0
compareSymptons [] _ = 0
compareSymptons [] [] = 0
compareSymptons x ((_,_,d,_):xs) = if (checkIfContainsList x d >= 1) then checkIfContainsList x d + compareSymptons x xs else compareSymptons x xs

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
gerlist_pat ([n,d,p]:xs) = (n,d,p):(gerlist_pat xs)

print_lst_pat [] =""
print_lst_pat ((n,d,p):xs) = "Patient- name=" ++ n ++ ", Symptons= " ++ d ++ ", date= " ++ p ++ "\n"  ++ (print_lst_pat xs) 

loadTab_diseases = do s <-readFile "diseases.txt"
                      return (gerlist_dis (map words (lines s)))

gerlist_dis [] = []
gerlist_dis ([n,c,d,p]:xs) = ((n::Name,c::Virus,d::Symptons2,p::Quarantine)::Disease2):(gerlist_dis xs)

print_lst_dis [] =""
print_lst_dis ((n,c,d,p):xs) = "Disease- name=" ++ n ++ ", Virus =" ++ c ++ ", Symptons= " ++ d ++ ", Quarantine= " ++ p ++ "\n" ++ (print_lst_dis xs)

count_out_quarantine [] = 0
count_out_quarantine ((n,d,p):xs) = if (p=="yes") then 1 + count_out_quarantine xs else count_out_quarantine xs

words2 s =  case dropWhile Foo.isSpace s of
                      "" -> []
                      s' -> w : words2 s''
                            where (w, s'') = break Foo.isSpace s'

split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]

print_symptons :: [Disease2] -> String
print_symptons [] = ""
print_symptons ((_,_,d,_):xs) = print_symptons_each d ++ "\n\n" ++ (print_symptons xs)

print_symptons_each d = print_symptons_each2(split d)

print_symptons_each2 [] = ""
print_symptons_each2 (x:xs) = x ++ " - " ++ print_symptons_each2 xs


main :: IO()
main = do list_patients <- loadTab_patients
          list_diseases2 <- loadTab_diseases
          putStr "[1] Inserir Doenca\n"
          putStr "[2] Inserir Paciente\n"
          putStr "[3] Listar Todas Doencas\n"
          putStr "[4] Listar Todos Pacientes\n"
          putStr "[5] Listar Pacientes em Quarentena\n"
          putStr "[6] Contar Pacientes Fora de Quarentena\n"
          putStr "[7] Grafico\n"
          putStr "Op: "
          resp <- getLine
          if (resp=="1") then do insert_dise
                              
          else if (resp=="2") then do insert_pati

          else if (resp=="3") then do putStr(print_lst_dis list_diseases2)

          else if (resp=="4") then do putStr(print_lst_pat list_patients)
          
          -- Essa funcao indica que existem elementos iguais em ambas as listas (verificacao booleana)
          else if (resp=="5") then do if (checkIfContainsList ["febre","dor"] ["coriza","febre"] >= 1) then print("Existe elementos iguais") else print("Nao existem elementos iguais")

          -- Essa funcao retorna quantos elementos iguais foram encontrados em duas listas ( medidor que identifica se o paciente se encaixa em doenca de quarentena ou nao )
          --else if (resp=="6") then do print(compareSymptons ["febre","dor","caxumba","fraqueza"] list_diseases2)

          else if (resp=="7") then do putStr(print_symptons list_diseases2)
                                 
          else error "Opcao nao encontrada"

          putStr "Deseja continuar (s/n)? : "
          resp <- getLine
          if (resp=="s" || resp=="S") then main else return()