{-# LANGUAGE RecordWildCards #-}    -- per utilitzar fields

-- IMPORT --

import Data.Char (isUpper)
import Data.List (nub)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)    -- cal instal·lar: cabal install split
import Data.String.Utils (strip)    -- cal instal·lar: cabal install MissingH
import Data.Maybe (mapMaybe)
import Data.Maybe (fromMaybe)



-- TYPE --

-- Conjunt de regles.
type Programa = [ Regla ]

-- Conjunt de sustitucions (Variable, Constant).
type Sustitucio = [ (Term, Term) ]

-- Conjunt de ground atoms.
type BaseConeixement = [ Atom ]



-- DATA --

-- Representa una regla logica. Si _cos = [] tenim un fet.
data Regla = Regla { _cap::Atom, _cos::[ Atom ] }
    deriving (Eq)
    
-- Representa un atom amb un nom i un conjunt de termes.
data Atom = Atom { _nomPredicat::String, _termes::[ Term ] }
    deriving (Eq)
   
-- Representa un terme que pot ser una variable o una constant.
data Term = Var String | Sym String
    deriving (Eq) 
    
    
    
-- INSTANCE --

{- 
Imprimir regla de forma visualment atractiva.
Si no te antecedent només imprimeix el consequent.
Si es una pregunta sense termes no imprimeix "=>" i només imprimeix un interrogant després dels antecedents.
Si es una pregunta amb termes usa el tal que amb les variables que s'imprimiran amb comes si hi ha més d'una.
Si la llista d'atoms té només un no usa parentèsis. Altrament sí.
-}
instance Show Regla where 
    show (Regla consequent llistaAtoms)
        | llistaAtoms == []                  = show consequent
        | _nomPredicat consequent == "query" = if _termes consequent == [] then antecedent ++ " ?"
                                                                           else show consequent ++ " tal que [" ++ antecedent ++ "] ?"
        | length llistaAtoms == 1            = antecedent ++  " => " ++ show consequent
        | otherwise                          = "(" ++ antecedent ++ ") => " ++ show consequent
            where
                antecedent = afegirSeparador " & " $ map show llistaAtoms

{-
Imprimir atom de forma visualment atractiva.
Si es una resposta aplica una funció que retorna correctament el format adient.
-}
instance Show Atom where 
    show (Atom predicat llistaTermes)
        | predicat == "query" = if llistaTermes /= [] then obtenirStringResposta llistaTermes else "?"
        | otherwise           =  predicat ++ " " ++ termes
            where
                termes = afegirSeparador " " $ map show llistaTermes
    
-- Imprimir terme de forma visualment atractiva. Basicament el nom que té.
instance Show Term where 
    show (Var var)  = var
    show (Sym cons) = cons

    
    
-- UTILS --

{-
Donat un separador y una llista de strings retorna un string on s'ha afegit el separador entre cada element de la llista.
S'utilitza pels shows.
-}
afegirSeparador :: String -> [String] -> String
afegirSeparador separador llistaStrings =
    foldl (\a b -> (if a == "" then "" else a ++ separador) ++ b) "" llistaStrings

-- Retorna una llista de termes en format resposta. Se li afegeix "," després de cada terme. No ho fa després del últim.
obtenirStringResposta :: [Term] -> String
obtenirStringResposta [] = ""
obtenirStringResposta (terme:llistaTermes)
    | llistaTermes == [] = show terme ++ obtenirStringResposta llistaTermes
    | otherwise          = show terme ++ "," ++ obtenirStringResposta llistaTermes

{-
Imprimeix un programa en el format adient. Te un contador per indicar quina regla estem imprimint.
-}
imprimirPrograma :: Programa -> Int -> IO()
imprimirPrograma [] _ = putStr "\n"
imprimirPrograma (regla:programa) num = do
    putStrLn $ " " ++ show num ++ ". " ++ show regla ++ "\n" 
    imprimirPrograma programa $ num + 1

{-
Imprimeix una linia de "+-". S'utilitza com a separador per donar les respostes.
-}
imprimirLinia :: IO()
imprimirLinia = putStrLn "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"

{-
Retorna un programa buit.
-}
programaBuit :: Programa
programaBuit = []

{-
Retorna una base de coneixement buida.
-}
baseConeixementBuida :: BaseConeixement
baseConeixementBuida = []

{-
Retorna una sustitucio buida.
-}
sustitucioBuida :: Sustitucio
sustitucioBuida = []

{-
Retorna una sustitucio invalida. S'utilitza en els fromMaybe com a valor predeterminat.
Notar que es realment invalida ja que mai obtindrem, parsejant, una sustitucio d'un símbol cap a una variable i
mai obtindrem un símbol que començi per majúscula i una variable que no ho faci.
-}
sustitucioInvalida :: Sustitucio
sustitucioInvalida = [(Sym "Invalida",Var "invalida")]

{-
Retorna cert si un Either general és del tipus esquerra.
Fals atrament.
-}
esEsquerraEither :: Either a b -> Bool
esEsquerraEither (Left _) = True
esEsquerraEither _        = False

{-
Pre: L'Either d'entrada es del tipus esquerra.
Obté el valor de l'esquerra d'un Either general.
-}
obtenirEsquerra :: Either a b -> a
obtenirEsquerra (Left esquerra) = esquerra

{-
Pre: L'Either d'entrada es del tipus dret.
Obté el valor de la dreta d'un Either general.
-}
obtenirDreta :: Either a b -> b
obtenirDreta (Right dreta) = dreta

{-
Imprimeix un boolea en català.
-}
imprimirBool :: Bool -> IO()
imprimirBool True  = putStrLn "Cert\n"
imprimirBool  _    = putStrLn "Fals\n"

{-
Imprimeix un conjunt de sustitucions en un format adient.
Quan es la ultima sustitucio passem a la funció imprimirSustitucio "\n\n" per tal que quan acabi imprimeixi això.
Sino usem un salt de linia i espais com a separador de les sustitucions.
-}
imprimirSustitucions :: [Sustitucio] -> IO()
imprimirSustitucions [] = return ()
imprimirSustitucions (sustitucio:sustitucions) = do
    if sustitucions == [] then imprimirSustitucio sustitucio "\n\n"
    else imprimirSustitucio sustitucio "\n             "
    imprimirSustitucions sustitucions
    
{-
Imprimeix una sustitucio en un format adient.
Si es la última sustitucio utilitzem l'String que ens ve com a parametre.
Si no es la última usem un espaciador "&" per separar-les.
-}
imprimirSustitucio :: Sustitucio -> String -> IO()
imprimirSustitucio [] _ = return ()
imprimirSustitucio ((Var var, Sym con):sustitucio) string = do
    if sustitucio == [] then putStr $ var ++ " = " ++ con ++ string
    else putStr $ var ++ " = " ++ con ++ " & "
    imprimirSustitucio sustitucio string



-- MAIN --

{-
Inici de la execucció. Parseja el programa, parseja les preguntes, imprimeix els fets, les regles i respon totes les preguntes una per una.
-}
main :: IO()
main = do
    programa <- parsejarPrograma programaBuit
    preguntes <- parsejarPrograma programaBuit
    
    putStrLn "\n ######  ######  #######  #####  ######     #    #     #    #    "
    putStrLn   " #     # #     # #     # #     # #     #   # #   ##   ##   # #   "
    putStrLn   " #     # #     # #     # #       #     #  #   #  # # # #  #   #  "
    putStrLn   " ######  ######  #     # #  #### ######  #     # #  #  # #     # "
    putStrLn   " #       #   #   #     # #     # #   #   ####### #     # ####### "
    putStrLn   " #       #    #  #     # #     # #    #  #     # #     # #     # "
    putStrLn   " #       #     # #######  #####  #     # #     # #     # #     # " 

    let fets = filter (\regla -> _cos regla == []) programa
    let numFets = length fets -- si hi ha mes de 99 no s'imprimirà correctament
    if numFets == 1 then do
        putStrLn "\n +-+-+-+-+-+"
        putStrLn $ " |1| |F|E|T|"
        putStrLn   " +-+-+-+-+-+\n"
    else if numFets < 10 then do
        putStrLn "\n +-+-+-+-+-+-+"
        putStrLn $ " |" ++ show numFets ++ "| |F|E|T|S|"
        putStrLn   " +-+-+-+-+-+-+\n"
    else do
        putStrLn "\n +-+-+-+-+-+-+-+"
        putStrLn $ " |" ++ show (numFets `div` 10) ++ "|" ++ show (numFets `mod` 10) ++ "| |F|E|T|S|"
        putStrLn   " +-+-+-+-+-+-+-+\n"
    imprimirPrograma fets 1
    
    let regles = filter (\regla -> _cos regla /= []) programa
    let numRegles = length regles -- si hi ha mes de 99 no s'imprimirà correctament
    if numRegles == 1 then do
        putStrLn   " +-+-+-+-+-+-+-+"
        putStrLn $ " |1| |R|E|G|L|A|"
        putStrLn   " +-+-+-+-+-+-+-+\n"
    else if numRegles < 10 then do
        putStrLn   " +-+-+-+-+-+-+-+-+"
        putStrLn $ " |" ++ show numRegles ++ "| |R|E|G|L|E|S|"
        putStrLn   " +-+-+-+-+-+-+-+-+\n"
    else do
        putStrLn   " +-+-+-+-+-+-+-+-+-+"
        putStrLn $ " |" ++ show (numRegles `div` 10) ++ "|" ++ show (numRegles `mod` 10) ++ "| |R|E|G|L|E|S|"
        putStrLn   " +-+-+-+-+-+-+-+-+-+\n"
    imprimirPrograma regles 1
    
    putStrLn "\n ######        ##      ######  "
    putStrLn   " #     #      #  #     #     # "
    putStrLn   " #     #       ##      #     # "
    putStrLn   " ######       ###      ######  "
    putStrLn   " #           #   # #   #   #   "
    putStrLn   " #           #    #    #    #  "
    putStrLn   " # reguntes   ###  #   #     # espostes \n"
                     
    let numPreguntes = length preguntes -- si hi ha mes de 99 no s'imprimirà correctament
    if numPreguntes == 1 then do
        putStrLn   " +-+-+-+-+-+-+-+-+-+-+"
        putStrLn $ " |1| |P|R|E|G|U|N|T|A|"
        putStrLn   " +-+-+-+-+-+-+-+-+-+-+\n"
    else if numPreguntes < 10 then do
        putStrLn   " +-+-+-+-+-+-+-+-+-+-+-+"
        putStrLn $ " |" ++ show numPreguntes ++ "| |P|R|E|G|U|N|T|E|S|"
        putStrLn   " +-+-+-+-+-+-+-+-+-+-+-+\n"
    else do
        putStrLn   " +-+-+-+-+-+-+-+-+-+-+-+-+"
        putStrLn $ " |" ++ show (numPreguntes `div` 10) ++ "|" ++ show (numPreguntes `mod` 10) ++ "| |P|R|E|G|U|N|T|E|S|"
        putStrLn   " +-+-+-+-+-+-+-+-+-+-+-+-+\n"
    
    respondreTotesPreguntes programa preguntes 1
    return ()
    
    
    
-- PARSING --

{-
Parseja el programa. Va llegint línia per línia fins que troba un "end." on pararà.
Si no es un "end." divideix les regles pel seu separador "." per si hi hagués més d'una a la mateixa línia.
Elimina els espais en blanc (strip) i elimina l'última llista que es buida per acabar en "." (init).
A partir d'aquí afegeix les regles ja separades al programa que serà retornat.
-}
parsejarPrograma :: Programa -> IO(Programa)
parsejarPrograma programa = do
    linia <- getLine
    if linia /= "end." then parsejarPrograma $ afegirReglesAlPrograma programa $ init $ map strip $ splitOn "." linia
                       else return $ nub programa

{-
Afegeix recursivament una llista de regles en forma de strings al programa que serà retornat.
-}     
afegirReglesAlPrograma :: Programa -> [String] -> Programa
afegirReglesAlPrograma programa []             = programa
afegirReglesAlPrograma programa (regla:regles) = afegirReglesAlPrograma (programa ++ [crearRegla regla]) regles

{-
Crea una regla a partir d'un string que conté una regla. Primer separa el string per "=>".
Si aquesta separació només té un element vol dir que tot l'string era un fet. Al altre cas serà una regla amb antecedent.
Si es un fet creem una regla amb un atom creat a partir del atom que compon el fet.
Si es una regla amb antecedent creem una regla que té un atom com a consequent i un conjunt d'atoms com a antecedent.
Aquest conjunt d'atoms el creem a partir de la separació per "&" dels atoms que hi havía al consequent del string.
-}
crearRegla :: String -> Regla
crearRegla regla
    | length reglaSeparadaPerFlecha == 1 = Regla (crearAtom $ reglaSeparadaPerFlecha !! 0) []
    | otherwise = 
        Regla (crearAtom $ reglaSeparadaPerFlecha !! 1) $ map crearAtom $ map strip $ splitOn "&" $ reglaSeparadaPerFlecha !! 0
            where
                reglaSeparadaPerFlecha = map strip $ splitOn "=>" regla

{-
Crea un atom a partir d'un string que conté un atom. Primer de tot separem el atom per " ".
Creem l'atom amb el primer element de la llista separada (que serà el nom del predicat) i 
una llista creada de termes que serà la cua (eliminant el nom del predicat) de la llista separada per " ".
-}
crearAtom :: String -> Atom
crearAtom atom = Atom (atomsSeparatsPerEspai !! 0) $ map crearTerme $ tail atomsSeparatsPerEspai
    where
        atomsSeparatsPerEspai = map strip $ splitOn " " atom

{-
Crea un terme a partir d'un string. Com que ja no s'ha de separar més l'string en questió serà el nom del terme.
Només hem de comprobar si serà una variable (Var) o una constant (Sym) que es comproba mirant si la primera lletra es una majúscula o no.
-}
crearTerme :: String -> Term
crearTerme nomTerme
    | isUpper $ head nomTerme = Var nomTerme
    | otherwise               = Sym nomTerme
    

    
-- RESPOSTES --

{-
Rep un programa principal i un conjunt (també un programa) de preguntes que anirà responent una per una recursivament fins fer-les totes.
Delega totes les operacions importants a la funció respondrePregunta que amb el programa incial i una de les preguntes executa tot el procediment.
Després imprimeix en format adient la pregunta i la seva corresponent resposta
-}
respondreTotesPreguntes :: Programa -> Programa -> Int -> IO()
respondreTotesPreguntes programa [] _ = do
    imprimirLinia
    putStrLn ""
respondreTotesPreguntes programa (pregunta:preguntes) num = do
    resposta <- respondrePregunta $ programa ++ [pregunta]
    
    imprimirLinia
    putStr $ "\n Pregunta "++ show num ++ ": " ++ show pregunta
    if num < 10 then putStr "\n\n Resposta:   "
                else putStr "\n\n Resposta:    "
    
    if esEsquerraEither resposta then imprimirBool $ obtenirEsquerra resposta
    else imprimirSustitucions $ obtenirDreta resposta
    
    respondreTotesPreguntes programa preguntes $ num + 1
    
{-
Rep un programa principal que ja conté una pregunta. El que fa es cridar a una funció que recursivament calcularà la base de coneixement
i, a partir d'aquesta, crea totes les sustitucions vàlides com a resposta de la pregunta afegida.
Depenent d'aquesta resposta retornará un Bool (False si no hi ha possible sustitució, True si la pregunta no tenía variables i és certa) o un conjunt de sustitucions vàlides que satisfacin la pregunta.
-}
respondrePregunta :: Programa -> IO(Either Bool [Sustitucio])
respondrePregunta programa = do
    base <- calcularBaseRecursiva programa baseConeixementBuida
    
    let substitutions = crearTotesSustitucions base $ _termes $ _cap $ last programa
    
    if substitutions == []        then return $ Left False
    else if substitutions == [[]] then return $ Left True 
    else                               return $ Right substitutions

{-
Crea recursivament totes les sustitucions possibles a partir d'una base de coneixement on buscarem la paraula "query" i la llista de termes del cap d'una pregunta.
Com que busquem la paraula query només estarem revisant les possibles aparicions de respostes a la base de coneixement final.
Si apareix podem crear una sustitució amb la llista de termes d'aquest atom resposta. I, com tenim la pregunta, podem crear
una sustitucio amb les dues llistes de termes.
-}
crearTotesSustitucions :: BaseConeixement -> [Term] -> [Sustitucio]
crearTotesSustitucions [] _ = []
crearTotesSustitucions (atom:base) llistaTermes
    | _nomPredicat atom == "query" = [crearSustitucio (_termes atom) llistaTermes] ++ totesSustitucions
    | otherwise                    = totesSustitucions
    where
        totesSustitucions = crearTotesSustitucions base llistaTermes
    
{-
Crea recursivament una sustitució amb les dues llistes de termes que té com a paràmetre.
Com que sabem que la primera llista de termes correspon a la base de coneixement també sabem que tots será constants per ser atoms grounds.
En canvi l'altre llista correspon al cap de la pregunta per tant seràn variables.
-}
crearSustitucio :: [Term] -> [Term] -> Sustitucio
crearSustitucio [] [] = []
crearSustitucio (termeBase:llistaTermesBase) (termeRegla:llistaTermesRegla) =
    [(termeRegla,termeBase)] ++ crearSustitucio llistaTermesBase llistaTermesRegla
   
{-
Calcula recursivament una base de coneixement nova fins que aquesta base sigui la mateixa a la nova creada.
Finalitza quan això succeix i retorna la base calculada.
-}
calcularBaseRecursiva :: Programa -> BaseConeixement -> IO(BaseConeixement)
calcularBaseRecursiva programa base
    | novaBase == base = return novaBase
    | otherwise        = calcularBaseRecursiva programa novaBase
    where
        novaBase       = consequencia programa base
       
{-
Calcula una nova base de coneixement a partir d'una base de coneixement i totes les regles d'un programa.
S'utilitza el nub per eliminar duplicats.
-}
consequencia :: Programa -> BaseConeixement -> BaseConeixement
consequencia programa base = nub $ concat $ map (avaluaRegla base) programa



-- AVALUA --

{-
Avalua una regla amb una base de coneixement per retornar una altre base amb més atoms grounds.
Obté, per mitjà d'una altre funció, un llistat de sustitucions possibles que aplica sobre el cap de la regla.
Si l'atom ja es ground (_cos regla == []) simplement l'afegim. 
-}
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla base regla
    | _cos regla == [] = base ++ [_cap regla]
    | otherwise        = base ++ (sustitueixLlista (_cap regla) $ avaluaAtoms base (_cos regla) [[]])

{-
Avalua recursivament un conjunt d'atoms amb una base de coneixement i una llista de sustitucions que ha de ser,
a cada pas recursiu, acumulable amb l'anterior pas.
Veure la funció següent: avaluaAtom.
-}
avaluaAtoms :: BaseConeixement -> [Atom] -> [Sustitucio] -> [Sustitucio]
avaluaAtoms  _        []      sustitucio = sustitucio
avaluaAtoms base (atom:atoms) sustitucio = avaluaAtoms base atoms $ avaluaAtom base atom sustitucio

{-
Avalua recursivament un atom amb una base de coneixement i una llista de sustitucions.
El que fem es agafar una per una les sustitucions de la llista i aplicarles sobre un atom.
Les sustitucions que obtenim les afegim a la llista de sustitucions que retornarem.
Aquesta llista de sustitucions la creem aplicant la sustitucio a l'atom 
i intentant unificar el resultat amb tots els atoms de la base de coneixement.
Això ens retornarà una Maybe Sustitucio, per tant usem, sobre tots els atoms de la base de coneixement, un mapMaybe.
Per últim afegim la sustitucio aplicada incialment a l'atom a la llista ja que si obtenim algunes substitutions necessitarem la incial per arribar-hi.
Com que el mapMaybe ja ens elimina els que no retornin una sustitucio no estarem afegint la sustitucio incial a no ser que realment poguem unificar amb ella i alguna sustitucio més (fins i tot la buida).
-}
avaluaAtom :: BaseConeixement -> Atom -> [Sustitucio] -> [Sustitucio]
avaluaAtom base atom []                               = []
avaluaAtom base atom (sustitucioInicial:sustitucions) = llistaTotesSustitucions ++ avaluaAtom base atom sustitucions
    where
        llistaTotesSustitucions = map (\sus -> sus ++ sustitucioInicial) $ mapMaybe (unifica $ sustitueix atom sustitucioInicial) base
        -- afegim la propia sustitucioInicial a la llista de sustitucions final (es com fer un pas inductiu)



-- SUSTITUCIO --

{-
Aplica recursivament una sèrie de sustitucions sobre un atom. Per cadascuna d'elles retorna un atom que afegirà
a una llista d'atoms que serà finalment retornada.
-}
sustitueixLlista :: Atom -> [Sustitucio] -> [Atom]
sustitueixLlista atom []                        = []
sustitueixLlista atom (sustitucio:sustitutions) = [sustitueix atom sustitucio] ++ sustitueixLlista atom sustitutions

{-
Aplica una sustitucio sobre un atom. Retorna aquest mateix atom amb totes les sustitucions de la sustitucio aplicades.
-}
sustitueix :: Atom -> Sustitucio -> Atom  
sustitueix atom []                        = atom
sustitueix atom ((var,cons):sustitucions) = sustitueix (mutarAtom atom var cons) sustitucions 

{-
Crea un nou atom a partir d'un atom antic i dos termes. Aquests termes seràn una variable i una constant.
Aquest nou atom tindrà la constant mencionada com a terme on abans tenía la variable mencionada.
-}
mutarAtom :: Atom -> Term -> Term -> Atom
mutarAtom atom var cons = Atom (_nomPredicat atom) $ map (\term -> if term == var then cons else term) $ _termes atom



-- UNIFICACIO --

{-
Unfica dos atoms i retorna la sustitucio necessària per aconseguir-ho. Si no es poden unificar retorna Nothing.
No podem unificar si el nom del predicat es diferent als dos atoms,
si, després de calcular la unificació ncesserària, aquesta es Nothing (per tant invalida),
o si aquesta sustitucio té dues vegades la mateixa variable sustituida per dues constants diferents (utilitzo el nub per treure repetits).
En canvi, sí que podrem unificar si tot això no passa, i retornarem aquesta sustitucio sense repetits.
-}
unifica :: Atom -> Atom -> Maybe Sustitucio
unifica atom1 atom2
    | _nomPredicat atom1 /= _nomPredicat atom2              = Nothing
    | sustitucio == sustitucioInvalida                      = Nothing
    | mateixaVariableDiferentsSustitucions $ nub sustitucio = Nothing
    | otherwise                                             = Just $ nub sustitucio
        where
            sustitucio = fromMaybe sustitucioInvalida $ llistaTermesUnificable (_termes atom1) (_termes atom2) sustitucioBuida

{-
Pre: La sustitucio no te repeticions idéntiques.
Retorna cert si una variable apareix dos cops a la sustitucio.
Això ho podem fer perquè si no tenim repeticions aleshores les sustitucions d'aquesta variable seràn amb diferents constants.
-}
mateixaVariableDiferentsSustitucions :: Sustitucio -> Bool
mateixaVariableDiferentsSustitucions []                                          = False
mateixaVariableDiferentsSustitucions ((var,_):sustitucio)
    | or $ map (\(varAux,_) -> if var == varAux then True else False) sustitucio = True 
    | otherwise                                                                  = mateixaVariableDiferentsSustitucions sustitucio

{-
Pre: La segona llista de termes son tots constants
Retorna potser sustitucio que faci unificar els dos llistats de termes.
Si es possible, calcula la sustitucio que hauría de fer per tal que ho fossin. I en cas contrari retorna Nothing.
Ho fa recursivament recorrent el llistat de termes mirant un a un si ho són amb la funció termeUnificable que retorna la sustitucio que s'hauría de fer perquè ho fossin o Nothing en cas que no ho fossin.
Notar que si trobem un terme no unificable (per exemple dos constants diferents) parem la recursivitat i retornem Nothing.
Només retornarem la sustitucio calculada si arribem al final de totes dues llistes alhora (tenen el mateix número de termes).
-}
llistaTermesUnificable :: [Term] -> [Term] -> Sustitucio -> Maybe Sustitucio
llistaTermesUnificable  [] [] sustitucio       = Just sustitucio
llistaTermesUnificable (terme1:llistaTermes1) (terme2:llistaTermes2) sustitucio
    | sustitucioDeTermes == sustitucioInvalida = Nothing
    | otherwise                                = llistaTermesUnificable llistaTermes1 llistaTermes2 $ sustitucio ++ sustitucioDeTermes
    where
        sustitucioDeTermes = fromMaybe sustitucioInvalida $ termeUnificable terme1 terme2
llistaTermesUnificable  _ _ _                  = Nothing       

{-
Pre: El segon terme es una constant
Retorna potser la sustitucio neccessaría per tal d'unificar els dos termes.
Només podem unificar si tenim dues constants iguals (que ens dona la sustitucio buida),
o si tenim una variable i una constant (que ens dona la sustitucio Var = Cons).
En els altres casos retornem Nothing ja que no podrem unificar.
-}
termeUnificable :: Term -> Term -> Maybe Sustitucio
termeUnificable (Sym cons1) (Sym cons2) = if cons1 == cons2 then Just [] else Nothing
termeUnificable (Var var) (Sym cons)    = Just [(Var var, Sym cons)]
termeUnificable     _         _         = Nothing
