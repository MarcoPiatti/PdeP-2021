import Data.Char

data Barbaro = Barbaro
    { nombre :: String
    , fuerza :: Int
    , habilidades :: [Habilidad]
    , objetos :: [Objeto]
    }

type Objeto = Barbaro -> Barbaro
type Habilidad = String

mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre modificadorNombre unBarbaro = unBarbaro { nombre = modificadorNombre . nombre $ unBarbaro }

mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza modificadorFuerza unBarbaro = unBarbaro { fuerza = modificadorFuerza . fuerza $ unBarbaro }

sumarAFuerza :: Int -> Barbaro -> Barbaro
sumarAFuerza = mapFuerza . (+)

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Barbaro -> Barbaro
mapHabilidades modificadorHabilidades unBarbaro = unBarbaro { habilidades = modificadorHabilidades . habilidades $ unBarbaro }

agregarHabilidad :: Habilidad -> Barbaro -> Barbaro
agregarHabilidad = mapHabilidades . (:)

mapObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetos modificadorObjetos unBarbaro = unBarbaro { objetos = modificadorObjetos . objetos $ unBarbaro }

agregarObjeto :: Objeto -> Barbaro -> Barbaro
agregarObjeto = mapObjetos . (:)

borrarObjetos :: Barbaro -> Barbaro
borrarObjetos = mapObjetos $ const []

---------------------------------------------------------------------------------

espada :: Int -> Objeto
espada = sumarAFuerza . (*2)

amuletoMistico :: Habilidad -> Objeto
amuletoMistico = agregarHabilidad

varitaDefectuosa :: Objeto
varitaDefectuosa = agregarObjeto varitaDefectuosa . borrarObjetos . agregarHabilidad "hacer magia"

ardilla :: Objeto
ardilla = id

cuerda :: Objeto -> Objeto -> Objeto
cuerda = (.)

------------------------------------------------------------------------------------

aMayuscula :: [Habilidad] -> [Habilidad]
aMayuscula = map $ map toUpper

megafono :: Objeto
megafono = mapHabilidades $ (: []) . concat . aMayuscula

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono

------------------------------------------------------------------------------------

type Evento = Barbaro -> Bool

tieneHabilidad :: Habilidad -> Barbaro -> Bool
tieneHabilidad = flip $ flip elem . habilidades 

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes = tieneHabilidad "Escribir Poesia Atroz"

noTienePulgares :: Evento
noTienePulgares = (`elem` ["Faffy", "Astro"]) . nombre

cremalleraDelTiempo :: Evento
cremalleraDelTiempo = noTienePulgares

pam :: [a -> b] -> a -> [b]
pam = flip $ map . flip ($)

cumpleLasSiguientes :: [a -> Bool] -> a -> Bool
cumpleLasSiguientes condiciones unValor = and . pam condiciones $ unValor

ritualDeFechorias :: Barbaro -> Bool
ritualDeFechorias = and . pam [saqueo, gritoDeGuerra, caligrafia] 

saqueo :: Evento
saqueo = and . pam [tieneHabilidad "robar", (> 80) . fuerza ]

gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = (>=) (length . concat . habilidades $ unBarbaro) . (4*) . length . objetos $ unBarbaro

caligrafia :: Evento
caligrafia = and . map (cumpleLasSiguientes [tieneMasDeTresVocales, comienzaConMayuscula]) . habilidades

esVocal :: Char -> Bool
esVocal = flip elem "aeiouAEIOU"

tieneMasDeTresVocales :: Habilidad -> Bool
tieneMasDeTresVocales = (> 3) . length . filter esVocal

comienzaConMayuscula :: Habilidad -> Bool
comienzaConMayuscula = isUpper . head

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos (cabeza : cola) = (cabeza :) . quitarRepetidos . filter (/= cabeza) $ cola

aplicarObjetos :: Barbaro -> Barbaro
aplicarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

generarDescendiente :: Barbaro -> Barbaro
generarDescendiente = aplicarObjetos . mapHabilidades quitarRepetidos . mapNombre (++ "*")

descendientes :: Barbaro -> [Barbaro]
descendientes = iterate generarDescendiente