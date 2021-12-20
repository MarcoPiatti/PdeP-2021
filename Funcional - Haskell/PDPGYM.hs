data Persona = Persona
    { nombre          :: String
    , calorias        :: Int
    , hidratacion     :: Int
    , tiempo          :: Int
    , equipamentos    :: [Equipamento]
    }

type Equipamento = String

--------------------------------------------------------------------------------------
--helpers

type MapInt = (Int -> Int) -> Persona -> Persona

mapNombre :: (String -> String) -> Persona -> Persona
mapNombre f unaPersona = unaPersona { nombre = f . nombre $ unaPersona }

mapEquipamentos :: ([Equipamento] -> [Equipamento]) -> Persona -> Persona
mapEquipamentos f unaPersona = unaPersona { equipamentos = f . equipamentos $ unaPersona }

mapCalorias :: MapInt
mapCalorias f unaPersona = unaPersona { calorias = f . calorias $ unaPersona }

multiplicarCalorias :: Int -> Persona -> Persona
multiplicarCalorias = mapCalorias . (*)

subirCalorias :: Int -> Persona -> Persona
subirCalorias = mapCalorias . (+)

mapTiempo :: (Int -> Int) -> Persona -> Persona
mapTiempo f unaPersona = unaPersona { tiempo = f . tiempo $ unaPersona }

mapHidratacion :: MapInt
mapHidratacion f unaPersona = unaPersona { hidratacion = f . hidratacion $ unaPersona }

multiplicarHidratacion :: Int -> Persona -> Persona
multiplicarHidratacion = mapHidratacion . ((max 100) .) . (*)

pam :: [a -> b] -> a -> [b]
pam = flip $ map . flip ($)

pam :: [a -> b] -> a -> [b]
pam funciones valor = map ($ valor) funciones

-- La uso para los casos donde quiero hacer point-free a una funcion de tipo
-- f x y = g x . h x $ y
-- Me encantaria saber la forma correcta de lograrlo sin recurrir a esto 
componerConMismaVariable :: (a -> c -> d, a -> b -> c) -> a -> (b -> d)
componerConMismaVariable (unaFuncion, otraFuncion) unParametro = unaFuncion unParametro . otraFuncion unParametro

--------------------------------------------------------------------------------------
-- Parte A

type Ejercicio = Persona -> Persona

perder :: MapInt -> Int -> Persona -> Persona
perder unMapper = unMapper . subtract

perderPorRepeticion :: MapInt -> Int -> Int -> Int -> Persona -> Persona
perderPorRepeticion mapper caloriasPorRepeticion repeticionesPorCaloria repeticiones = perder mapper ((caloriasPorRepeticion * repeticiones) `div` repeticionesPorCaloria)

abdominales :: Int -> Ejercicio
abdominales = perderPorRepeticion mapCalorias 8 1

flexiones :: Int -> Ejercicio
flexiones = componerConMismaVariable (perderPorRepeticion mapHidratacion 2 10, perderPorRepeticion mapCalorias 16 1)

tieneEquipamento :: Equipamento -> Persona -> Bool
tieneEquipamento = (. equipamentos) . elem

tienePesa :: Persona -> Bool
tienePesa = tieneEquipamento "pesa"

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas peso repeticiones unaPersona
    | tienePesa unaPersona = perderPorRepeticion mapHidratacion peso 10 repeticiones . perderPorRepeticion mapCalorias 32 1 repeticiones $ unaPersona
    | otherwise            = unaPersona

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson = id

type Accion = Persona -> Persona

renovarEquipo :: Accion
renovarEquipo = mapEquipamentos $ map ("Nuevo" ++)

volverseYoguista :: Accion
volverseYoguista = multiplicarHidratacion 2 . mapCalorias (`div` 2)

volverseBodyBuilder :: Accion
volverseBodyBuilder unaPersona
    | all (== "pesa") . equipamentos $ unaPersona = multiplicarCalorias 3 . mapNombre (++ "BB") $ unaPersona
    | otherwise                                   = unaPersona

comerUnSandwich :: Accion
comerUnSandwich = mapHidratacion (const 100) . subirCalorias 500

--------------------------------------------------------------------------------------
-- Parte B

data Rutina = Rutina
    { duracion :: Int
    , ejercicios :: [Ejercicio]   
    }

tieneTiempoParaRutina :: Rutina -> Persona -> Bool
tieneTiempoParaRutina  = (. tiempo) . (>=) . duracion 

realizarRutina :: Rutina -> Persona -> Persona
realizarRutina unaRutina unaPersona
    | tieneTiempoParaRutina unaRutina unaPersona = foldr ($) unaPersona (ejercicios unaRutina)
    | otherwise                                  = unaPersona

cumpleLasSiguientes :: [Persona -> Bool] -> Persona -> Bool
cumpleLasSiguientes = (and .) . pam 

estaAgotada :: Persona -> Bool
estaAgotada = cumpleLasSiguientes [(< 50) . calorias, (< 10) . hidratacion]

esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa = (estaAgotada .) . realizarRutina

estaBalanceada :: Int -> Persona -> Bool
estaBalanceada valorInicial = cumpleLasSiguientes [(> 80) . hidratacion, (< valorInicial `div` 2) . calorias]

esBalanceada :: Persona -> Rutina -> Bool
esBalanceada = componerConMismaVariable (estaBalanceada . calorias, flip realizarRutina)

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina
    { duracion = 1
    , ejercicios = map abdominales [1..]
    }

--------------------------------------------------------------------------------------
-- Parte C

mismoTiempoDisponible :: Persona -> Persona -> Bool
mismoTiempoDisponible = (. tiempo) . (==) . tiempo

seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio = filter . mismoTiempoDisponible

-- No pude darle la vuelta para hacerla point-free
promedioDeGrupo :: (Persona -> Int) -> [Persona] -> Int
promedioDeGrupo accesor unGrupo = ($ length unGrupo ) . (div) . sum . map accesor $ unGrupo

pamDupla :: ([Persona] -> Int, [Persona] -> Int) -> [Persona] -> (Int, Int)
pamDupla (unaFuncion, otraFuncion) unGrupo = (unaFuncion unGrupo, otraFuncion unGrupo)

promedioDeRutina :: Rutina -> [Persona] -> (Int, Int)
promedioDeRutina = (pamDupla (promedioDeGrupo calorias, promedioDeGrupo hidratacion) .) . map . realizarRutina