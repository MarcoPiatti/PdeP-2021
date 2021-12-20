data Persona = Persona
    { nombre          :: String
    , calorias        :: Int
    , hidratacion     :: Int
    , tiempo          :: Int
    , equipamientos   :: [Equipamiento]
    }

type Equipamiento = String

--------------------------------------------------------------------------------------
--helpers de accessors

mapNombre :: (String -> String) -> Persona -> Persona
mapNombre f unaPersona = unaPersona { nombre = f . nombre $ unaPersona }

mapEquipamientos :: ([Equipamiento] -> [Equipamiento]) -> Persona -> Persona
mapEquipamientos f unaPersona = unaPersona { equipamientos = f . equipamientos $ unaPersona }

type MapInt = (Int -> Int) -> Persona -> Persona

mapCalorias :: MapInt
mapCalorias f unaPersona = unaPersona { calorias = f . calorias $ unaPersona }

mapHidratacion :: MapInt
mapHidratacion f unaPersona = unaPersona { hidratacion = f . hidratacion $ unaPersona }

mapTiempo :: MapInt
mapTiempo f unaPersona = unaPersona { tiempo = f . tiempo $ unaPersona }

subirCalorias :: Int -> Persona -> Persona
subirCalorias = mapCalorias . (+)

bajarCalorias :: Int -> Persona -> Persona
bajarCalorias = mapCalorias . subtract

multiplicarCalorias :: Int -> Persona -> Persona
multiplicarCalorias = mapCalorias . (*)

dividirCalorias :: Int -> Persona -> Persona
dividirCalorias = mapCalorias . flip div

multiplicarHidratacion :: Int -> Persona -> Persona
multiplicarHidratacion = mapHidratacion . ((max 100) .) . (*)

bajarHidratacion :: Int -> Persona -> Persona
bajarHidratacion = mapHidratacion . subtract

establecerHidratacion :: Int -> Persona -> Persona
establecerHidratacion = mapHidratacion . const

agregarEquipamiento :: Equipamiento -> Persona -> Persona
agregarEquipamiento = mapEquipamientos . (:)

establecerEquipamientos :: [Equipamiento] -> Persona -> Persona
establecerEquipamientos = mapEquipamientos . const

--------------------------------------------------------------------------------------

type Ejercicio = Persona -> Persona

abdominales :: Int -> Ejercicio
abdominales repeticiones unaPersona = bajarCalorias ( 8 * repeticiones ) unaPersona

flexiones :: Int -> Ejercicio
flexiones repeticiones unaPersona = bajarHidratacion ( 2 * div repeticiones 10 ) . bajarCalorias ( 16 * repeticiones ) $ unaPersona

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas repeticiones peso unaPersona
    | tieneEquipamiento "pesa" unaPersona = bajarHidratacion ( peso * div repeticiones 10 ) . bajarCalorias ( 32 * repeticiones ) $ unaPersona
    | otherwise = unaPersona

tieneEquipamiento :: Equipamiento -> Persona -> Bool
tieneEquipamiento articulo = elem articulo . equipamientos 

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson = id

--------------------------------------------------------------------------------------

type Accion = Persona -> Persona

renovarEquipo :: Accion
renovarEquipo unaPersona = mapEquipamientos (map ("Nuevo " ++)) $ unaPersona

volverseYoguista :: Accion
volverseYoguista unaPersona = agregarEquipamiento "colchoneta" . venderTodo . multiplicarHidratacion 2 . dividirCalorias 2 $ unaPersona

venderTodo :: Accion
venderTodo = establecerEquipamientos []

volverseBodyBuilder :: Accion
volverseBodyBuilder unaPersona
    | todosSusEquipamientosSon "pesa" unaPersona = multiplicarCalorias 3 . mapNombre (++ "BB") $ unaPersona
    | otherwise = unaPersona

todosSusEquipamientosSon :: Equipamiento -> Persona -> Bool
todosSusEquipamientosSon articulo unaPersona = all (== articulo) . equipamientos $ unaPersona

comerUnSandwich :: Accion
comerUnSandwich = establecerHidratacion 100 . subirCalorias 500

------------------------------------------------------------------------------------------------------

data Rutina = Rutina 
    {   duracion :: Int
    ,   ejercicios :: [Ejercicio]
    }

puedeHacerRutina :: Rutina -> Persona -> Bool
puedeHacerRutina rutina unaPersona = (>= duracion rutina) . tiempo $ unaPersona

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina unaRutina unaPersona 
    |   puedeHacerRutina unaRutina unaPersona = foldl (flip ($)) unaPersona (ejercicios unaRutina)
    |   otherwise = unaPersona

esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa unaRutina unaPersona = estaAgotada . hacerRutina unaRutina $ unaPersona

estaAgotada :: Persona -> Bool
estaAgotada unaPersona = cumpleLasSiguientes [ (< 50) . calorias, (< 10) . hidratacion] unaPersona

cumpleLasSiguientes :: [a -> Bool] -> a -> Bool
cumpleLasSiguientes listaDeCondiciones unValor = all ($ unValor) listaDeCondiciones

esBalanceada :: Rutina -> Persona -> Bool
esBalanceada unaRutina unaPersona = cumpleLasSiguientes [ (> 80) . hidratacion, (< div (calorias unaPersona) 2) . calorias ] . hacerRutina unaRutina $ unaPersona

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina
    {   duracion = 2
    ,   ejercicios = map abdominales [1..]
    }

type Grupo = [Persona]

seleccionarGrupoDeEjercicio :: Persona -> Grupo -> Grupo
seleccionarGrupoDeEjercicio unaPersona grupoDePersonas = filter (tieneMismoTiempoDisponibleQue unaPersona) grupoDePersonas

tieneMismoTiempoDisponibleQue :: Persona -> Persona -> Bool
tieneMismoTiempoDisponibleQue unaPersona otraPersona = tiempo unaPersona == tiempo otraPersona

promedioDeRutina :: Rutina -> Grupo -> (Int, Int)
promedioDeRutina unaRutina grupoDePersonas = generarPromedios . hacerRutinaGrupal unaRutina $ grupoDePersonas

hacerRutinaGrupal :: Rutina -> Grupo -> Grupo
hacerRutinaGrupal unaRutina grupoDePersonas = map (hacerRutina unaRutina) grupoDePersonas

generarPromedios :: Grupo -> (Int, Int)
generarPromedios grupoDePersonas = mapDupla promedio . pamDupla (map calorias, map hidratacion) $ grupoDePersonas

promedio :: [Int] -> Int
promedio lista = div (sum lista) . length $ lista

pamDupla :: (a -> b, a -> c) -> a -> (b, c)
pamDupla (f, y) unValor = (f unValor, y unValor)

mapDupla :: (a -> b) -> (a, a) -> (b, b)
mapDupla f (unValor, otroValor) = (f unValor, f otroValor)