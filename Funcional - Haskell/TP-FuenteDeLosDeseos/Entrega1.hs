module Entrega1 where


type Sueno = Persona -> Persona 

data Persona = Persona{
    edad :: Int, 
    suenos :: [Sueno],
    nombre :: String,
    habilidades :: [String],
    felicidonios :: Int
}

-- Helpers
mapFelicidonios :: (Int -> Int) -> Persona -> Persona
mapFelicidonios funcionFelicidonios unaPersona = unaPersona { felicidonios = funcionFelicidonios.felicidonios $ unaPersona }

mapHabilidades :: ([String] -> [String]) -> Persona -> Persona
mapHabilidades funcionHabilidades unaPersona = unaPersona { habilidades = funcionHabilidades.habilidades $ unaPersona }

mapEdad :: (Int -> Int) -> Persona -> Persona
mapEdad funcionEdad unaPersona = unaPersona { edad = funcionEdad.edad $ unaPersona }

perderFelicidonios :: Int -> Sueno
perderFelicidonios unosFelicidonios = mapFelicidonios (subtract unosFelicidonios)

ganarFelicidonios :: Int -> Sueno
ganarFelicidonios unosFelicidonios = mapFelicidonios (+ unosFelicidonios)

obtenerHabilidad :: String -> Sueno
obtenerHabilidad unaHabilidad = mapHabilidades (++ [unaHabilidad])

cumplirAnos :: Sueno
cumplirAnos  = mapEdad (+1)


medirPersonaSegun :: (Int -> Int) -> (Persona -> Int) -> (Persona -> Int) -> (Persona -> Int) -> Persona -> Int
medirPersonaSegun unaFuncion primerAccesor segundoAccesor tercerAccesor unaPersona
    | (> 100) . felicidonios $ unaPersona = (* segundoAccesor unaPersona) . primerAccesor  $ unaPersona
    | (> 50 ) . felicidonios $ unaPersona = (* tercerAccesor unaPersona ) . primerAccesor  $ unaPersona
    | otherwise                           = unaFuncion . primerAccesor $ unaPersona


-- Punto 1 parte a
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona = medirPersonaSegun (`div` 2) felicidonios edad (length.suenos) unaPersona

-- Punto 1 parte b
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona = medirPersonaSegun (* 2) (length.suenos) felicidonios edad unaPersona

-- Punto 2 parte a
nombreLargo :: Persona -> Bool
nombreLargo = (>10) . length . nombre

-- Punto 2 parte b
personaSuertuda :: Persona -> Bool
personaSuertuda = even . (*3) . coeficienteDeSatisfaccion

-- Punto 2 parte c
nombreLindo :: Persona -> Bool
nombreLindo = (=='a') . last . nombre

--Punto 3
recibirseDeUnaCarrera :: String -> Sueno
recibirseDeUnaCarrera unaCarrera = ganarFelicidonios ((*1000) . length $ unaCarrera) . obtenerHabilidad unaCarrera

viajarAUnaListaDeCiudades :: [String] -> Sueno
viajarAUnaListaDeCiudades listaDeCiudades = cumplirAnos . ganarFelicidonios ((*100) . length $ listaDeCiudades)

enamorarseDeOtraPersona :: Persona -> Sueno
enamorarseDeOtraPersona otraPersona = ganarFelicidonios (felicidonios otraPersona)

queTodoSigaIgual :: Sueno
queTodoSigaIgual = id

comboPerfecto :: Sueno
comboPerfecto = viajarAUnaListaDeCiudades ["Berazategui", "Paris"] . recibirseDeUnaCarrera "Medicina" . ganarFelicidonios 100