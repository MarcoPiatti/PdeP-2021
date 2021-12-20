data Auto = Auto
    { color :: Color
    , velocidad :: Int
    , distancia :: Int
    } deriving (Show, Eq)

type Color = String
type Carrera = [Auto]

-- 1A
estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto otroAuto = cumpleLasSiguientes [ esDistintoDe otroAuto, (< 10) . distanciaEntre otroAuto ] unAuto

cumpleLasSiguientes :: [a -> Bool] -> a -> Bool
cumpleLasSiguientes listaDeCondiciones unValor = and . pam listaDeCondiciones $ unValor

pam :: [a -> b] -> a -> [b]
pam listaDeFunciones unValor = map ($ unValor) listaDeFunciones

esDistintoDe :: Auto -> Auto -> Bool
esDistintoDe unAuto otroAuto = unAuto /= otroAuto

distanciaEntre :: Auto -> Auto -> Int
distanciaEntre unAuto otroAuto = abs . subtract (distancia otroAuto) . distancia $ unAuto

-- 1B
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = cumpleLasSiguientes [ noTieneANadieCerca unaCarrera, vaGanandoATodos unaCarrera ] unAuto

noTieneANadieCerca :: Carrera -> Auto -> Bool
noTieneANadieCerca unaCarrera unAuto = all (not . estaCerca unAuto) unaCarrera

vaGanandoATodos :: Carrera -> Auto -> Bool
vaGanandoATodos unaCarrera unAuto = all (leVaGanando unAuto) unaCarrera

leVaGanando :: Auto -> Auto -> Bool
leVaGanando unAuto otroAuto = (> distancia otroAuto) . distancia $ unAuto

-- 1C
puesto :: Auto -> Carrera -> Int
puesto unAuto unaCarrera = succ . length . filter (flip leVaGanando unAuto) $ unaCarrera

----------------------------------------------------------------------------------------------

type MapInt = (Int -> Int) -> Auto -> Auto

mapDistancia :: MapInt
mapDistancia f unAuto = unAuto { distancia = f . distancia $ unAuto }

mapVelocidad :: (Int -> Int) -> Auto -> Auto
mapVelocidad f unAuto = unAuto { velocidad = f . velocidad $ unAuto }

correr :: Int -> Auto -> Auto
correr tiempo unAuto = sumarDistancia (distanciaRecorrida tiempo unAuto) unAuto

sumarDistancia :: Int -> Auto -> Auto
sumarDistancia = mapDistancia . (+)

distanciaRecorrida :: Int -> Auto -> Int
distanciaRecorrida tiempo unAuto = (* tiempo) . velocidad $ unAuto

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad velocidadBajada unAuto = mapVelocidad (max 0 . subtract velocidadBajada) unAuto

------------------------------------------------------------------------------------------------

type PowerUp = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- 3A
terremoto :: PowerUp
terremoto unAuto unaCarrera = afectarALosQueCumplen (estaCerca unAuto) (bajarVelocidad 50) unaCarrera

-- 3B
miguelitos :: Int -> PowerUp
miguelitos velocidadBajada unAuto unaCarrera = afectarALosQueCumplen (leVaGanando unAuto) (bajarVelocidad velocidadBajada) unaCarrera

-- 3C
jetPack :: Int -> PowerUp
jetPack duracion unAuto unaCarrera = afectarALosQueCumplen (== unAuto) (establecerVelocidad (velocidad unAuto) . correr duracion . multiplicarVelocidad 2) unaCarrera

multiplicarVelocidad :: Int -> Auto -> Auto
multiplicarVelocidad = mapVelocidad . (*)

establecerVelocidad :: Int -> Auto -> Auto
establecerVelocidad = mapVelocidad . const

type Evento = Carrera -> Carrera

type TablaDePosiciones = [(Int, String)]

--------------------------------------------------------------------------------------------------------------

-- 4A
simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera estadoInicial eventos = generarTablaDePosiciones . simularEventos estadoInicial $ eventos

simularEventos :: Carrera -> [Carrera -> Carrera] -> Carrera
simularEventos estadoInicial eventos = foldl (flip ($)) estadoInicial eventos

generarTablaDePosiciones :: Carrera -> [(Int, Color)]
generarTablaDePosiciones estadoFinal = zip (map (flip puesto estadoFinal) estadoFinal) (map color estadoFinal)

-- 4B
correnTodos :: Int -> Evento
correnTodos tiempo todos = map (correr tiempo) todos

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp powerUp colorDelAuto estadoActual = powerUp (encontrarAutoDeColor colorDelAuto estadoActual) estadoActual

encontrarAutoDeColor :: Color -> Carrera -> Auto
encontrarAutoDeColor colorDelAuto estadoActual = head . filter ((== colorDelAuto) . color) $ estadoActual

carreraDePrueba :: Carrera
carreraDePrueba = [autoRojo, autoBlanco, autoAzul, autoNegro]

-- 4C
powerUpsDePrueba :: [Evento]
powerUpsDePrueba = [correnTodos 30, usaPowerUp (jetPack 3) "Azul", usaPowerUp terremoto "Blanco", correnTodos 40, usaPowerUp (miguelitos 20) "Blanco", usaPowerUp (jetPack 6) "Negro", correnTodos 10]

autoRojo :: Auto
autoRojo = Auto
    {   color = "Rojo"
    ,   velocidad = 120
    ,   distancia = 0
    }

autoAzul :: Auto
autoAzul = Auto
    {   color = "Azul"
    ,   velocidad = 120
    ,   distancia = 0
    }

autoBlanco :: Auto
autoBlanco = Auto
    {   color = "Blanco"
    ,   velocidad = 120
    ,   distancia = 0
    }

autoNegro :: Auto
autoNegro = Auto
    {   color = "Negro"
    ,   velocidad = 120
    ,   distancia = 0
    }

{-- 5A. La solucion lo permite gracias a la currificacion
        cualquier parametro extra que se necesite puede especificarse junto al powerUp
        lo que devolverá un "powerUp puro".

    5B. No es posible para 1c, length no es lazy.
        en el caso del 1b depende. All y And son lazy siempre y cuando puedan encontrar un valor falso, y map es lazy.
        Funcionaría siempre y cuando la lista no evalue en su totalidad a true.
--}