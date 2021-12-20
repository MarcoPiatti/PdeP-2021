data Propiedad = Propiedad
    { nombrePropiedad :: String
    , precio :: Int
    }

type Accion = Jugador -> Jugador

data Jugador = Jugador
    { nombre        :: String
    , dinero        :: Int
    , tactica       :: String
    , propiedades   :: [Propiedad]
    , acciones      :: [Accion]
    }

carolina :: Jugador
carolina = Jugador
    { nombre        = "Carolina"
    , dinero        = 500
    , tactica       = "Accionista"
    , propiedades   = []
    , acciones      = [pagarAAccionistas, pasarPorElBanco]
    }
    
manuel :: Jugador
manuel = Jugador
    { nombre        = "Manuel"
    , dinero        = 500
    , tactica       = "Oferente singular"
    , propiedades   = []
    , acciones      = [enojarse, pasarPorElBanco]
    }

-- Setters para Jugador
mapNombre :: (String -> String) -> Accion
mapNombre modificadorNombre unJugador = unJugador { nombre = modificadorNombre.nombre $ unJugador }

mapDinero :: (Int -> Int) -> Accion
mapDinero modificadorDinero unJugador = unJugador { dinero = modificadorDinero.dinero $ unJugador }

agregarDinero :: Int -> Accion
agregarDinero dinero unJugador = mapDinero (+ dinero) unJugador
quitarDinero :: Int -> Accion
quitarDinero dinero unJugador = mapDinero (subtract dinero) unJugador

setTactica :: String -> Accion
setTactica nuevaTactica unJugador = unJugador { tactica = nuevaTactica }

mapPropiedades :: ([Propiedad] -> [Propiedad]) -> Accion
mapPropiedades modificadorPropiedades unJugador = unJugador { propiedades = modificadorPropiedades.propiedades $ unJugador }

mapAcciones :: ([Accion] -> [Accion]) -> Accion
mapAcciones modificadorAcciones unJugador = unJugador { acciones = modificadorAcciones.acciones $ unJugador }

--Funciones del TP
pasarPorElBanco :: Accion
pasarPorElBanco = (setTactica "Comprador compulsivo").(mapDinero (+40))

enojarse :: Accion
enojarse = (mapAcciones (gritar :)).(mapDinero (+50))

gritar :: Accion
gritar = mapNombre ("AHHHH" ++)

comprarPropiedad :: Propiedad -> Accion
comprarPropiedad unaPropiedad = (mapPropiedades (unaPropiedad :)).(mapDinero (subtract $ precio unaPropiedad))

tieneTacticaSubasta :: Jugador -> Bool
tieneTacticaSubasta unJugador = any (== tactica unJugador) ["Oferente singular", "Accionista"]

puedePagarPropiedad :: Propiedad -> Jugador -> Bool
puedePagarPropiedad unaPropiedad unJugador = dinero unJugador >= precio unaPropiedad

puedeGanarSubasta :: Propiedad -> Jugador -> Bool
puedeGanarSubasta unaPropiedad unJugador = tieneTacticaSubasta unJugador && puedePagarPropiedad unaPropiedad unJugador

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
    | puedeGanarSubasta unaPropiedad unJugador  = comprarPropiedad unaPropiedad unJugador
    | otherwise                                 = unJugador

precioAlquileresTotal :: Jugador -> Int
precioAlquileresTotal = sum.(map precioAlquilerPropiedad).propiedades

precioAlquilerPropiedad :: Propiedad -> Int
precioAlquilerPropiedad unaPropiedad
    | esPropiedadBarata unaPropiedad = 10
    | otherwise                      = 20

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = precio unaPropiedad < 150

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = mapDinero (+ precioAlquileresTotal unJugador) unJugador 

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | tactica unJugador == "Accionista" = agregarDinero 200 unJugador
    | otherwise                         = quitarDinero 100 unJugador

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unJugador
    | puedePagarPropiedad unaPropiedad unJugador = unJugador
    | otherwise                                  = hacerBerrinchePor unaPropiedad (gritar.(agregarDinero 10) $ unJugador)

ultimaRonda :: Jugador -> Accion
ultimaRonda unJugador = foldl1 (.) (acciones unJugador)

jugarUltimaRonda :: Accion
jugarUltimaRonda unJugador = ultimaRonda unJugador unJugador

dineroAlFinal :: Jugador -> Int
dineroAlFinal = dinero.jugarUltimaRonda

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador
    | dineroAlFinal unJugador > dineroAlFinal otroJugador = jugarUltimaRonda unJugador
    | otherwise                                           = jugarUltimaRonda otroJugador