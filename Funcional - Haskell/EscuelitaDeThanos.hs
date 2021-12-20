data Guantelete = Guantelete
    { material :: String
    , gemas :: [Gema]
    }

data Personaje = Personaje
    { nombre      :: String
    , edad        :: Int
    , energia     :: Int
    , habilidades :: [Habilidad]
    , planeta     :: Planeta
    }

type Habilidad = String
type Planeta   = String
type Universo  = [Personaje]

--------------------------------------------------------------------------------------
--helpers

mapEnergia :: (Int -> Int) -> Personaje -> Personaje
mapEnergia f unPersonaje = unPersonaje { energia = f . energia $ unPersonaje }

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Personaje -> Personaje
mapHabilidades f unPersonaje = unPersonaje { habilidades = f . habilidades $ unPersonaje }

mapPlaneta :: (Planeta -> Planeta) -> Personaje -> Personaje
mapPlaneta f unPersonaje = unPersonaje { planeta = f . planeta $ unPersonaje }

mapEdad :: (Int -> Int) -> Personaje -> Personaje
mapEdad f unPersonaje = unPersonaje { edad = f . edad $ unPersonaje }

-------------------------------------------------------------------------------------

estaCompleto :: Guantelete -> Bool
estaCompleto = (== 6) . length . take 6 . gemas

chasquido :: Guantelete -> Universo -> Universo
chasquido unGuantelete unUniverso
    | estaCompleto unGuantelete = take ((`div` 2) . length $ unUniverso) unUniverso 
    | otherwise                 = unUniverso

--------------------------------------------------------------------------------------

esMenorA45 :: Personaje -> Bool
esMenorA45 = (< 45) . edad

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any esMenorA45

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia . filter ((> 1) . length . habilidades)

-------------------------------------------------------------------------------------

type Gema = Personaje -> Personaje

gemaDeLaMente :: Int -> Gema
gemaDeLaMente = mapEnergia . subtract

eliminarHabilidadSiLaPosee :: Habilidad -> Personaje -> Personaje
eliminarHabilidadSiLaPosee = mapHabilidades . filter . (==)

gemaDelAlma :: Habilidad -> Gema
gemaDelAlma = (gemaDeLaMente 10 .) . eliminarHabilidadSiLaPosee

transportarA :: Planeta -> Personaje -> Personaje
transportarA = mapPlaneta . const

gemaDelEspacio :: Planeta -> Gema
gemaDelEspacio = (gemaDeLaMente 20 .) . transportarA

vaciarEnergia :: Personaje -> Personaje
vaciarEnergia = mapEnergia $ const 0

vaciarHabilidades :: Personaje -> Personaje
vaciarHabilidades = mapHabilidades $ const []

quitarHabilidadesSiTieneDosOMenos :: Personaje -> Personaje
quitarHabilidadesSiTieneDosOMenos unPersonaje
    | (<= 2) . length . habilidades $ unPersonaje = vaciarHabilidades unPersonaje
    | otherwise                                   = unPersonaje

gemaDelPoder :: Gema
gemaDelPoder = quitarHabilidadesSiTieneDosOMenos . vaciarEnergia

gemaDelTiempo :: Gema
gemaDelTiempo = gemaDeLaMente 50 . mapEdad (max 18 . (`div` 2))

--------------------------------------------------------------------------------------

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete
    { material = "Goma"
    , gemas = [gemaDelTiempo, gemaDelAlma "usar Mjolnir", gemaLoca $ gemaDelAlma "programacion en Haskell"]
    }

gemaLoca :: Gema -> Gema
gemaLoca = ((!! 2) .) . iterate

--------------------------------------------------------------------------------------

utilizar :: [Gema] -> Gema
utilizar = foldr1 (.)

--------------------------------------------------------------------------------------

menorSegun :: (Gema -> Int) -> Gema -> Gema -> Gema
menorSegun f unaGema otraGema
    | f unaGema < f otraGema = unaGema
    | otherwise              = otraGema

minimoSegun :: (Gema -> Int) -> [Gema] -> Gema
minimoSegun _ [] = id
minimoSegun _ (unaGema : []) = unaGema
minimoSegun f (primerGema : otrasGemas) = menorSegun f primerGema (minimoSegun f otrasGemas)  

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa unaPersona = minimoSegun (energia . ($ unaPersona)) . gemas

--------------------------------------------------------------------------------------

-- f (g x y) = (f .) . g
