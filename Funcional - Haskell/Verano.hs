data Turista = Turista
    {   cansancio   :: Int
    ,   stress      :: Int
    ,   viajaSolo   :: Bool
    ,   idiomas     :: [Idioma]
    } deriving(Show, Eq)

type Idioma = String

ana :: Turista
ana = Turista 0 21 False ["espanol"]

beto :: Turista
beto = Turista 15 15 True ["aleman"]

cathi :: Turista
cathi = Turista 15 15 True ["aleman","catalan"]

--------------------------------------------------------------------------------------
--helpers de accessors

mapCansancio :: (Int -> Int) -> Turista -> Turista
mapCansancio f unTurista = unTurista { cansancio = f . cansancio $ unTurista }

mapStress :: (Int -> Int) -> Turista -> Turista
mapStress f unTurista = unTurista { stress = f . stress $ unTurista }

mapIdiomas :: ([Idioma] -> [Idioma]) -> Turista -> Turista
mapIdiomas f unTurista = unTurista { idiomas = f . idiomas $ unTurista }

mapViajaSolo :: (Bool -> Bool) -> Turista -> Turista
mapViajaSolo f unTurista = unTurista { viajaSolo = f . viajaSolo $ unTurista }

subirCansancio :: Int -> Turista -> Turista
subirCansancio = mapCansancio . (+)

bajarCansancio :: Int -> Turista -> Turista
bajarCansancio = mapCansancio . subtract

multiplicarCansancio :: Int -> Turista -> Turista
multiplicarCansancio = mapCansancio . (*)

dividirCansancio :: Int -> Turista -> Turista
dividirCansancio = mapCansancio . flip div

subirStress :: Int -> Turista -> Turista
subirStress = mapStress . (+)

bajarStress :: Int -> Turista -> Turista
bajarStress = mapStress . subtract

multiplicarStress :: Int -> Turista -> Turista
multiplicarStress = mapStress . (*)

establecerStress :: Int -> Turista -> Turista
establecerStress = mapStress . const

aprenderIdioma :: Idioma -> Turista -> Turista
aprenderIdioma = mapIdiomas . (:)

establecerIdiomas :: [Idioma] -> Turista -> Turista
establecerIdiomas = mapIdiomas . const

sigueViajandoSolo :: Bool -> Turista -> Turista
sigueViajandoSolo = mapViajaSolo . const

--------------------------------------------------------------------------------------

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista   = bajarCansancio 5 unTurista
    | otherwise             = bajarStress 1 unTurista

apreciarAlgunElementoDelPaisaje :: String -> Excursion
apreciarAlgunElementoDelPaisaje = bajarStress . length

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma unIdioma unTurista = sigueViajandoSolo False . aprenderIdioma unIdioma $ unTurista

caminar :: Int -> Excursion
caminar minutos unTurista = bajarStress (intensidad minutos) . subirCansancio (intensidad minutos) $ unTurista

intensidad :: Int -> Int
intensidad minutos = div minutos 4

data Marea = Fuerte | Moderada | Tranquila

paseoEnBarco :: Marea -> Turista -> Turista
paseoEnBarco Fuerte unTurista   = subirCansancio 10 . subirStress 6 $ unTurista
paseoEnBarco Moderada unTurista = unTurista
paseoEnBarco Tranquila unTurista = salirAHablarUnIdioma "aleman" . apreciarAlgunElementoDelPaisaje "mar" . caminar 10 $ unTurista

------------------------------------------------------------------------------------------

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista = bajarStressPorciento 10 . unaExcursion $ unTurista

bajarStressPorciento :: Int -> Turista -> Turista
bajarStressPorciento porcentaje unTurista = bajarStress(stress unTurista * porciento porcentaje) unTurista

porciento :: Int -> Int
porciento = (`div` 100)

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice unTurista unaExcursion = (flip $ deltaSegun indice) unTurista . hacerExcursion unaExcursion $ unTurista

esEducativa :: Turista -> Excursion -> Bool
esEducativa unTurista unaExcursion = (> 0) . deltaExcursionSegun (length.idiomas) unTurista $ unaExcursion

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista unaExcursion = (<= 3) . deltaExcursionSegun stress unTurista $ unaExcursion

-------------------------------------------------------------------------------------------------------

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarAlgunElementoDelPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB unaExcursion = [paseoEnBarco Tranquila, unaExcursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina Fuerte = [paseoEnBarco Fuerte, apreciarAlgunElementoDelPaisaje "lago", paseoEnBarco Fuerte]
islaVecina marea  = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]

hacerTour :: Tour -> Turista -> Turista
hacerTour unTour unTurista = completarTour unTour . subirStress (length unTour) $ unTurista

completarTour :: Tour -> Turista -> Turista
completarTour unTour unTurista = foldl (flip ($)) unTurista unTour

existeAlgunTourConveniente :: [Tour] -> Turista -> Bool
existeAlgunTourConveniente tours unTurista = any (flip esConveniente $ unTurista) tours

esConveniente :: Tour -> Turista -> Bool
esConveniente unTour unTurista = algunaCumpleLasSiguientes [esDesestresante unTurista, loDejaAcompaniado unTurista] unTour

loDejaAcompaniado :: Turista -> Excursion -> Bool
loDejaAcompaniado unTurista unaExcursion = not . viajaSolo . hacerExcursion unaExcursion $ unTurista

cumpleLasSiguientes :: [a -> Bool] -> a -> Bool
cumpleLasSiguientes listaDeCondiciones unValor = all ($ unValor) listaDeCondiciones

algunaCumpleLasSiguientes :: [a -> Bool] -> [a] -> Bool
algunaCumpleLasSiguientes listaDeCondiciones listaDeValores = any (cumpleLasSiguientes listaDeCondiciones) listaDeValores

efectividadDeTour :: Tour -> [Turista] -> Int
efectividadDeTour unTour turistas = sum . map (espiritualidad unTour) . filter (esConveniente unTour) $ turistas

espiritualidad :: Tour -> Turista -> Int
espiritualidad unTour unTurista = deltaExcursionSegun stress unTurista (hacerTour unTour) + deltaExcursionSegun cansancio unTurista (hacerTour unTour)

--------------------------------------------------------------------------------------------------------

infinitasPlayas :: Tour
infinitasPlayas = repeat irALaPlaya

{-- B.
    Para que un tour sea convincente hay que revisar que alguna excursion cumpla ciertas condiciones,
    como todas las excursiones son irALaPlaya, basta con evaluar esta funcion en particular para ana y beto.
    Si retorna true, el any dentro de esConveniente va a cortar a la primera, si retorna false nunca corta.

    *Main> esDesestresante ana irALaPlaya
    True   
    *Main> loDejaAcompaniado ana irALaPlaya
    True

    *Main> esDesestresante beto irALaPlaya 
    True   
    *Main> loDejaAcompaniado beto irALaPlaya
    False

    Como para ana retornan ambas condiciones true, infinitasPlayas le es Conveniente,
    pero para beto no, ya que no ambas condiciones evaluan a true.

    *Main> esConveniente infinitasPlayas ana
    True

    la de beto se queda colgada.

    C.
    Nunca se puede conocer la efectividad de infinitasPlayas, ya que es imposible
    saber el deltaExcursionSegun de realizar un tour infinito, necesariamente debe poder marcarse
    un antes y un despues, pero el tour nunca termina.
    
--}