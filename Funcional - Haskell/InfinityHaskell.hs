data Personaje = Personaje {
    nombre :: String,
    poder  :: Int,
    derrotas :: [Derrota],
    equipamientos :: [Equipamiento]
}

data Derrota = Derrota {
    oponente :: String,
    anio     :: Int
}

mapPoder :: (Int -> Int) -> Personaje -> Personaje
mapPoder f unPersonaje = unPersonaje { poder = f . poder $ unPersonaje }

mapNombre :: (String -> String) -> Personaje -> Personaje
mapNombre f unPersonaje = unPersonaje { nombre = f . nombre $ unPersonaje }

mapDerrotas :: ([Derrota] -> [Derrota]) -> Personaje -> Personaje
mapDerrotas f unPersonaje = unPersonaje { derrotas = f . derrotas $ unPersonaje }

multiplicarPoder :: Int -> Personaje -> Personaje
multiplicarPoder = mapPoder . (*)

sumarPoder :: Int -> Personaje -> Personaje
sumarPoder = mapPoder . (+)

restarPoder :: Int -> Personaje -> Personaje
restarPoder = mapPoder . subtract

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento grupoDePersonajes = map (multiplicarPoder $ length grupoDePersonajes) grupoDePersonajes

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos personajes = filter esRivalDigno . entrenamiento $ personajes

esRivalDigno :: Personaje -> Bool
esRivalDigno unPersonaje = cumpleLasSiguientes [ (> 500) . poder, algunaDerrotaSeLlama "Hijo de Thanos" ] unPersonaje

cumpleLasSiguientes :: [a -> Bool] -> a -> Bool
cumpleLasSiguientes listaDeCondiciones unValor = all ($ unValor) listaDeCondiciones

algunaDerrotaSeLlama :: String -> Personaje -> Bool
algunaDerrotaSeLlama nombreOponente = any ((== nombreOponente) . oponente) . derrotas

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil anioDePelea unosPersonajes otrosPersonajes = zipWith (pelea anioDePelea) unosPersonajes otrosPersonajes

pelea :: Int -> Personaje -> Personaje -> Personaje
pelea anioDePelea unPersonaje otroPersonaje
    | unPersonaje `esMasFuerteQue` otroPersonaje = derrotar anioDePelea unPersonaje otroPersonaje
    | otherwise                                  = derrotar anioDePelea otroPersonaje unPersonaje

esMasFuerteQue :: Personaje -> Personaje -> Bool
esMasFuerteQue unPersonaje otroPersonaje = poder unPersonaje > poder otroPersonaje

derrotar :: Int -> Personaje -> Personaje -> Personaje
derrotar anioDePelea ganador perdedor = agregarDerrota (Derrota (nombre perdedor) anioDePelea) ganador

agregarDerrota :: Derrota -> Personaje -> Personaje
agregarDerrota = mapDerrotas . (:)

agregarDerrotas :: [Derrota] -> Personaje -> Personaje
agregarDerrotas = mapDerrotas . (++)

type Equipamiento = Personaje -> Personaje

escudo :: Equipamiento
escudo unPersonaje
    | (< 5) . length . derrotas $ unPersonaje = sumarPoder 50 unPersonaje
    | otherwise                               = restarPoder 100 unPersonaje

trajeMecanizado :: Int -> Personaje -> Personaje
trajeMecanizado version unPersonaje = mapNombre ((++ show version) . (++" V") . ("Iron "++)) unPersonaje

es :: Personaje -> String -> Bool
es unPersonaje otroPersonaje = nombre unPersonaje == otroPersonaje

usarSiEs :: String -> (Personaje -> Personaje) -> Personaje -> Personaje
usarSiEs unNombre unaFuncion unPersonaje
    | unPersonaje `es` unNombre = unaFuncion unPersonaje
    | otherwise                 = unPersonaje

stormBreaker :: Personaje -> Personaje
stormBreaker = usarSiEs "Thor" $ limpiarDerrotas . mapNombre (++ "dios del trueno")

limpiarDerrotas :: Personaje -> Personaje
limpiarDerrotas = mapDerrotas . const $ []

gemaDelAlma :: Personaje -> Personaje
gemaDelAlma = usarSiEs "Thanos" $ agregarDerrotas extras

extras :: [Derrota]
extras = map (\numero -> Derrota { oponente = "extra numero "++show numero, anio = 2017+numero } ) [1..]
