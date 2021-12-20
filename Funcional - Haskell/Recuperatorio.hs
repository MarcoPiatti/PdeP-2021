----------------------------------------------------------------------------------
-- Parte 1

-- 1 . A
data Peleador = Peleador {
    vida :: Int,
    resistencia :: Int,
    ataques :: [Ataque]
}

type Ataque = Peleador -> Peleador

mapVida :: (Int -> Int) -> Peleador -> Peleador
mapVida f unPeleador = unPeleador { vida = f . vida $ unPeleador }

mapAtaques :: ([Ataque] -> [Ataque]) -> Peleador -> Peleador
mapAtaques f unPeleador = unPeleador { ataques = f . ataques $ unPeleador }

reducirVida :: Int -> Peleador -> Peleador
reducirVida = mapVida . subtract

setVida :: Int -> Peleador -> Peleador
setVida = mapVida . const

dividirVida :: Int -> Peleador -> Peleador
dividirVida = mapVida . flip div

-- 1 . B . i
estaMuerto :: Peleador -> Bool
estaMuerto unPeleador = (< 1) . vida $ unPeleador

-- 1 . B . ii
esHabil :: Peleador -> Bool
esHabil unPeleador = (> 10) . length . ataques $ unPeleador

-- 1 . C . i
golpe :: Int -> Ataque
golpe intensidad oponente = reducirVida ( intensidad `div` resistencia oponente ) oponente

-- 1 . C . ii
toqueDeLaMuerte :: Ataque
toqueDeLaMuerte oponente = setVida 0 oponente

-- 1 . C . iii
patada :: String -> Ataque
patada "el pecho"  = patadaEnElPecho
patada "la carita" = dividirVida 2
patada "la nuca"   = olvidarPrimerAtaque
patada _           = id

patadaEnElPecho :: Ataque
patadaEnElPecho oponente
    |  estaMuerto oponente = reanimarCorazon oponente
    |  otherwise           = reducirVida 10 oponente

reanimarCorazon :: Ataque
reanimarCorazon unPeleador = setVida 1 unPeleador

olvidarPrimerAtaque :: Ataque
olvidarPrimerAtaque unPeleador = mapAtaques removerPrimerAtaqueSiTiene unPeleador

removerPrimerAtaqueSiTiene :: [Ataque] -> [Ataque]
removerPrimerAtaqueSiTiene []             = []
removerPrimerAtaqueSiTiene listaDeAtaques = tail listaDeAtaques

-- 1 . D
bruceLee :: Peleador
bruceLee = Peleador {
    vida = 200,
    resistencia = 25,
    ataques = [toqueDeLaMuerte, golpe 500, patadas 3 "la carita"]
}

patadas :: Int -> String -> Ataque
patadas cantidad parteGolpeada = foldl1 (.) . replicate cantidad $ patada parteGolpeada

-----------------------------------------------------------------------------------
-- Parte 2

-- 2 . A
terrible :: Ataque -> [Peleador] -> Bool
terrible unAtaque enemigos = (< length enemigos `div` 2) . length . sobrevivientes . map unAtaque $ enemigos

sobrevivientes :: [Peleador] -> [Peleador]
sobrevivientes grupoDePeleadores = filter (not . estaMuerto) $ grupoDePeleadores

-- 2 . B
peligroso :: Peleador -> [Peleador] -> Bool
peligroso unPeleador enemigos = todosSonTerriblesPara (habiles enemigos) . ataques $ unPeleador

todosSonTerriblesPara :: [Peleador] -> [Ataque] -> Bool
todosSonTerriblesPara enemigos unosAtaques = all (flip terrible enemigos) unosAtaques

habiles :: [Peleador] -> [Peleador]
habiles enemigos = filter esHabil enemigos

-- 2 . C
mejorAtaque :: Peleador -> Peleador -> Ataque
mejorAtaque unPeleador enemigo = minimoSegun (vidaDespuesDeSerAtacado enemigo) . ataques $ unPeleador

vidaDespuesDeSerAtacado :: Peleador -> Ataque -> Int
vidaDespuesDeSerAtacado enemigo unAtaque = vida . unAtaque $ enemigo

minimoSegun :: Ord b => (a -> b) -> [a] -> a
minimoSegun f valores = foldl1 (menorSegun f) valores

menorSegun :: Ord b => (a -> b) -> a -> a -> a
menorSegun f unValor otroValor
    | f unValor < f otroValor = unValor
    | otherwise               = otroValor

-- 2 . D
invencible :: Peleador -> [Peleador] -> Bool
invencible unPeleador enemigos = (== vida unPeleador) . vida . realizarMejoresAtaquesSobre unPeleador $ enemigos

realizarMejoresAtaquesSobre :: Peleador -> [Peleador] -> Peleador
realizarMejoresAtaquesSobre unPeleador enemigos = realizarAtaquesSobre unPeleador . mejoresAtaques unPeleador $ enemigos

realizarAtaquesSobre :: Peleador -> [Ataque] -> Peleador
realizarAtaquesSobre unPeleador unosAtaques = foldr ($) unPeleador unosAtaques

mejoresAtaques :: Peleador -> [Peleador] -> [Ataque]
mejoresAtaques unPeleador enemigos = map (flip mejorAtaque unPeleador) enemigos