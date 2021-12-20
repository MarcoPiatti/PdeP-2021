data Persona = Persona 
    { nombre :: String
    , direccion :: String
    , dinero :: Float
    , comida :: Comida
    , cupones :: [Cupon]
    }

data Comida = Comida
    { nombreComida :: String
    , costo :: Float
    , ingredientes :: [String]
    }

paula :: Persona
paula = Persona
    { nombre = "Paula"
    , direccion = "Thames 1585"
    , dinero = 3600
    , comida = hamburguesaDeluxe
    , cupones = []
    }

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida
    { nombreComida = "hamburguesa deluxe"
    , costo = 350
    , ingredientes = ["pan", "carne", "lechuga", "tomate", "panceta", "queso", "huevo frito"]
    }

marcoPiatti :: Persona
marcoPiatti = Persona
    { nombre = "Marco Piatti"
    , direccion = "Calle Falsa 123"
    , dinero = 9999
    , comida = zapallosEnAlmibar
    , cupones = [ findeVegetariano, semanaVegana]
    }

zapallosEnAlmibar :: Comida
zapallosEnAlmibar = Comida
    { nombreComida = "zapallos en almibar"
    , costo = 100
    , ingredientes = ["zapallo", "agua", "cal", "azucar"]
    }


mapDinero :: (Float -> Float) -> Persona -> Persona
mapDinero modificadorDinero unaPersona = unaPersona { dinero = modificadorDinero.dinero $ unaPersona }

quitarDinero :: Float -> Persona -> Persona
quitarDinero monto = mapDinero (subtract monto) 

mapComida :: (Comida -> Comida) -> Persona -> Persona
mapComida modificadorComida unaPersona = unaPersona { comida = modificadorComida.comida $ unaPersona }

nuevaComidaFavorita :: Comida -> Persona -> Persona
nuevaComidaFavorita unaComida = mapComida (const unaComida)

comprar :: Comida -> Persona -> Persona
comprar unaComida unaPersona
    | dinero unaPersona >= costo unaComida && costo unaComida < 200 = nuevaComidaFavorita unaComida . quitarDinero (costo unaComida) $ unaPersona
    | dinero unaPersona >= costo unaComida                          = quitarDinero (costo unaComida) unaPersona
    | otherwise                                                     = unaPersona

carritoDeCompras :: [Comida] -> Persona -> Persona --Preguntar por el reemplazo de la comida favorita cuando son muchas
carritoDeCompras comidas = foldr1 (.) (map comprar comidas) . quitarDinero 100

type Cupon = Comida -> Comida

mapIngredientes :: ([String] -> [String]) -> Comida -> Comida
mapIngredientes modificadorIngredientes unaComida = unaComida { ingredientes = modificadorIngredientes.ingredientes $ unaComida }

mapCosto :: (Float -> Float) -> Comida -> Comida
mapCosto modificadorCosto unaComida = unaComida { costo = modificadorCosto.costo $ unaComida }

multiplicadorCosto :: Float -> Float
multiplicadorCosto factor = (100 - factor) / 100

reducirCostoPorciento :: Float -> Comida -> Comida
reducirCostoPorciento factor = mapCosto (* multiplicadorCosto factor)

mapNombreComida :: (String -> String) -> Comida -> Comida
mapNombreComida modificadorNombre unaComida = unaComida { nombreComida = modificadorNombre.nombreComida $ unaComida}

noContiene :: [String] -> Comida -> Bool
noContiene productos = all (`notElem` productos) . ingredientes

descuentoPorFaltaDeProductos :: [String] -> Float -> Cupon
descuentoPorFaltaDeProductos productos porcentaje unaComida
    | noContiene productos unaComida = reducirCostoPorciento porcentaje unaComida
    | otherwise                      = unaComida

semanaVegana :: Cupon
semanaVegana = descuentoPorFaltaDeProductos ["carne", "huevos", "queso"] 50

esoNoEsCocaPapi :: String -> Cupon
esoNoEsCocaPapi unaBebida = mapIngredientes ( unaBebida : ) . mapNombreComida (++ " Party")

sinTACCis :: Cupon
sinTACCis = mapIngredientes (map (++ " libre de gluten"))

findeVegetariano :: Cupon
findeVegetariano = descuentoPorFaltaDeProductos ["carne"] 30

largaDistiancia :: Cupon
largaDistiancia = mapIngredientes (filter ((<=10) . length)) . mapCosto (+50)

comprarConCupones :: Comida -> Persona -> Persona
comprarConCupones unaComida unaPersona = comprar  ( ($ unaComida) . foldr1 (.) . cupones $ unaPersona ) unaPersona

noEsVocal :: Char -> Bool
noEsVocal letra = notElem letra "aeiouAEIOU"

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos (cabeza : cola) = (cabeza :) . quitarRepetidos . filter (/= cabeza) $ cola

superComida :: [Comida] -> Comida
superComida comidas = Comida 
    { nombreComida  = foldl1 (++) . map (filter noEsVocal . nombreComida) $ comidas
    , costo         = sum . map costo $ comidas
    , ingredientes  = quitarRepetidos . foldl1 (++) . map ingredientes $ comidas
    }

compraDeluxe :: [Comida] -> Persona -> Persona
compraDeluxe comidas = comprar . superComida . map (mapCosto (*2)) . filter ((< 400) . costo) $ comidas