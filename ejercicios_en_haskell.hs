import Data.List (nub)
import Data.Char (toUpper)

-- 1) Aplicar descuento e IVA a un precio
aplicarDescuento :: Double -> Double -> Double
aplicarDescuento precio porcentaje = precio * (1 - porcentaje / 100)

aplicarIVA :: Double -> Double -> Double
aplicarIVA precio porcentaje = precio * (1 + porcentaje / 100)

aplicarACesta :: [(Double, Double)] -> (Double -> Double -> Double) -> Double
aplicarACesta cesta funcion = sum [funcion precio porcentaje | (precio, porcentaje) <- cesta]

-- 2) Aplicar una función a cada elemento de una lista
aplicarFuncionLista :: (a -> b) -> [a] -> [b]
aplicarFuncionLista f xs = map f xs

-- 3) Obtener un diccionario con las palabras y su longitud
longitudPalabras :: String -> [(String, Int)]
longitudPalabras frase = [(palabra, length palabra) | palabra <- words frase]

-- 4) Convertir notas a calificaciones
calificacion :: Double -> String
calificacion nota
    | nota >= 95 = "Excelente"
    | nota >= 85 = "Notable"
    | nota >= 75 = "Bueno"
    | nota >= 70 = "Suficiente"
    | otherwise = "Desempenio insuficiente"

convertirNotas :: [(String, Double)] -> [(String, String)]
convertirNotas notas = [(map toUpper asignatura, calificacion nota) | (asignatura, nota) <- notas]

-- 5) Calcular módulo de un vector
moduloVector :: [Double] -> Double
moduloVector v = sqrt (sum [x^2 | x <- v])

-- 6) Valores atípicos
media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

desviacionEstandar :: [Double] -> Double
desviacionEstandar xs = sqrt (sum [(x - m) ^ 2 | x <- xs] / fromIntegral (length xs))
  where m = media xs

valoresAtipicos :: [Double] -> [Double]
valoresAtipicos xs = 
  if sd == 0 then []  -- Si la desviación estándar es 0, no hay valores atípicos
  else [x | x <- xs, let z = abs (x - m) / sd, z > 1.5]  -- Se reduce el umbral a 1.5
  where
    m = media xs
    sd = desviacionEstandar xs

