
-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util
import Data.List (zipWith4)

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) =  Histograma l u (replicate (n + 2) 0) --usa
-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma l u lista) =
  if x < l
    then Histograma l u (actualizarElem 0 (+1) lista)       -- por debajo del rango, outliers inferiores
  else if x >= u
    then Histograma l u (actualizarElem (length lista - 1) (+1) lista)  -- el ultimo casillero es para outliers por encima del rango
  else Histograma l u (actualizarElem indice (+1) lista)   -- dentro del rango
  where
    indice = 1 + floor ((x - l) / ancho)  -- usamos la posicion 0 para outliers inferiores, asi que el primer indice es 1
    ancho = (u - l) / fromIntegral (length lista - 2)  -- restamos 2 porque quitamos los valores extremos
    
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma casilleros rango valores = foldr agregar (vacio casilleros rango) valores --Usamos vacio para obtener un histograma vacio con el rango indicado. Luego usamos foldr para recorrer la lista de derecha a izquierda e ir aplicando la funcion agregar

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p


casilleros :: Histograma -> [Casillero]
casilleros (Histograma valorMinimo valorMaximo valores) =
  zipWith4 Casillero limitesInferiores limitesSuperiores valores porcentajes --tomamos la idea del enunciado y usamos zipwith4, vamos a armar 4 listas donde vamos a tener:
  --1-limites inferiores de todos los casilleros
  --2-limites superiores
  --3-la cantidad de elementos de cada casillero
  --4-el porcentaje que representa con respecto al total
  where
    anchoCasillero = (valorMaximo - valorMinimo) / fromIntegral casillerosEnRango  --esta es la longitud de cada intervalo dentro del histograma
    total = fromIntegral (sum valores) --sumamos todos los valores para despues calcular porcentajes
    casillerosEnRango = length valores - 2 --esta es la cantidad de valores que estan en rango (no son outliers)

    limitesInferiores = [-1/0] ++ [valorMinimo + anchoCasillero * fromIntegral i | i <- [0..casillerosEnRango-1]] ++ [valorMaximo] --calculamos el limite inferior de cada casillero, comenzando en -inf 
    limitesSuperiores = [valorMinimo] ++ [valorMinimo + anchoCasillero * fromIntegral i | i <- [1..casillerosEnRango]] ++ [1/0] --calculamos el limite superior de cada uno, claramente el primer limite superior (despues de -inf, que es donde caen los errores) es el primer valor minimo. Terminamos con +inf, que son valores mayores al maximo

    porcentajes = if total == 0 then replicate (length valores) 0.0 --si no hay elementos todos los porcentajes son claramente 0
                  else  map (\cantidadElems -> fromIntegral cantidadElems / total * 100) valores --porcentaje que representa cada casillero con respecto al total de elementos del histograma