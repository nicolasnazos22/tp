module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma
import qualified Control.Applicative as Estado

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr ::  (Float -> a) -> (Float -> Float -> a) -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> Expr -> a

recrExpr fConst fRango fSuma fResta fMult fDiv expr = case expr of
    Const x -> fConst x
    Rango x y -> fRango x y
    Suma e1 e2 -> fSuma e1 (recr e1) e2 (recr e2)
    Resta e1 e2 -> fResta e1 (recr e1) e2 (recr e2)
    Mult e1 e2 -> fMult e1 (recr e1) e2 (recr e2)
    Div e1 e2 -> fDiv e1 (recr e1) e2 (recr e2)
    where
      recr = recrExpr fConst fRango fSuma fResta fMult fDiv --esquema de recursividad primitiva, es decir, con acceso a las subexpresiones originales

foldExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr fConst fRango fSuma fResta fMult fDiv expresion = case expresion of
    Const x -> fConst x
    Rango x y -> fRango x y
    Suma expresion1 expresion2 -> fSuma (recr expresion1) (recr expresion2)
    Resta expresion1 expresion2 -> fResta (recr expresion1) (recr expresion2)
    Mult expresion1 expresion2 -> fMult (recr expresion1) (recr expresion2)
    Div expresion1 expresion2 -> fDiv (recr expresion1) (recr expresion2)
    where
      recr = foldExpr fConst fRango fSuma fResta fMult fDiv --esquema de recursividad estructural de expr
-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = foldExpr (\x -> (\g -> (x, g))) --solo necesitamos recibir los resultados recursivos, no necesitamos acceso a subestructuras                   
                (\l u -> dameUno (l, u))     
                (\g1 g2 -> combinarGeneradores (+) g1 g2)      --para evitar codigo repetitivo agrupamos codigo en una funcion auxiliar         
                (\g1 g2 -> combinarGeneradores (-) g1 g2)          
                (\g1 g2 -> combinarGeneradores (*) g1 g2)              
                (\g1 g2 -> combinarGeneradores(/) g1 g2)               
                
-- Combina dos generadores de números aleatorios usando una operación binaria.
-- generamos un primer valor usando g1 y el gI (generador inicial), despues generamos un segundo valor con g2 usando el generador resultante de g1 gI. Devolvemos el valor resultante y el nuevo generador.
combinarGeneradores :: (Float -> Float -> Float) -> G Float -> G Float -> G Float
combinarGeneradores op g1 g2 gI = 
  (op (fst resInicial) (fst resIntermedio), snd resIntermedio) 
  where
    resInicial = (g1 gI)
    resIntermedio = g2 (snd resInicial)
--
-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = generarHistograma (muestra f n g) 
                          where
                            generarHistograma (valores, gFinal) = (histograma m (rango95 valores) valores, gFinal) 
--Separamos la logica de generacion de muestras de la construccion del histograma para mayor claridad.
--usamos el generador devuelto por la funcion muestra, resultado de generar n valores recursivamente, y la lista de valores generados, para construir el histograma

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr gI = armarHistograma m n (eval expr) gI

--Cada evaluacion produce un nuevo numero aleatorio
--Mediante armarHistograma entonces construimos un histograma de m casilleros usando las n muestras obtenidas previamente con el generador gI.
--Finalmente evalHistograma devuelve un histograma junto con el generador actualizado

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
--en este caso necesitamos acceso a las subestructuras originales, por eso usamos recr.
--analizamos la precedencia de las operaciones elementales para ver donde quitar parentesis no altera las expresiones originales y concluimos:
mostrar :: Expr -> String
mostrar = recrExpr 
    show
    (\low high -> show low ++ "~" ++ show high) -- rango no lleva paréntesis
    (\e1 s1 e2 s2 ->
        maybeParen (esMultODiv e1) s1 
        ++ " + "
        ++ maybeParen (esMultODiv e2 || esRango e2) s2) 
    (\e1 s1 e2 s2 ->
        maybeParen (esResta e1 || esMultODiv e1) s1 
        ++ " - "
        ++ maybeParen (esResta e2) s2)
    (\e1 s1 e2 s2 ->
        maybeParen (esSumaOResta e1) s1
        ++ " * "
        ++ maybeParen (esSumaOResta e2) s2)
    (\e1 s1 e2 s2 ->
        maybeParen (esSumaOResta e1) s1 
        ++ " / "
        ++ maybeParen (esSumaOResta e2 || esMultODiv e2) s2)
  where
    -- 
    esSumaOResta e = case constructor e of
        CESuma  -> True
        CEResta -> True
        _       -> False

    esMultODiv e = case constructor e of
        CEMult -> True
        CEDiv  -> True
        _      -> False

    esRango e = case constructor e of
        CERango -> True
        _       -> False

    esResta e = case constructor e of
        CEResta -> True
        _       -> False


data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
