module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = if length s >= n then s -- si la longitud del string es n entonces no necesitamos hacer nada
                     else replicate (n- length s) ' ' ++ s -- usamos replicate para crear lista de espacios y la concatenamos con el string. La cantidad de espacios sera n - la longitud del string s
-- us

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = zipWith (\indice elemento -> if indice == n then f elemento else elemento) [0..] xs
--tomamos la opcion 1. Lo que ocurre en este caso es que con la lambda generamos una lista
--con todos los indices, que se empareja con los elementos de la lista xs. En el indice n se aplica f, el resto de los elementos queda igual


-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
