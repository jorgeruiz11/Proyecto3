module Ejemplo where

import Syntax
import Semantics

-- Funciones
sumaM m a b = mod (a + b) m
restaM m a b = mod (a - b) m
prodM m a b = mod (a * b) m
expM m a b = mod (a ^ b) m

-- Predicados
prirel a b = gcd a b == 1

-- Inteoretación de símbolos de función
iF :: Int -> IntF Int
iF m f l = case f of
  "s" -> sumaM m (l!!0) (l!!1)
  "p" -> prodM m (l!!0) (l!!1)
  "r" -> restaM m (l!!0) (l!!1)
  "exp" -> expM m (l!!0) (l!!1)
  _ -> read f :: Int

  -- Inteoretación de símbolos de predicado
iP :: Int -> IntR Int
iP m p l = case p of
  "prirel" -> prirel (l!!0) (l!!1)    -- (l!!1) saca el segundo elemento de la lista

-- Estado de la variables
est = id :: Int -> Int

main = do
  let m = 7
  let u = [0..(m-1)]
  let phi = All 1 ( (Imp ( Neg ( Eq (V 1) (F "0" []) ) ) (Ex 2 (Eq (F "p" [(V 1), (V 2)]) (F "1" []) ) ) ) )

  print $ iForm (u, iF m, iP m) est phi
