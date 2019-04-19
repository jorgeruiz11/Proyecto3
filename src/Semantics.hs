     module Semantics where

import Syntax

-- Definimos la interpretación de fórmulas como una función que toma un nombre,
-- una lista de elementos del universo y devuelve un elemento del universo.
type IntF a = Nombre -> [a] -> a

-- Definimos la interpretación de predicados como una función que toma un nombre,
-- una lista de elementos del universo y nos dice si los elementos en nuestro universo
-- se relacionan.
type IntR a = Nombre -> [a] -> Bool

-- Definimos el estado de un elemento del universo como:    sigma: Var -> M
type Estado a = Ind -> a

--  L-Estructura
type Estructura a = ([a], IntF a, IntR a)


actEst :: Estado a -> Ind -> a -> Estado a
actEst e x n = ne
  where ne y = if x == y then n else e y


  -- Interpretación de términos. Dado un estado, una interpretación de funciones,
  -- y un término, aplica la interpretación a dicho término.
iTerm :: Estado a -> IntF a -> Term -> a
iTerm e iF t = case t of
  V x -> e x
  F t lt -> iF t [iTerm e iF t | t <- lt] -- La interpretación de la lista de términos.


-- Interpretación de fórmulas. Dada una estructura, un estado y una fórmula
-- determina la veracidad de dicha fórmula bajo una interpretación dad y el Estado
-- acuatlizado
iForm :: Eq a => Estructura a -> Estado a -> Form -> Bool
iForm str e phi = case phi of   -- str -> estructura
  FalseF -> False
  TrueF -> True
  Pr p lt -> iR p (map (iTerm e iF) lt) where (_,iF,iR) = str    -- Interpreta el predicado con su lista de términos
  Eq t1 t2 -> (iTerm e iF t1) == (iTerm e iF t2) where (_,iF,_) = str
  Neg p -> not (iForm str e p)
  Conj p q -> (iForm str e p) && (iForm str e q)  -- La interpretación de los dos es la misma
  Disy p q -> (iForm str e p) || (iForm str e q)
  Imp p q -> iForm str e (Disy (Neg p) q)
  Equiv p q -> iForm str e (Conj (Imp p q) (Imp q p))
  All x p -> and ([iForm str (actEst e x m) p | m <- u]) where (u,_,_) = str   -- Verificamos que la sustitución se cumpla para cada uno
  Ex x p -> or ([iForm str (actEst e x m) p | m <- u]) where (u,_,_) = str


-- Satisfacibilidad de una fórmula. Dada una Estructura, un estado de las variables
-- y una fórmula nos dice si la fórmula es satisfacible con ese modelo dado.
satForm :: Eq a => Estructura a -> Estado a -> Form -> Bool
satForm str e phi = iForm str e phi
