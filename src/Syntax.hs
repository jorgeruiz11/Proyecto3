module Syntax where

import Data.List

-- Los indices los definimos como enteros para poder usarlos como variables.
type Ind = Int

-- El nombre de una función o un predicado es una cadena.
type Nombre = String

-- Las sustituciones serán pares de indices y terminos.
type Subst = [(Ind, Term)]

-- Define los términos como una variable o una función de n términos. (v in V)
data Term = V Ind | F Nombre [Term] deriving (Show, Eq)


{- Tipo de dato que representa las literales, las cuales pueden ser: T, F, un
   un Predicado o una igualdad de términos.
-}
data Lit = TrueF
         | FalseF
         | Pr Nombre [Term]
         | Eq Term Term deriving (Show, Eq)


 -- Recibe un término y nos devuelve una lista de indices (usamos "map" porque
 -- aplica la función a cada 1).
varT :: Term -> [Ind]
varT term = case term of
  V x -> [x]
  F _ l -> nub (concat [varT t | t <- l])

-- Verfica si una sustitución es válida.
verifSus :: Subst -> Bool
verifSus s = tieneRep [v | (v,t) <- s]

-- Sustitución para terminos.
-- Necesario para seguir haciendo las descomposiciones en MM-Extendido
apsubT :: Term -> Subst -> Term
apsubT t sus = case t of
  V x -> case sus of
    [] -> V x                 -- Si la sustitución está vacía entonces dejamos la variable tal cual.
    (v, t2):xs -> if x == v   -- Checamos el primero y aplicamos la sustitución o recorremos e.o.c
                  then t2
                  else apsubT (V x) xs
  F f lt -> F f [apsubT t sus | t <- lt] -- Para las cctes. va a darnos vacío. lt es la lista de términos


-- Verifica si hay elementos repetidos en una lista de elementos comparables.
tieneRep :: (Eq a) => [a] -> Bool
tieneRep l = case l of
  [] -> False
  x:xs -> if (elem x xs) then True
          else tieneRep xs
