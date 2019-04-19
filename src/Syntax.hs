module Syntax where

import Data.List

-- Los indices los definimos como enteros para poder usarlos como variables.
type Ind = Int

-- El nombre de una función o un predicado es una cadena.
type Nombre = String

-- Define los términos como una variable o una función de n términos. (v in V)
data Term = V Ind | F Nombre [Term] deriving (Show, Eq)

-- Define las fórmulas como T, F, predicados, igualdad, operadores binarios,
-- cuantidicadores.
data Form = TrueF
            | FalseF
            | Pr Nombre [Term]
            | Eq Term Term
            | Neg Form
            | Conj Form Form
            | Disy Form Form
            | Imp Form Form
            | Equiv Form Form
            | All Ind Form
            | Ex Ind Form

-- Función que te dice las variables libres de una formula. Checa caso a caso.
-- Las variables libres de una formula son todas aquellas que no estan cuantificadas.
fv :: Form -> [Ind]
fv TrueF = []
fv FalseF = []
fv (Pr _ ts) = concat[varT t | t <- ts]           -- Encontramos las variables en su lista de términos
fv (Eq t1 t2) = union (varT t1) (varT t2)
fv (Neg phi) = fv phi
fv (Conj phi psi) = union (fv phi) (fv psi)
fv (Disy phi psi) = union (fv phi) (fv psi)
fv (Imp phi psi) = union (fv phi) (fv psi)
fv (Equiv phi psi) = union (fv phi) (fv psi)
fv (All x phi) = (fv phi) \\ [x]                  --  Encontramos sus variables libres y después eliminamos la variable ligada
fv (Ex x phi) = (fv phi) \\ [x]


-- Recibe un término y nos devuelve una lista de indices (usamos "map" porque
-- aplica la función a cada 1).
varT :: Term -> [Ind]
varT (V x) = [x]
varT (F _ []) = [] -- "_" representa el nombre.
-- varT (F _ (t:ts)) = (varT t) ++ concat(map varT ts)
varT (F _ l) = nub(concat([varT t | t <- l]))   -- Obtenemos las variables en la función y eliminamos los duplicados.


-- Función que nos devuelve las varibales ligadas de una formula.
bv :: Form -> [Ind]
bv TrueF = []
bv FalseF = []
bv (Pr _ ts) = [] -- Porque no hay ningun cuatificador.
bv (Eq t1 t2) = [] -- Porque son todos términos.
bv (Neg phi) = bv phi
bv (Conj phi psi) = union (bv phi) (bv psi)
bv (Disy phi psi) = union (bv phi) (bv psi)
bv (Imp phi psi) = union (bv phi) (bv psi)
bv (Equiv phi psi) = union (bv phi) (bv psi)
bv (All x phi) = union (bv phi) [x]           -- Encontramos las variables ligadas y añadimos la var ligada x.
bv (Ex x phi) = union (bv phi) [x]


-- Cerradura universal.
-- La cerradura añade un cuatificador universal a cada variable que sea libre en φ.
aCl :: Form -> Form
aCl phi = aClaux phi (fv phi)

-- Mete el "para todo" a cada elemento.
aClaux :: Form -> [Ind] -> Form
aClaux phi l = case l of
  [] -> phi
  x:xs -> All x (aClaux phi xs)       -- Dada una variable x, esta función agrega a φ un cuantificador universal que liga a x
                                      -- y continua recursivamente



-- Cerradura existencial.
-- La cerradura añade un cuatificador existencial a cada variable que sea libre en φ.
eCl :: Form -> Form
eCl phi = eClaux phi (fv phi)


-- Mete el "existe" a cada elemento.
eClaux :: Form -> [Ind] -> Form
eClaux phi l = case l of
  [] -> phi
  x:xs -> Ex x (eClaux phi xs)   -- Dada una variable x, esta función agrega a φ un cuantificador existencial que liga a x
                                 -- y continua recursivamente


-- Sustitución de términos. Se define como una lista de (Indice, Término).
type Subst = [(Ind, Term)]

-- Verfica si una sustitución es válida.
verifSus :: Subst -> Bool
verifSus s = tieneRep [v | (v,t) <- s]

-- Sustitución para terminos.
apsubT :: Term -> Subst -> Term
apsubT t sus = case t of
  V x -> case sus of
    [] -> V x                           -- Si la sustitución está vacía entonces dejamos la variable tal cual.
    (v,t2):xs -> if (x == v)            -- Checamos el primero y aplicamos la sustitución o recorremos e.o.c
                 then t2
                 else apsubT (V x) xs
  F f lt -> F f [apsubT t sus | t <- lt] -- Para las cctes. va a darnos vacío. lt es la lista de términos

-- Sustitución de Formulas.
apsubF :: Form -> Subst -> Form
apsubF phi sus = case phi of
  TrueF -> TrueF
  FalseF -> FalseF
  Pr p lt -> Pr p [apsubT t sus | t <- lt]
  Eq t1 t2 -> Eq (apsubT t1 sus) (apsubT t2 sus)
  Neg p -> Neg (apsubF p sus)
  Conj p q -> Conj (apsubF p sus) (apsubF q sus)
  Disy p q -> Disy (apsubF p sus) (apsubF q sus)
  Imp p q -> Imp (apsubF p sus) (apsubF q sus)
  Equiv p q -> Equiv (apsubF p sus) (apsubF q sus)
  All x p -> if elem x lv
             then error "Sustitución inválida"
             else All x (apsubF p sus)
    where lv = union xs ts -- xs es la primer entrada ej: z:= y ts es el del lado derecho :=asd
          (xs, tt) = unzip sus
          ts = concat [varT t | t <- tt] -- concat (map varT tt)
  Ex x p -> if elem x lv
             then error "Sustitución inválida"
             else Ex x (apsubF p sus)
    where lv = union xs ts -- xs es la primer entrada ej: z:= y ts es el del lado derecho :=asd
          (xs, tt) = unzip sus
          ts = concat [varT t | t <- tt] -- concat (map varT tt)

-- Verifica si hay elementos repetidos en una lista de elementos comparables.
tieneRep :: (Eq a) => [a] -> Bool
tieneRep l = case l of
  [] -> False
  x:xs -> if (elem x xs) then True
          else tieneRep xs
