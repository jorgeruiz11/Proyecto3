module Mar_Mont where

import Syntax
import Data.List


--Simplifica una sustitución.
simpSus :: Subst -> Subst
simpSus sus = [(x, t) | (x, t) <- sus, V x /= t]  -- Verifica que nos queden igual
                                                  -- después de la sustitución.


{--
  Tomamos las variables x tales que ya se les aplicó la primera sustitución y
  después les aplicamos la segunda sustitución y quitamos aquellos que están
  repetidos.

  ws checamos que las variables de la segunda sustitución no estén el las variables
  de la primera.
--}
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = zs ++ ws
  where zs = simpSus [(x, apsubT t s2) | (x, t) <- s1] -- Pares de la primera sustitución y les aplicamos la segunda.
        ws = [ (x, t) | (x, t) <- s2, not (elem x vs1)]
        vs1 = fst (unzip s1) -- devuelve las variables de la primer sustitución



{-- Recibe una lista de elementos [t1,...,tn] y nos regresa una lista de pares
    [t1 = t2, t2 = t3,..., tn-1 = tn]
--}
hazPares :: [a] -> [(a, a)]
hazPares l = case l of
  [] -> []
  x:y:xs -> (x,y):(hazPares (y:xs))
  x:xs -> []


-- Unifica los términos por pares
unificaC_aux:: [(Term, Term)] -> Subst
unificaC_aux pares = case pares of
  [] -> []
  (t1, t2):lp -> case (t1, t2) of     -- Checamos que tengamos al menos un par
    (F f lt1, F g lt2) -> if f == g && length lt1 == length lt2
                          then unificaC_aux ((zip lt1 lt2) ++ lp)        --[DESC]    recibimos f(t1,..,tn) y f(r1,..rn) queremos hacer t1=r1, ..., tn=rn
                          else error "[DFALLA] Imposible unificar"                              --y hacemos lo mismo con el resto.
    (V x, V y) -> if x == y
                  then unificaC_aux lp                -- [ELIM]
                  else compSus d (unificaC_aux lps)   -- [SUST] cuando t es variable
        where d = [(x, V y)]
              lps = [(apsubT t1 d, apsubT t2 d) | (t1, t2) <- lp ]

    (V x, F f lt) -> if elem x (varT (F f lt))
                     then error "[SFalla] Imposible unificar"      -- El algoritmo dice que si x figura en los terminos entonces falla
                     else compSus d (unificaC_aux lps)
         where d = [(x, F f lt)]
               lps = [(apsubT t1 d, apsubT t2 d) | (t1, t2) <- lp ]

    (F f lt, V x) -> unificaC_aux ((t2, t1):lp)          -- [SWAP]


-- Unifica una lista de términos
unificaConj:: [Term] -> Subst
unificaConj = unificaC_aux.hazPares   -- La parte del punto lo que que hace es la composición de funciones
                                            -- Podemos hacer unificaConj lt = unificaC_aux.hazPares lt y haskell ve el lt implicito

-- unifica dos términos
unifica:: Term -> Term -> Subst
unifica s t = unificaConj [s,t]


-- Unifica solamente las literales
unificaLit:: Lit -> Lit -> Subst
unificaLit phi psi = case (phi,psi) of
  (TrueF, TrueF) -> []
  (FalseF, FalseF) -> []
  (Pr p lt1, Pr q lt2) -> if p == q && length lt1 == length lt2
                          then unificaC_aux (zip lt1 lt2)    -- Igualamos entre cada "termino" del predicado.
                          else error "Imposible unificar"   -- Falla cuando P != Q
  (Eq t1 s1, Eq t2 s2) -> unificaC_aux [(t1, t2), (s1, s2)]
  _ -> error "Imposible unificar"  -- Las literales son solo las definidas arriba por lo que otra
                                   -- cosa no es admitido


------------------------ Añadimos los nuevos métodos ---------------------------
-- Devuelve el umg de un conjunto de literales.
-- Recibe una lista de literales y nos devuelve una sustitución.
mmE :: [Lit] -> Subst
mmE [] = []
mmE [x] = [] -- Hacemos esto porque en caso de ser solo uno, no podemos hacer nada, ya que
             -- nuestra unificacion de terminos solo esta definida por pares.
mmE (x:y:xs) = (unificaLit x y) ++ mmE (y:xs)  -- Toma los primeros 2 elementos y los unifica y luego toma el segundo con el tercero y
                                               -- asi sucesivamente, luego llega al n-1 y lo unifica con el n y cuando llega al
                                               -- n-ésimo como ya no tiene ningun otro mas adelante regresa la vacia (con ese elem.)
                                               -- y se queda con el anterior (la ultima que hizo).

-- Idea anterior:
--mmE li = mmE lit = case lit of
  --[phi, psi] -> unificaLit phi psi
-- La dejo solo para recordar como fue mi idea principal.


-- Función que aplica la sustitución a cada elemento del conjunto.
{- Recibimos una lista de literales y una sustitución, entonces usamos la función
   que aplica la sustitución a las literales
-}
sust_G :: [Lit] -> Subst -> [Lit]
sust_G [li] sust = [apsubL li sust]


-- Aplica una sustitución a una literal
{- Tomamos una literal y dependiendo del tipo que sea le aplicamos una sustitución
   a cada termino de esa literal.
-}
apsubL :: Lit -> Subst -> Lit
apsubL lit sus = case lit of
  TrueF -> TrueF
  FalseF -> FalseF
  Pr p lt -> Pr p [apsubT t sus | t <- lt]
  Eq t1 t2 -> Eq (apsubT t1 sus) (apsubT t2 sus)
