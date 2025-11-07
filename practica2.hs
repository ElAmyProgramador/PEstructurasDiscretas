{-
Nombres: Emilio Alexeiv Rosales Guillen, Maria Fernanda Miranda
Correos: da4amy@ciencias.unam.mx, fernmirandas@ciencias.unam.mx
-}

-- Definición de los tipos de datos
data LProp = PTrue | PFalse | Var Nombre | Neg LProp
           | Conj LProp LProp | Disy LProp LProp
           | Impl LProp LProp | Syss LProp LProp
           deriving (Show, Eq)

type Nombre = String
type Asignación = [(String, Bool)]

-- 1.
vars :: LProp -> [Nombre]
vars PTrue = []
vars PFalse = []
vars (Var nombre) = [nombre]
vars (Neg p) = vars p
vars (Conj p q) = (vars p ++ vars q)
vars (Disy p q) = (vars p ++ vars q)
vars (Impl p q) = (vars p ++ vars q)
vars (Syss p q) = (vars p ++ vars q)

-- 2.
deMorgan :: LProp -> LProp
deMorgan (Neg (Conj p q)) = Disy (Neg p) (Neg q)
deMorgan (Neg (Disy p q)) = Conj (Neg p) (Neg q)
deMorgan (Neg p) = Neg (deMorgan p)
deMorgan (Conj p q) = Conj (deMorgan p) (deMorgan q)
deMorgan (Disy p q) = Disy (deMorgan p) (deMorgan q)
deMorgan (Impl p q) = Impl (deMorgan p) (deMorgan q)
deMorgan (Syss p q) = Syss (deMorgan p) (deMorgan q)
deMorgan p = p

-- 3.
equiv_op :: LProp -> LProp
equiv_op (Impl p q) = Disy (Neg p) q
equiv_op (Neg p) = Neg (equiv_op p)
equiv_op (Conj p q) = Conj (equiv_op p) (equiv_op q)
equiv_op (Disy p q) = Disy (equiv_op p) (equiv_op q)
equiv_op (Syss p q) = Syss (equiv_op p) (equiv_op q)
equiv_op p = p

-- 4.
dobleNeg :: LProp -> LProp
dobleNeg (Neg (Neg p)) = dobleNeg p
dobleNeg (Neg p) = Neg (dobleNeg p)
dobleNeg (Conj p q) = Conj (dobleNeg p) (dobleNeg q)
dobleNeg (Disy p q) = Disy (dobleNeg p) (dobleNeg q)
dobleNeg (Impl p q) = Impl (dobleNeg p) (dobleNeg q)
dobleNeg (Syss p q) = Syss (dobleNeg p) (dobleNeg q)
dobleNeg p = p

-- 5.
num_conectivos :: LProp -> Int
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var _) = 0
num_conectivos (Neg p) = 1 + num_conectivos p
num_conectivos (Conj p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Disy p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Impl p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Syss p q) = 1 + num_conectivos p + num_conectivos q

-- 6.
num_variables :: LProp -> Int
num_variables PTrue = 0
num_variables PFalse = 0
num_variables (Var _) = 1
num_variables (Neg p) = num_variables p
num_variables (Conj p q) = num_variables p + num_variables q
num_variables (Disy p q) = num_variables p + num_variables q
num_variables (Impl p q) = num_variables p + num_variables q
num_variables (Syss p q) = num_variables p + num_variables q

-- 7.
profundidad :: LProp -> Int
profundidad PTrue = 0
profundidad PFalse = 0
profundidad (Var _) = 0
profundidad (Neg p) = 1 + profundidad p
profundidad (Conj p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Disy p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Impl p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Syss p q) = 1 + max (profundidad p) (profundidad q)

-- 8.
interpretation :: LProp -> Asignación -> Bool
interpretation PTrue _ = True
interpretation PFalse _ = False
interpretation (Var nombre) asignacion = 
    case lookup nombre asignacion of
        Just valor -> valor
        Nothing -> error $ "Variable no encontrada: " ++ nombre
interpretation (Neg p) asignacion = not (interpretation p asignacion)
interpretation (Conj p q) asignacion = interpretation p asignacion && interpretation q asignacion
interpretation (Disy p q) asignacion = interpretation p asignacion || interpretation q asignacion
interpretation (Impl p q) asignacion = not (interpretation p asignacion) || interpretation q asignacion
interpretation (Syss p q) asignacion = interpretation p asignacion == interpretation q asignacion
