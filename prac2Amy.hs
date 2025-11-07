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

-- 3.
equiv_op :: LProp -> LProp
equiv_op (Impl p q) = Disy (Neg p) q
equiv_op (Neg p) = Neg (equiv_op p)
equiv_op (Conj p q) = Conj (equiv_op p) (equiv_op q)
equiv_op (Disy p q) = Disy (equiv_op p) (equiv_op q)
equiv_op (Syss p q) = Syss (equiv_op p) (equiv_op q)
equiv_op p = p

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
