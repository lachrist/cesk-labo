module Expression where

import Formatable (Formatable (format), Tree (Atom, Struct))
import Primitive (Primitive)

type Variable = String

data Location
  = Location String Int Int
  deriving (Eq, Show)

data Expression
  = Literal Primitive Location
  | Variable Variable Location
  | Condition Expression Expression Expression Location
  | Binding Variable Expression Expression Location
  | Lambda [Variable] Expression Location
  | Application Expression [Expression] Location
  deriving (Show)

instance Eq Expression where
  (Literal p1 _) == (Literal p2 _) = p1 == p2
  (Variable v1 _) == (Variable v2 _) = v1 == v2
  (Condition t1 c1 a1 _) == (Condition t2 c2 a2 _) = t1 == t2 && c1 == c2 && a1 == a2
  (Binding v1 r1 b1 _) == (Binding v2 r2 b2 _) = v1 == v2 && r1 == r2 && b1 == b2
  (Lambda h1 b1 _) == (Lambda h2 b2 _) = h1 == h2 && b1 == b2
  (Application f1 xs1 _) == (Application f2 xs2 _) = f1 == f2 && xs1 == xs2
  _ == _ = False

printLocation :: Location -> String
printLocation (Location name line column) =
  name ++ ":" ++ show line ++ ":" ++ show column

instance Formatable Location where
  format = Atom . printLocation

instance Formatable Expression where
  format (Literal primitive _) = format primitive
  format (Variable variable _) = Atom variable
  format (Condition test consequent alternate _) =
    Struct "if" [format test, format consequent, format alternate]
  format (Binding variable right body _) =
    Struct "let" [Atom variable, format right, format body]
  format (Lambda parameters body _) =
    Struct "let" [Atom "lambda", Struct "" $ map Atom parameters, format body]
  format (Application callee arguments _) =
    Struct "" $ format callee : map format arguments
