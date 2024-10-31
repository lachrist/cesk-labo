-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}

module Interprete where

import Continuation (Continuation (..))
import Data (Builtin, Data (Builtin, Closure, Cons, Primitive), isTruthy)
import Data.Map (fromList, insert, lookup, union)
import Error (Error (ApplyError, MissingError), Message)
import Expression (Expression (..), Location)
import State (State (Failure, Ongoing, Success))
import Store (Store)
import Valuation (Valuation (get, new))

type Call v = (Data v, [v], Location)

type BuiltinResult v x = (Store x, Either Message v)

step :: (Valuation v x) => State v x -> IO (State v x)
step (Ongoing (Literal prm _) _ mem nxt) =
  continue nxt (new mem (Primitive prm))
step (Ongoing (Lambda vars body _) env mem nxt) =
  continue nxt (new mem (Closure env vars body))
step (Ongoing (Variable var loc) env mem nxt) =
  maybe
    (return $ Failure (MissingError var loc) mem nxt)
    (continue nxt . (mem,))
    (Data.Map.lookup var env)
step (Ongoing (Binding var val res _) env mem nxt) =
  return $ Ongoing val env mem (Bind env var res nxt)
step (Ongoing (Application fct args loc) env mem nxt) =
  return $ Ongoing fct env mem (Apply env args [] nxt loc)
step (Ongoing (Condition test cons alt _) env mem nxt) =
  return $ Ongoing test env mem (Branch env cons alt nxt)
step final = return final

continue :: (Valuation v x) => Continuation v -> (Store x, v) -> IO (State v x)
continue Finish (mem, val) =
  return $ Success val mem
continue (Branch env pos neg nxt) (mem, val) =
  return $ Ongoing (if isTruthy (get mem val) then pos else neg) env mem nxt
continue (Bind env var res nxt) (mem, val) =
  return $ Ongoing res (insert var val env) mem nxt
continue (Apply env (arg : args) vals nxt loc) (mem, val) =
  return $ Ongoing arg env mem (Apply env args (vals ++ [val]) nxt loc)
continue (Apply _ [] vals nxt loc) (mem, val) =
  let (fct : args) = vals ++ [val]
   in apply (get mem fct, args, loc) mem nxt

collect :: (Valuation v x) => Store x -> Data v -> [v]
collect mem (Cons car cdr) = car : collect mem (get mem cdr)
collect _ _ = []

apply :: (Valuation v x) => Call v -> Store x -> Continuation v -> IO (State v x)
apply (fct@(Closure env vars res), args, loc) mem nxt
  | length vars == length args =
      return $ Ongoing res (fromList (zip vars args) `union` env) mem nxt
  | otherwise = return $ Failure (ApplyError "arity-mismatch" fct args loc) mem nxt
apply (Builtin "apply", [arg0, arg1], loc) mem nxt =
  apply (get mem arg0, collect mem (get mem arg1), loc) mem nxt
apply (fct@(Builtin "apply"), args, loc) mem nxt =
  return $ Failure (ApplyError "arity-mismatch" fct args loc) mem nxt
apply call@(Builtin name, args, _) mem nxt =
  applyBuiltin name args mem >>= continueBuiltin nxt call
apply (fct, args, loc) mem nxt = return $ Failure (ApplyError "cannot-apply" fct args loc) mem nxt

continueBuiltin :: (Valuation v x) => Continuation v -> Call v -> BuiltinResult v x -> IO (State v x)
continueBuiltin nxt (fct, args, loc) (mem, Left msg) =
  return $ Failure (ApplyError msg fct args loc) mem nxt
continueBuiltin nxt _ (mem, Right val) = continue nxt (mem, val)

applyBuiltin :: (Valuation v x) => Builtin -> [v] -> Store x -> IO (Store x, Either Message v)
applyBuiltin = error "todo"

-- applyBuiltin "read-line" [] mem =
--   fmap (Right . Primitive . String) getLine
-- applyBuiltin σ κ "print" ξs@(_ : _) = putStrLn (loop (map (valueToData σ) ξs)) >> fmap Right (nxtinue κ (dataToValue σ (Atomic Null)))
--   where
--     loop ((Atomic (String τ)) : δs) = τ ++ loop δs
--     loop (δ : δs) = showData δ ++ loop δs
--     loop [] = ""
-- applyBuiltin σ κ β ξs = either (return . Left) (fmap Right . nxtinue κ) (applyPure σ β ξs)

-- toCons :: Data v -> Either string (v, v)
-- toCons (Cons fst snd) = Right (fst, snd)
-- toCons other = Left ("expected a pair but got: " ++ showData other)

-- applyPure :: (System v e) => memre e -> Builtin -> [v] -> Either String (memre e, v)
-- applyPure σ "begin" ξs@(_ : _) = Right (σ, last ξs)
-- applyPure σ "eq?" (ξ : ξ' : []) = Right $ dataToValue σ (Atomic $ Boolean $ ξ == ξ')
-- applyPure σ "inspect" ξs = Right $ dataToValue σ (Atomic $ String $ concat $ Data.List.intersperse ", " (map (inspect σ) ξs))
-- applyPure σ "cons" (ξ : ξ' : []) = Right $ dataToValue σ (Compound $ Cons (ξ, ξ'))
-- applyPure σ "list" ξs = Right $ foldr (\ξ (σ, ξ') -> dataToValue σ (Compound $ Cons (ξ, ξ'))) (dataToValue σ (Atomic $ Null)) ξs
-- applyPure σ "car" (ξ : []) =
--   maybe
--     (Left "expected a pair as first argument")
--     (Right . (σ,) . fst)
--     (toCons $ valueToData σ ξ)
-- applyPure σ "cdr" (ξ : []) =
--   maybe
--     (Left "expected a pair as first argument")
--     (Right . (σ,) . snd)
--     (toCons $ valueToData σ ξ)
-- applyPure σ "set-car!" (cons : car : []) =
--   toCons cons >>= \(fst, snd) ->
--     Right
--       (fmap (,cons) (mutate σ fst car))
--       maybe
--       (Left "expected a pair as first argument")
--       (fmap (,ξ) . mutate σ ξ . Compound . Cons ξ')
--       . snd
--         (toCons $ valueToData σ ξ)
-- applyPure σ "set-cdr!" (ξ : ξ' : []) =
--   maybe
--     (Left "expected a pair as first argument")
--     (fmap (,ξ) . mutate σ ξ . Compound . Cons . (,ξ') . fst)
--     (toCons $ valueToData σ ξ)
-- applyPure σ β ξs = fmap (dataToValue σ) (applyData β (map (valueToData σ) ξs))

-- applyData :: (Eq v) => Builtin -> [Data v] -> Either String (Data v)
-- applyData "raise" (δ : []) = Left $ showData δ
-- applyData "any->string" (δ : []) = Right $ Atomic $ String $ showData δ
-- applyData "null?" (Atomic (Null) : []) = Right $ Atomic $ Boolean True
-- applyData "null?" (_ : []) = Right $ Atomic $ Boolean False
-- applyData "boolean?" (Atomic (Boolean _) : []) = Right $ Atomic $ Boolean True
-- applyData "boolean?" (_ : []) = Right $ Atomic $ Boolean False
-- applyData "number?" (Atomic (Number _) : []) = Right $ Atomic $ Boolean True
-- applyData "number?" (_ : []) = Right $ Atomic $ Boolean False
-- applyData "string?" (Atomic (String _) : []) = Right $ Atomic $ Boolean True
-- applyData "string?" (_ : []) = Right $ Atomic $ Boolean False
-- applyData "pair?" (Compound (Cons _ _) : []) = Right $ Atomic $ Boolean True
-- applyData "pair?" (_ : []) = Right $ Atomic $ Boolean False
-- applyData "procedure?" (Compound (Closure _ _ _) : []) = Right $ Atomic $ Boolean True
-- applyData "procedure?" (Atomic (Builtin _) : []) = Right $ Atomic $ Boolean True
-- applyData "procedure?" (_ : []) = Right $ Atomic $ Boolean False
-- applyData β δs =
--   maybe
--     (Left $ "expected only atomic arguments")
--     (fmap Atomic . applyAtomic β)
--     (sequence $ map toAtomic δs)

-- applyAtomic :: Builtin -> [Literal] -> Either String Literal
-- applyAtomic "eqv?" (θ : θ' : []) = Right $ Boolean $ θ == θ'
-- applyAtomic "&&" (Boolean ο : Boolean ο' : []) = Right $ Boolean $ ο && ο'
-- applyAtomic "||" (Boolean ο : Boolean ο' : []) = Right $ Boolean $ ο || ο'
-- applyAtomic "=" (Number ν : Number ν' : []) = Right $ Boolean $ ν == ν'
-- applyAtomic "<" (Number ν : Number ν' : []) = Right $ Boolean $ ν < ν'
-- applyAtomic "<=" (Number ν : Number ν' : []) = Right $ Boolean $ ν <= ν'
-- applyAtomic "+" θs =
--   maybe
--     (Left "expected only numbers")
--     (Right . Number . foldr (+) 0)
--     (sequence $ map (toNumber . Atomic) θs)
-- applyAtomic "-" (Number ν : Number ν' : []) = Right $ Number $ ν - ν'
-- applyAtomic "-" (Number ν : []) = Right $ Number $ 0 - ν
-- applyAtomic "*" θs =
--   maybe
--     (Left "expected only numbers")
--     (Right . Number . foldr (*) 1)
--     (sequence $ map (toNumber . Atomic) θs)
-- applyAtomic "/" (Number ν : Number ν' : []) = Right $ Number $ ν / ν'
-- applyAtomic "expt" (Number ν : Number ν' : []) = Right $ Number $ ν ** ν'
-- applyAtomic "sqrt" (Number ν : []) = Right $ Number $ sqrt ν
-- applyAtomic "string-append" θs =
--   maybe
--     (Left "expected only strings")
--     (Right . String . concat)
--     (sequence $ map (toString . Atomic) θs)
-- applyAtomic "string-length" (String τ : []) = Right $ Number $ fromIntegral $ length τ
-- applyAtomic "substring" (String τ : Number ν : Number ν' : []) = Right $ String $ take (round $ ν' - ν) (drop (round ν) τ)
-- applyAtomic "number->string" (Number ν : []) = Right $ String $ showData $ Number ν
-- applyAtomic "string->number" (String τ : []) = case reads τ of
--   [(ν, "")] -> Right $ Number ν
--   _ -> Left "cannot parse as number"
-- applyAtomic β θs = Left $ if elem β builtins then "invalid arguments" else "unsupported builtin"

-- execute :: (ShowIndent v, ShowIndent e, System v e) => Control -> IO (State v e)
-- execute γ =
--   let (σ, ε) = Data.List.foldl' accumulator (initialmemre, Data.Map.empty) builtins
--    in loop $ Ongoing γ ε σ Finish
--   where
--     loop φ@(Ongoing _ _ _ _) = step φ >>= loop -- putStrLn (showIndent 0 φ) >>
--     loop φ = return φ
--     accumulator (σ, ε) β =
--       let (σ', ξ) = dataToValue σ (Atomic $ Builtin β)
--        in (σ', Data.Map.insert β ξ ε)

-- top :: String -> Either Text.Parsec.ParseError Control -> IO String
-- top _ (Left ε) = return $ show ε
-- top "reference" (Right γ) = fmap printState (execute γ :: IO (State Value1 Element1))
-- top "mixed" (Right γ) = fmap printState (execute γ :: IO (State Value2 Element2))
-- top "reference-interning" (Right γ) = fmap printState (execute γ :: IO (State Value3 Element3))
-- top "value" (Right γ) = fmap printState (execute γ :: IO (State Value4 Element4))
-- top λ _ = return $ "Error >> unknown system " ++ λ
