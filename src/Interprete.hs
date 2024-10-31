-- {-# LANGUAGE TupleSections #-}

module Interprete where

import Expression (Expression)
import Text.Parsec (ParseError)

top :: String -> Either Text.Parsec.ParseError Expression -> IO String
top _ _ = error "todo"

-- import Control
-- import Data
-- import qualified Data.List
-- import qualified Data.Map
-- import Literal
-- import ShowIndent
-- import State
-- import System
-- import qualified Text.Parsec

-- step :: (System v e) => State v e -> IO (State v e)
-- step (Ongoing (Control.Literal θ) _ σ κ) = kontinue κ (dataToValue σ (Atomic (Data.Literal θ)))
-- step (Ongoing (Abstraction ηs γ) ε σ κ) = kontinue κ (dataToValue σ (Compound $ Closure ε ηs γ))
-- step (Ongoing (Variable η) ε σ κ) = maybe (return $ Failure σ κ ("ReferenceError", η ++ " is not defined")) (kontinue κ . (σ,)) (Data.Map.lookup η ε)
-- step (Ongoing (Binding η γ γ') ε σ κ) = return $ Ongoing γ ε σ (Bind ε η γ' κ)
-- step (Ongoing (Application γ γs) ε σ κ) = return $ Ongoing γ ε σ (Apply ε γs [] κ)
-- step (Ongoing (Condition γ γ' γ'') ε σ κ) = return $ Ongoing γ ε σ (Branch ε γ' γ'' κ)
-- step φ = return φ

-- kontinue :: (System v e) => Kontinuation v -> (Store e, v) -> IO (State v e)
-- kontinue Finish (σ, ξ) = return $ Success σ ξ
-- kontinue (Branch ε γ γ' κ) (σ, ξ) = case valueToData σ ξ of
--   (Atomic (Data.Literal (Literal.Boolean False))) -> return $ Ongoing γ' ε σ κ
--   _ -> return $ Ongoing γ ε σ κ
-- kontinue (Bind ε η γ κ) (σ, ξ) = return $ Ongoing γ (Data.Map.insert η ξ ε) σ κ
-- kontinue (Apply ε (γ : γs) ξs κ) (σ, ξ) = return $ Ongoing γ ε σ (Apply ε γs (ξs ++ [ξ]) κ)
-- kontinue (Apply ε [] ξs κ) (σ, ξ) = apply σ κ (valueToData σ (head $ ξs ++ [ξ])) (tail $ ξs ++ [ξ])

-- apply :: (System v e) => Store e -> Kontinuation v -> Data v -> [v] -> IO (State v e)
-- apply σ κ δ@(Compound (Closure ε ηs γ)) ξs =
--   return $
--     if length ηs == length ξs
--       then Ongoing γ (Data.Map.union (Data.Map.fromList (zip ηs ξs)) ε) σ κ
--       else Failure σ κ ("ArityError", showData δ ++ " expected " ++ show (length ηs) ++ " argument(s) but received " ++ show (length ξs))
-- apply σ κ (Atomic (Builtin β)) ξs = fmap (either (Failure σ κ . ("BuiltinError",) . format) id) (applyBuiltin σ κ β ξs)
--   where
--     format τ = (showAtomic $ Builtin β) ++ ": " ++ τ ++ ", got: " ++ concat (Data.List.intersperse ", " (map (inspect σ) ξs))
-- apply σ κ δ _ = return $ Failure σ κ ("TypeError", showData δ ++ " cannot be applied")

-- applyBuiltin :: (System x y) => Store y -> Kontinuation x -> Builtin -> [x] -> IO (Either String (State x y))
-- applyBuiltin σ κ "apply" (ξ : ξ' : []) =
--   maybe
--     (return $ Left "did not received a list as second argument")
--     (fmap Right . apply σ κ (valueToData σ ξ))
--     (loop $ valueToData σ ξ')
--   where
--     loop (Compound (Cons ξ'' ξ''')) = fmap (ξ'' :) (loop $ valueToData σ ξ''')
--     loop (Atomic Null) = Just []
--     loop _ = Nothing
-- applyBuiltin σ κ "read-line" [] = getLine >>= (fmap Right . kontinue κ . (dataToValue σ) . Atomic . String)
-- applyBuiltin σ κ "print" ξs@(_ : _) = putStrLn (loop (map (valueToData σ) ξs)) >> fmap Right (kontinue κ (dataToValue σ (Atomic Null)))
--   where
--     loop ((Atomic (String τ)) : δs) = τ ++ loop δs
--     loop (δ : δs) = showData δ ++ loop δs
--     loop [] = ""
-- applyBuiltin σ κ β ξs = either (return . Left) (fmap Right . kontinue κ) (applyPure σ β ξs)

-- toCons :: Data v -> Either string (v, v)
-- toCons (Cons fst snd) = Right (fst, snd)
-- toCons other = Left ("expected a pair but got: " ++ showData other)

-- applyPure :: (System v e) => Store e -> Builtin -> [v] -> Either String (Store e, v)
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
--   let (σ, ε) = Data.List.foldl' accumulator (initialStore, Data.Map.empty) builtins
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