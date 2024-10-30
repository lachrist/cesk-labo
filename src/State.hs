{-# LANGUAGE FlexibleInstances #-}

module State where

import Control
import Data.Map
import Data.Word
import Format

data State v e
  = Ongoing Expression (Environment v) (Store e) (Kontinuation v)
  | Success (Store e) v
  | Failure (Store e) (Kontinuation v) (String, String)

type Environment v = Data.Map.Map Variable v

type Store e = Data.Map.Map Address e

type Address = Data.Word.Word64

data Kontinuation v
  = Bind (Environment v) Variable Expression (Kontinuation v)
  | Apply (Environment v) [Expression] [v] (Kontinuation v)
  | Branch (Environment v) Expression Expression (Kontinuation v)
  | Finish

instance (ShowIndent v) => ShowIndent (Environment v) where
  showIndent ι ε = "{" ++ concat (map (\(η, ξ) -> indent (ι + 1) ++ η ++ ": " ++ showIndent (ι + 1) ξ) (Data.Map.toAscList ε)) ++ "}"

instance (ShowIndent e) => ShowIndent (Store e) where
  showIndent ι σ = "[" ++ concat (map (\(α, ψ) -> indent (ι + 1) ++ show α ++ " -> " ++ showIndent (ι + 1) ψ) (Data.Map.toAscList σ)) ++ "]"

instance (ShowIndent v) => ShowIndent (Kontinuation v) where
  showIndent ι (Bind ε η γ κ) = "(bind " ++ η ++ showIndentOne ι ε ++ showIndentOne ι γ ++ showIndentOne ι κ ++ ")"
  showIndent ι (Apply ε γs ξs κ) = "(apply" ++ showIndentOne ι ε ++ showIndentAllBracket ι γs ++ showIndentAllBracket ι ξs ++ showIndentOne ι κ ++ ")"
  showIndent ι (Branch ε γ γ' κ) = "(branch " ++ showIndentOne ι ε ++ showIndentOne ι γ ++ showIndentOne ι γ' ++ showIndentOne ι κ ++ ")"
  showIndent _ Finish = "(finish)"

instance (ShowIndent v, ShowIndent e) => ShowIndent (State v e) where
  showIndent ι (Ongoing γ ε σ κ) = "(ongoing" ++ showIndentOne ι γ ++ showIndentOne ι ε ++ showIndentOne ι σ ++ showIndentOne ι κ ++ ")"
  showIndent ι (Success σ ξ) = "(success" ++ showIndentOne ι ξ ++ showIndentOne ι σ ++ ")"
  showIndent ι (Failure σ κ (τ, τ')) = "(failure " ++ τ ++ " >> " ++ τ' ++ showIndentOne ι σ ++ showIndentOne ι κ ++ ")"

printState :: (ShowIndent x, ShowIndent y) => State x y -> String
-- printState = showIndent 0
printState φ@(Ongoing _ _ _ _) = "Error >> expected a final state but got: " ++ showIndent 0 φ
printState φ@(Failure σ κ (τ, τ')) = "Failure >> " ++ τ ++ ": " ++ τ'
printState φ@(Success σ ξ) = "Success >> " ++ showIndent 0 ξ
