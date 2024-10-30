module Instrument where

import Control

meta :: String -> [Expression] -> Expression
meta τ γs = Application (Variable $ "meta-" ++ τ) γs

-- instrument :: Control.Control -> Control.Control
-- instrument (Control.Literal θ) = meta "literal" [Control.Literal θ]
-- instrument (Control.Variable η) = meta "variable" [Literal $ Literal.String η, Variable η]
-- instrument (Control.Condition γ γ' γ'') = Condition (meta "condition" [instrument γ]) (instrument γ') (instrument γ'')
-- instrument (Control.Application γ γs) = meta "application" [instrument γ, Application (Variable "list") (map instrument γs)]
-- instrument (Control.Binding η γ γ') = Binding η (meta "binding" [Literal $ Literal.String η, instrument γ]) (instrument γ')
