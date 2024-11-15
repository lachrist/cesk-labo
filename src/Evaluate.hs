{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluate (eval) where

import Continuation (Continuation (..))
import Control.Arrow (second)
import Data.Map (empty, fromList, insert, lookup, union)
import Domain (BuiltinName, Domain (Builtin, Closure, Pair, Primitive), builtins, isTruthy)
import Environment (Environment)
import Error (Error (ApplicationError, MissingVariableError), Message)
import Expression (Expression (..), Location (Location))
import Primitive (Primitive (Boolean, Null, Number, String))
import Serial (Serializable (serialize), render)
import State (Outcome (Failure, Success), State (Final, Ongoing))
import Storage (Storage (get, new, set))
import Store (Store, initial)

accumulateBuiltin :: (Eq v, Serializable v, Storage x v) => BuiltinName -> (Store x, Environment v) -> (Store x, Environment v)
accumulateBuiltin tag (mem1, env) =
  second (\val -> insert tag val env) (new mem1 (Builtin tag))

eval :: (Eq v, Serializable v, Storage x v) => Expression -> IO (Outcome x v)
eval cur =
  let (mem, env) = foldr accumulateBuiltin (initial, empty) builtins
   in loop (Ongoing cur env mem Finish)

loop :: (Eq v, Serializable v, Storage x v) => State x v -> IO (Outcome x v)
loop cur@(Ongoing {}) = step cur >>= loop
loop (Final out) = return out

step :: (Eq v, Serializable v, Storage x v) => State x v -> IO (State x v)
step (Ongoing (Literal prm _) _ mem nxt) =
  continue nxt (new mem (Primitive prm))
step (Ongoing (Lambda vars body _) env mem nxt) =
  continue nxt (new mem (Closure env vars body))
step (Ongoing (Variable var loc) env mem nxt) =
  maybe
    (return $ Final $ Failure mem (MissingVariableError var loc))
    (continue nxt . (mem,))
    (Data.Map.lookup var env)
step (Ongoing (Binding var val res _) env mem nxt) =
  return $ Ongoing val env mem (Bind env var res nxt)
step (Ongoing (Application fct args loc) env mem nxt) =
  return $ Ongoing fct env mem (Apply env args [] nxt loc)
step (Ongoing (Condition test cons alt _) env mem nxt) =
  return $ Ongoing test env mem (Branch env cons alt nxt)
step final = return final

shift :: [a] -> a -> (a, [a])
shift [] x1 = (x1, [])
shift (x1 : xs) x2 = (x1, xs ++ [x2])

continue :: (Eq v, Serializable v, Storage x v) => Continuation v -> (Store x, v) -> IO (State x v)
continue Finish (mem, val) =
  return $ Final $ Success mem val
continue (Branch env pos neg nxt) (mem, val) =
  return $ Ongoing (if isTruthy (get mem val) then pos else neg) env mem nxt
continue (Bind env var res nxt) (mem, val) =
  return $ Ongoing res (insert var val env) mem nxt
continue (Apply env (arg : args) vals nxt loc) (mem, val) =
  return $ Ongoing arg env mem (Apply env args (vals ++ [val]) nxt loc)
continue (Apply _ [] vals nxt loc) (mem, val) =
  let (fct, args) = shift vals val
   in apply (get mem fct, fct, args, loc) (mem, nxt)

apply :: (Eq v, Serializable v, Storage x v) => (Domain v, v, [v], Location) -> (Store x, Continuation v) -> IO (State x v)
apply (Closure env vars res, fct, args, loc) (mem, nxt)
  | length vars == length args =
      return $ Ongoing res (fromList (zip vars args) `union` env) mem nxt
  | otherwise = return $ Final $ Failure mem (ApplicationError "arity mismatch" fct args loc)
apply (Builtin tag, fct, args, loc) (mem, nxt) =
  applyReflectBuiltin (tag, fct, args, loc) (mem, nxt)
apply (_, fct, args, loc) (mem, _) =
  return $ Final $ Failure mem (ApplicationError "cannot apply callee" fct args loc)

collect :: (Storage x v) => Store x -> Domain v -> [v]
collect mem (Pair fst snd) = fst : collect mem (get mem snd)
collect _ _ = []

trace :: (Serializable v) => Location -> BuiltinName -> [v] -> v -> String
trace (Location _ lin col) tag args val =
  render 0 (serialize val)
    ++ " <- ("
    ++ tag
    ++ concatMap (("  " ++) . render 0 . serialize) args
    ++ ")  @"
    ++ show lin
    ++ ":"
    ++ show col

applyReflectBuiltin :: (Eq v, Serializable v, Storage x v) => (BuiltinName, v, [v], Location) -> (Store x, Continuation v) -> IO (State x v)
applyReflectBuiltin ("apply", fct, [arg0, arg1], loc) (mem, nxt) =
  apply (get mem arg0, fct, collect mem (get mem arg1), loc) (mem, nxt)
applyReflectBuiltin (tag, fct, args, loc) (mem1, nxt) = do
  res <- applyActionBuiltin (tag, args, loc) mem1
  case res of
    Left msg -> return $ Final $ Failure mem1 (ApplicationError msg fct args loc)
    Right (mem2, val) -> continue nxt (mem2, val)

applyActionBuiltin :: (Eq v, Serializable v, Storage x v) => (BuiltinName, [v], Location) -> Store x -> IO (Either Message (Store x, v))
applyActionBuiltin ("trace", arg0 : args, loc) mem1 =
  case get mem1 arg0 of
    Builtin tag -> do
      out <- applyActionBuiltin (tag, args, loc) mem1
      case out of
        Left msg -> return $ Left msg
        Right (mem2, res) -> do
          putStrLn $ trace loc tag args res
          return $ Right (mem2, res)
    _ -> return $ Left "arg0 should be a builtin"
applyActionBuiltin ("read-line", [], _) mem1 = do
  str <- getLine
  let (mem2, val) = new mem1 (Primitive $ Primitive.String str)
  return $ Right (mem2, val)
applyActionBuiltin ("display", [arg], _) mem1 =
  case get mem1 arg of
    Primitive (String str) -> do
      putStr str >> return (Right $ new mem1 (Primitive Null))
    _ -> return $ Left "arg0 should be a string"
applyActionBuiltin (tag, args, _) mem1 = return $ applyValueBuiltin (tag, args) mem1

accumulateList :: (Storage x v) => v -> (Store x, v) -> (Store x, v)
accumulateList fst (mem, snd) = new mem (Pair fst snd)

toCons :: Domain v -> Either Message (v, v)
toCons (Pair fst snd) = Right (fst, snd)
toCons _ = Left "arg0 should be a cons"

applyValueBuiltin :: (Eq v, Serializable v, Storage x v) => (BuiltinName, [v]) -> Store x -> Either Message (Store x, v)
applyValueBuiltin ("begin", args) mem =
  Right (mem, last args)
applyValueBuiltin ("inspect", [arg0]) mem =
  Right $ new mem (Primitive $ String $ render 0 (serialize arg0))
applyValueBuiltin ("set!", [arg0, arg1]) mem =
  fmap (,arg1) (set mem arg0 (get mem arg1))
applyValueBuiltin ("eq?", [arg0, arg1]) mem =
  Right $ new mem (Primitive $ Boolean $ arg0 == arg1)
applyValueBuiltin ("cons", [arg0, arg1]) mem =
  Right $ new mem (Pair arg0 arg1)
applyValueBuiltin ("list", args) mem =
  Right $ foldr accumulateList (new mem (Primitive Null)) args
applyValueBuiltin ("car", [arg]) mem =
  fmap ((mem,) . fst) (toCons (get mem arg))
applyValueBuiltin ("cdr", [arg]) mem =
  fmap ((mem,) . snd) (toCons (get mem arg))
applyValueBuiltin ("set-car!", [arg0, arg1]) mem = do
  (_, cdr) <- toCons (get mem arg0)
  fmap (,arg1) (set mem arg0 (Pair arg1 cdr))
applyValueBuiltin ("set-cdr!", [arg0, arg1]) mem = do
  (car, _) <- toCons (get mem arg0)
  fmap (,arg1) (set mem arg0 (Pair car arg1))
applyValueBuiltin (tag, args) mem =
  fmap (new mem) (applyDataBuiltin (tag, map (get mem) args))

fromString :: Domain v -> Maybe String
fromString (Primitive (String str)) = Just str
fromString _ = Nothing

applyDataBuiltin :: (Eq v) => (BuiltinName, [Domain v]) -> Either Message (Domain v)
-- raise --
applyDataBuiltin ("raise", [Primitive (String msg)]) =
  Left msg
-- type predicate --
applyDataBuiltin ("null?", [Primitive Null]) =
  Right $ Primitive $ Boolean True
applyDataBuiltin ("null?", [_]) =
  Right $ Primitive $ Boolean False
applyDataBuiltin ("boolean?", [Primitive (Boolean _)]) =
  Right $ Primitive $ Boolean True
applyDataBuiltin ("boolean?", [_]) =
  Right $ Primitive $ Boolean False
applyDataBuiltin ("number?", [Primitive (Number _)]) =
  Right $ Primitive $ Boolean True
applyDataBuiltin ("number?", [_]) =
  Right $ Primitive $ Boolean False
applyDataBuiltin ("string?", [Primitive (String _)]) =
  Right $ Primitive $ Boolean True
applyDataBuiltin ("string?", [_]) =
  Right $ Primitive $ Boolean False
applyDataBuiltin ("pair?", [Pair _ _]) =
  Right $ Primitive $ Boolean True
applyDataBuiltin ("pair?", [_]) =
  Right $ Primitive $ Boolean False
applyDataBuiltin ("procedure?", [Closure {}]) =
  Right $ Primitive $ Boolean True
applyDataBuiltin ("procedure?", [Builtin _]) =
  Right $ Primitive $ Boolean True
applyDataBuiltin ("procedure?", [_]) =
  Right $ Primitive $ Boolean False
-- data equality --
applyDataBuiltin ("eqv?", [arg0, arg1]) =
  Right $ Primitive $ Boolean $ arg0 == arg1
-- boolean arithmetic --
applyDataBuiltin ("&&", [Primitive (Boolean arg0), Primitive (Boolean arg1)]) =
  Right $ Primitive $ Boolean $ arg0 && arg1
applyDataBuiltin ("||", [Primitive (Boolean arg0), Primitive (Boolean arg1)]) =
  Right $ Primitive $ Boolean $ arg0 || arg1
-- numerical comparison --
applyDataBuiltin ("=", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Boolean $ arg0 == arg1
applyDataBuiltin ("<", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Boolean $ arg0 < arg1
applyDataBuiltin ("<=", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Boolean $ arg0 <= arg1
applyDataBuiltin (">", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Boolean $ arg0 > arg1
applyDataBuiltin (">=", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Boolean $ arg0 >= arg1
-- numerical arithmetic --
applyDataBuiltin ("+", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Number $ arg0 + arg1
applyDataBuiltin ("-", [Primitive (Number arg)]) =
  Right $ Primitive $ Number $ -arg
applyDataBuiltin ("-", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Number $ arg0 - arg1
applyDataBuiltin ("*", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Number $ arg0 * arg1
applyDataBuiltin ("/", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Number $ arg0 / arg1
applyDataBuiltin ("expt", [Primitive (Number arg0), Primitive (Number arg1)]) =
  Right $ Primitive $ Number $ arg0 ** arg1
applyDataBuiltin ("sqrt", [Primitive (Number arg0)]) =
  Right $ Primitive $ Number $ sqrt arg0
-- string manipulation --
applyDataBuiltin ("string-append", args) =
  case mapM fromString args of
    Just strs -> Right $ Primitive $ String $ concat strs
    Nothing -> Left "can only concatenate string"
applyDataBuiltin ("string-length", [Primitive (String arg0)]) =
  Right $ Primitive $ Number $ fromIntegral $ length arg0
applyDataBuiltin ("substring", [Primitive (String arg0), Primitive (Number arg1), Primitive (Number arg2)]) =
  Right $ Primitive $ String $ take (round $ arg2 - arg1) (drop (round arg1) arg0)
applyDataBuiltin ("number->string", [Primitive (Number arg0)]) =
  Right $ Primitive $ String $ show arg0
applyDataBuiltin ("string->number", [Primitive (String arg0)]) =
  case reads arg0 of
    [(arg, "")] -> Right $ Primitive $ Number arg
    _ -> Left "cannot parse as number"
-- falltrough --
applyDataBuiltin _ = Left "generic builtin error"
