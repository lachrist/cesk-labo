{-# LANGUAGE TupleSections #-}

module Evaluate (eval) where

import Continuation (Continuation (..))
import Data (BuiltinName, Data (Builtin, Closure, Cons, Primitive), builtins, isTruthy)
import Data.Map (empty, fromList, insert, lookup, union)
import Environment (Environment)
import Error (Error (BuiltinApplicationError, ClosureApplicationError, MissingVariableError), Message)
import Expression (Expression (..), Location)
import Primitive (Primitive (Boolean, Null, Number, String))
import State (State (Failure, Ongoing, Success))
import Storage (Storage (get, new, set))

accumulateBuiltin :: (Eq v, Show v, Storage s v) => BuiltinName -> (s, Environment v) -> (s, Environment v)
accumulateBuiltin key (mem1, env) =
  let (mem2, val) = new mem1 (Builtin key)
   in (mem2, insert key val env)

eval :: (Eq v, Show v, Storage s v) => s -> Expression -> IO (s, Either (Error v) v)
eval mem1 cur =
  let (mem2, env) = foldr accumulateBuiltin (mem1, empty) builtins
   in loop (Ongoing cur env mem2 Finish)

loop :: (Eq v, Show v, Storage s v) => State s v -> IO (s, Either (Error v) v)
loop cur@(Ongoing {}) = step cur >>= loop
loop (Success val mem) = return (mem, Right val)
loop (Failure err mem) = return (mem, Left err)

step :: (Eq v, Show v, Storage s v) => State s v -> IO (State s v)
step (Ongoing (Literal prm _) _ mem nxt) =
  continue nxt (new mem (Primitive prm))
step (Ongoing (Lambda vars body _) env mem nxt) =
  continue nxt (new mem (Closure env vars body))
step (Ongoing (Variable var loc) env mem nxt) =
  maybe
    (return $ Failure (MissingVariableError var loc) mem)
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

continue :: (Eq v, Show v, Storage s v) => Continuation v -> (s, v) -> IO (State s v)
continue Finish (mem, val) =
  return $ Success val mem
continue (Branch env pos neg nxt) (mem, val) =
  return $ Ongoing (if isTruthy (get mem val) then pos else neg) env mem nxt
continue (Bind env var res nxt) (mem, val) =
  return $ Ongoing res (insert var val env) mem nxt
continue (Apply env (arg : args) vals nxt loc) (mem, val) =
  return $ Ongoing arg env mem (Apply env args (vals ++ [val]) nxt loc)
continue (Apply _ [] vals nxt loc) (mem, val) =
  let (fct, args) = shift vals val
   in apply (get mem fct, args, loc) (mem, nxt)

apply :: (Eq v, Show v, Storage s v) => (Data v, [v], Location) -> (s, Continuation v) -> IO (State s v)
apply (fct@(Closure env vars res), args, loc) (mem, nxt)
  | length vars == length args =
      return $ Ongoing res (fromList (zip vars args) `union` env) mem nxt
  | otherwise = return $ Failure (ClosureApplicationError "arity mismatch" fct args loc) mem
apply (Builtin key, args, loc) (mem, nxt) =
  applyReflectBuiltin (key, args, loc) (mem, nxt)
apply (fct, args, loc) (mem, _) =
  return $ Failure (ClosureApplicationError "cannot apply callee" fct args loc) mem

collect :: (Storage s v) => s -> Data v -> [v]
collect mem (Cons car cdr) = car : collect mem (get mem cdr)
collect _ _ = []

applyReflectBuiltin :: (Eq v, Show v, Storage s v) => (BuiltinName, [v], Location) -> (s, Continuation v) -> IO (State s v)
applyReflectBuiltin ("apply", [arg0, arg1], loc) (mem, nxt) =
  apply (get mem arg0, collect mem (get mem arg1), loc) (mem, nxt)
applyReflectBuiltin (key, args, loc) (mem1, nxt) = do
  res <- applyActionBuiltin (key, args) mem1
  case res of
    Left msg -> return $ Failure (BuiltinApplicationError msg key args loc) mem1
    Right (mem2, val) -> continue nxt (mem2, val)

applyActionBuiltin :: (Eq v, Show v, Storage s v) => (BuiltinName, [v]) -> s -> IO (Either Message (s, v))
applyActionBuiltin ("read-line", []) mem1 = do
  str <- getLine
  let (mem2, val) = new mem1 (Primitive $ Primitive.String str)
  return $ Right (mem2, val)
applyActionBuiltin ("print", [arg]) mem1 =
  case get mem1 arg of
    Primitive (String str) -> putStrLn str >> return (Right (mem1, arg))
    _ -> return $ Left "arg0 should be a string"
applyActionBuiltin (key, args) mem1 = return $ applyValueBuiltin (key, args) mem1

accumulateList :: (Storage s v) => v -> (s, v) -> (s, v)
accumulateList car (mem, cdr) = new mem (Cons car cdr)

toCons :: Data v -> Either Message (v, v)
toCons (Cons car cdr) = Right (car, cdr)
toCons _ = Left "arg0 should be a cons"

applyValueBuiltin :: (Eq v, Show v, Storage s v) => (BuiltinName, [v]) -> s -> Either Message (s, v)
applyValueBuiltin ("begin", args) mem =
  Right (mem, last args)
applyValueBuiltin ("inspect", [arg0]) mem =
  Right $ new mem (Primitive $ String $ show arg0)
applyValueBuiltin ("set!", [arg0, arg1]) mem =
  fmap (,arg1) (set mem arg0 (get mem arg1))
applyValueBuiltin ("eq?", [arg0, arg1]) mem =
  Right $ new mem (Primitive $ Boolean $ arg0 == arg1)
applyValueBuiltin ("cons", [arg0, arg1]) mem =
  Right $ new mem (Cons arg0 arg1)
applyValueBuiltin ("list", args) mem =
  Right $ foldr accumulateList (new mem (Primitive Null)) args
applyValueBuiltin ("car", [arg]) mem =
  fmap ((mem,) . fst) (toCons (get mem arg))
applyValueBuiltin ("cdr", [arg]) mem =
  fmap ((mem,) . snd) (toCons (get mem arg))
applyValueBuiltin ("set-car!", [arg0, arg1]) mem = do
  (_, cdr) <- toCons (get mem arg0)
  fmap (,arg1) (set mem arg0 (Cons arg1 cdr))
applyValueBuiltin ("set-cdr!", [arg0, arg1]) mem = do
  (car, _) <- toCons (get mem arg0)
  fmap (,arg1) (set mem arg0 (Cons car arg1))
applyValueBuiltin (key, args) mem =
  fmap (new mem) (applyDataBuiltin (key, map (get mem) args))

applyDataBuiltin :: (Eq v) => (BuiltinName, [Data v]) -> Either Message (Data v)
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
applyDataBuiltin ("pair?", [Cons _ _]) =
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
applyDataBuiltin ("string-append", [Primitive (String arg0), Primitive (String arg1)]) =
  Right $ Primitive $ String $ arg0 ++ arg1
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
