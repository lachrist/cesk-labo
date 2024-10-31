module Weave where

import Expression (Expression (..), Location)
import Primitive (Primitive (String))

data JoinPoint
  = LiteralAfter
  | VariableAfter
  | ConditionBefore
  | ConditionAfter
  | BindingBefore
  | BindingAfter
  | LambdaAfter
  | LambdaEnter
  | LambdaLeave
  | ApplicationAround

type Pointcut = JoinPoint -> Location -> Bool

nameJoinPoint :: JoinPoint -> String
nameJoinPoint LiteralAfter = "literal-after"
nameJoinPoint VariableAfter = "variable-after"
nameJoinPoint ConditionBefore = "condition-before"
nameJoinPoint ConditionAfter = "condition-after"
nameJoinPoint BindingBefore = "binding-before"
nameJoinPoint BindingAfter = "binding-after"
nameJoinPoint ApplicationAround = "application-around"
nameJoinPoint LambdaAfter = "lambda-after"
nameJoinPoint LambdaEnter = "lambda-enter"
nameJoinPoint LambdaLeave = "lambda-leave"

cut :: JoinPoint -> [Expression] -> Location -> Expression
cut point arguments location =
  Application
    (Variable ("meta-" ++ nameJoinPoint point) location)
    (arguments ++ [Literal (Primitive.String (show location)) location])
    location

weaveTarget ::
  Pointcut ->
  JoinPoint ->
  Expression ->
  [Expression] ->
  Location ->
  Expression
weaveTarget pointcut point target optional location =
  if pointcut point location
    then cut point (target : optional) location
    else target

weaveApplication ::
  Pointcut ->
  JoinPoint ->
  Expression ->
  [Expression] ->
  Location ->
  Expression
weaveApplication pointcut point callee arguments location =
  if pointcut point location
    then
      cut
        point
        [callee, Application (Variable "list" location) arguments location]
        location
    else Application callee arguments location

weaveBegin ::
  Pointcut ->
  JoinPoint ->
  Expression ->
  [Expression] ->
  Location ->
  Expression
weaveBegin pointcut point result optional location =
  if pointcut point location
    then
      Application
        (Variable "begin" location)
        [ cut point optional location,
          result
        ]
        location
    else result

weave :: Pointcut -> Expression -> Expression
weave pointcut target@(Literal _ location) =
  weaveTarget
    pointcut
    LiteralAfter
    target
    []
    location
weave pointcut target@(Variable variable location) =
  weaveTarget
    pointcut
    VariableAfter
    target
    [Literal (String variable) location]
    location
weave pointcut (Condition test consequent alternate location) =
  weaveTarget
    pointcut
    ConditionAfter
    ( Condition
        ( weaveTarget
            pointcut
            ConditionBefore
            (weave pointcut test)
            []
            location
        )
        (weave pointcut consequent)
        (weave pointcut alternate)
        location
    )
    []
    location
weave pointcut (Binding variable right body location) =
  weaveTarget
    pointcut
    BindingAfter
    ( Binding
        variable
        ( weaveTarget
            pointcut
            BindingBefore
            (weave pointcut right)
            [Literal (Primitive.String variable) location]
            location
        )
        (weave pointcut body)
        location
    )
    []
    location
weave pointcut (Lambda parameters body location) =
  weaveTarget
    pointcut
    LambdaAfter
    ( Lambda
        parameters
        ( weaveTarget
            pointcut
            LambdaLeave
            ( weaveBegin
                pointcut
                LambdaEnter
                body
                [ Application
                    (Variable "list" location)
                    ( map
                        (\v -> Literal (Primitive.String v) location)
                        parameters
                    )
                    location,
                  Application
                    (Variable "list" location)
                    (map (`Variable` location) parameters)
                    location
                ]
                location
            )
            []
            location
        )
        location
    )
    []
    location
weave pointcut (Application callee arguments location) =
  weaveApplication pointcut ApplicationAround callee arguments location
