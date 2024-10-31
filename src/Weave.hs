module Weave where

import Expression (Expression (..))
import Primitive (Primitive (String))
import Text.Parsec (SourcePos)

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

type Pointcut = JoinPoint -> SourcePos -> Bool

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

cut :: JoinPoint -> [Expression] -> SourcePos -> Expression
cut point arguments position =
  Application
    (Variable $ "meta-" ++ nameJoinPoint point)
    (arguments ++ [Literal $ Primitive.String $ show position])

weaveTarget :: Pointcut -> JoinPoint -> Expression -> [Expression] -> SourcePos -> Expression
weaveTarget pointcut point target optional position =
  if pointcut point position
    then cut point (target : optional) position
    else target

weaveApplication :: Pointcut -> JoinPoint -> Expression -> [Expression] -> SourcePos -> Expression
weaveApplication pointcut point callee arguments position =
  if pointcut point position
    then cut point [callee, Application (Variable "list") arguments] position
    else Application callee arguments

weaveBegin :: Pointcut -> JoinPoint -> Expression -> [Expression] -> SourcePos -> Expression
weaveBegin pointcut point result optional position =
  if pointcut point position
    then
      Application
        (Variable "begin")
        [ cut point optional position,
          result
        ]
    else result

weave :: Pointcut -> Expression -> SourcePos -> Expression
weave pointcut target@(Literal _) position =
  weaveTarget pointcut LiteralAfter target [] position
weave pointcut target@(Variable variable) position =
  weaveTarget pointcut VariableAfter target [Literal (String variable)] position
weave pointcut (Condition test consequent alternate) position =
  weaveTarget
    pointcut
    ConditionAfter
    ( Condition
        (weaveTarget pointcut ConditionBefore test [] position)
        consequent
        alternate
    )
    []
    position
weave pointcut (Binding variable right body) position =
  weaveTarget
    pointcut
    BindingAfter
    ( Binding
        variable
        (weaveTarget pointcut BindingBefore right [Literal $ Primitive.String variable] position)
        body
    )
    []
    position
weave pointcut (Lambda parameters body) position =
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
                    (Variable "list")
                    ( map
                        (Literal . Primitive.String)
                        parameters
                    ),
                  Application
                    (Variable "list")
                    (map Variable parameters)
                ]
                position
            )
            []
            position
        )
    )
    []
    position
weave pointcut (Application callee arguments) position =
  weaveApplication pointcut ApplicationAround callee arguments position
