module ErrorMessages where

import AST

arg_type_mismatch :: String -> String -> String
arg_type_mismatch expected received =
  "For function application, expected argument of type " 
  ++ expected
  ++ " but received "
  ++ received

nonfunction_application :: String -> String
nonfunction_application received =
  "Attempted to apply non-function of type "
  ++ received

uninstantiated_app :: String -> String -> String
uninstantiated_app func_type arg_type =
  "Attempted to apply uninstantiated type "
  ++ func_type
  ++ " to type "
  ++ arg_type

undeclared_var :: String -> String
undeclared_var var_name =
  "Attempted to use undeclared variable <"
  ++ var_name ++ ">"

ifte_type_mismatch :: String -> String -> String
ifte_type_mismatch opt1_type opt2_type =
  "In if/then/else, the first option has type "
  ++ opt1_type
  ++ " and the second has type "
  ++ opt2_type

uninstantiated_ifte :: String -> String -> String
uninstantiated_ifte opt1_type opt2_type =
  "In if/then/else, neither the first option type, "
  ++ opt1_type
  ++ "or the second option type, "
  ++ opt2_type
  ++ " is instantiated"

nonbool_cond :: String -> String
nonbool_cond cond_type =
  "In if/then/else, the condition is of type "
  ++ cond_type
  ++ ", not of type Bool."

type_annotation_mismatch :: String -> String -> String
type_annotation_mismatch annotation_type exp_type =
  "Mismatch between the type annotation " ++
  annotation_type ++ "and the actual type "
  ++ exp_type

recursive_nonfunction :: String
recursive_nonfunction = "Attempted to define non-function recursively"