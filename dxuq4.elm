type ExprC
 = NumC Float
 | IdC String
 | StringC String
 | IfC { test : ExprC, first : ExprC, second : ExprC }
 | LamC { params : List String, body : ExprC }
 | AppC { func : ExprC, args : List ExprC }

type Value
 = NumV Float
 | BoolV Bool
 | StringV String
 | CloV { params : List String, body : ExprC, env : Environment }
 | PrimV { p : List Value -> Value }

type Environment = List Binding
type alias Binding = { name : String, val : Value }

lookup-env : Environment -> String -> Value
lookup-env env name = 
    case env of
        [] ->
            Err "Unbound identifier: "++name
        first :: rest ->
            if first.name == name then
                first.val
            else
                lookup-env rest name

interp e env = 
  
