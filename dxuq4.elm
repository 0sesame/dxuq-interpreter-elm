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

--interp e env = 

add : ExprC -> ExprC ->  Float
add l r =
  case l of
    NumC ln ->
      case r of
        NumC rn ->
          ln + rn
      _ ->   
        Err "DXUQ: r not numC"
  _ -> 
    Err "DXUQ: l not numC"

    --l.float + r.float

subtract l r = 
    l.float - r.float

--multiply l 

  
