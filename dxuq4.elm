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

lookupenv env name = 
    case env of
        [] ->
            let errmsg = "Unbound identifier: "++name
            in
                Err errmsg
        first :: rest ->
            if first.name == name then
                first.val
            else
                lookupenv rest name 
