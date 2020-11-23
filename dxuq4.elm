type ExprC
 = NumC Float
 | IdC String
 | StringC String
 | IfC ExprC ExprC ExprC
 | LamC (List String) ExprC
 | AppC ExprC (List ExprC)

type Value
 = NumV Float
 | BoolV Bool
 | StringV String
 | CloV (List String) ExprC Environment
 | PrimV ((List Value) -> Value)

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

interp e env = 
  case e of
    NumC num ->
      NumV num
    IdC id ->
      lookup-env id env
    StringC str ->
      StringV str
    IfC test first second ->
      let
        testBool = interp test env
      in
      case testBool of
        BoolV b -> 
          if b then (interp first env) else (interp second env)
        _ ->
          Err "DXUQ: non-boolean test " ++ testBool ++ " used with if statement"
    LamC params body ->
      CloV params body env
    AppC f args ->
      let
        argVals = List.map (\expr -> interp expr env) args
        fval = interp f env
      in
      case fval of
        CloV params body cloEnv ->
          if (List.length params) == (List.length argVals) then
            interp body (add-env params argVals cloEnv)
          else
            Err "DXUQ: expected " ++ (List.length params) ++ " arguments to function, given: " ++ (List.length argVals)
        PrimV proc ->
          proc argVals
        _ ->
          Err "DXUQ: " ++ fval ++ " cannot be used as a function"
                lookupenv rest name 
