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
                lookupenv rest name

interp e env = 
  case e of
    NumC num ->
      NumV num
    IdC id ->
      lookupenv id env
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
            interp body (addenv params argVals cloEnv)
          else
            Err "DXUQ: expected " ++ (List.length params) ++ " arguments to function, given: " ++ (List.length argVals)
        PrimV proc ->
          proc argVals
        _ ->
          Err "DXUQ: " ++ fval ++ " cannot be used as a function"





add l r =
  case l of
    NumC ln ->
      case r of
        NumC rn ->
          let
            res = ln + rn
          in
            NumV res
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"


subtract l r = 
  case l of
    NumC ln ->
      case r of
        NumC rn ->
          let
            res = ln - rn
          in
            NumV res
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"


multiply l r = 
  case l of
    NumC ln ->
      case r of
        NumC rn ->
          let
            res = ln + rn
          in
            NumV res
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"


div l r =
  case l of
    NumC ln ->
      case r of
        NumC rn ->
          if rn == 0 then
            ErrV "DXUQ: divide by 0"
          else
            let
                res = ln / rn
            in
                NumV res
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"


lt l r = 
  case l of
    NumC ln ->
      case r of
        NumC rn ->
          if ln <= rn then
            BoolV True
          else
            BoolV False
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"

--error primitive funciton, still working on