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
 | CloV (List String) ExprC (List Binding)
 | PrimV (Value -> Value -> Value)
 | ErrV String

type Environment = List Binding
type alias Binding = { name : String, val : Value }

topInterp expr =
  interp expr constructtopenv

interp : ExprC -> List Binding -> Value
interp e env = 
  case e of
    NumC num ->
      NumV num
    IdC id ->
      lookupenv env id
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
          ErrV "DXUQ: non-boolean test used with if statement"
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
            ErrV "DXUQ: wrong number of arguments to function"
        PrimV proc ->
          if not ((List.length argVals) == 2) then
            ErrV "DXUQ: expected two arguments to primitive"
          else
            case argVals of
              first :: second :: third ->
                proc first second
              [] ->
                ErrV "DXUQ: incorrect number of arguments"
              [_] ->
                ErrV "DXUQ: incorrect number of arguments"
        _ ->
          ErrV "DXUQ: invalid function"

lookupenv env name = 
    case env of
        [] ->
            let errmsg = "Unbound identifier: "++name
            in
                ErrV errmsg
        first :: rest ->
            if first.name == name then
                first.val
            else
                lookupenv rest name

addenv params argVals env =
  case params of
    [] ->
      env
    firstParam :: restParams ->
      let
        firstArgVals = List.head argVals
        restArgVals = List.tail argVals
      in
      case restArgVals of
        Nothing ->
          []
        Just restArgs ->
          case firstArgVals of
          Nothing ->
            []
          Just firstArg ->
            addenv restParams restArgs ((Binding firstParam firstArg) :: env)

add l r =
  case l of
    NumV ln ->
      case r of
        NumV rn ->
          let
            res = ln + rn
          in
            NumV res
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"

sub l r = 
  case l of
    NumV ln ->
      case r of
        NumV rn ->
          let
            res = ln - rn
          in
            NumV res
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"

mult l r = 
  case l of
    NumV ln ->
      case r of
        NumV rn ->
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
    NumV ln ->
      case r of
        NumV rn ->
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

leq l r = 
  case l of
    NumV ln ->
      case r of
        NumV rn ->
          if ln <= rn then
            BoolV True
          else
            BoolV False
        _ ->   
          ErrV "DXUQ: r not numC"
    _ -> 
      ErrV "DXUQ: l not numC"

constructtopenv = 
  [Binding "+" (PrimV add),
   Binding "-" (PrimV sub),
   Binding "*" (PrimV mult),
   Binding "/" (PrimV div),
   Binding "<=" (PrimV leq),
   -- Binding "equal?" (PrimV equal),
   -- Binding "error" (PrimV error),
   Binding "true" (BoolV True),
   Binding "false" (BoolV False)]
