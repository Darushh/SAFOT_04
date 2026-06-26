use "hw4_q1.sml"; (*qusetion 1*)
use "hw3_q3.sml"; (*parser from hw3*)

exception LispError;

(* Helper function - feel free to delete *)
fun first (x, _) = x;

local
    fun tokenize x = 
        String.tokens (fn c => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

    (* Helper functions - feel free to delete *)
    (* ====================================== *)
    fun is_digit c = c >= #"0" andalso c <= #"9";

    fun is_number str =
        let
            fun check [] = true
              | check (c::cs) = is_digit c andalso check cs
            
            val chars = String.explode str
        in
            case chars of
                [] => false
              | #"~" :: cs => not (List.null cs) andalso check cs
              | #"-" :: cs => not (List.null cs) andalso check cs
              | cs => check cs
        end;
        
    fun char_to_int c = ord(c) - ord(#"0")

    fun string_to_int str =
        let
            fun convert [] acc = acc
              | convert (c::cs) acc = convert cs (10 * acc + char_to_int c)
            val chars = String.explode str
        in
            case chars of
                #"~" :: cs => ~ (convert cs 0)
              | #"-" :: cs => ~ (convert cs 0)
              | cs => convert cs 0
        end;

    fun sexp_to_int sexp =
        case sexp of
            ATOM (SYMBOL s) => string_to_int s
          | _ => raise LispError;

    fun is_nil NIL = true
      | is_nil (SYMBOL "nil") = true
      | is_nil (SYMBOL "NIL") = true
      | is_nil _ = false;

    fun eq_atoms a1 a2 =
        a1 = a2 orelse (is_nil a1 andalso is_nil a2);

    (*function bind args for updating enviroment for each argument*)
    fun bindArgs (ATOM NIL) (ATOM NIL) currentEnv = currentEnv
      | bindArgs (CONS (ATOM (SYMBOL name), nextNames)) (CONS (value, nextValues)) currentEnv =
        let
            val updatedEnv = define name currentEnv value
        in
            bindArgs nextNames nextValues updatedEnv
        end
      | bindArgs _ _ _ = raise LispError;

in
    fun evalSExp (ATOM NIL) env = (ATOM NIL, env)
      | evalSExp (ATOM (SYMBOL "nil")) env = (ATOM NIL, env)
      | evalSExp (ATOM (SYMBOL "NIL")) env = (ATOM NIL, env)
      | evalSExp (ATOM (SYMBOL "t")) env = (ATOM (SYMBOL "t"), env)
      | evalSExp (ATOM (SYMBOL "T")) env = (ATOM (SYMBOL "t"), env)
      | evalSExp (ATOM (SYMBOL s)) env = 
            if is_number s then (ATOM (SYMBOL s), env) (*if it's a number*)
            else ((find s env, env) handle _ => raise LispError)

      | evalSExp (CONS (ATOM (SYMBOL "quote"), CONS (arg, ATOM NIL))) env = (arg, env)

      (*CAR function*)
      | evalSExp (CONS (ATOM (SYMBOL "car"), CONS (arg, ATOM NIL))) env =
        let
            val (evaluatedArg, _) = evalSExp arg env
        in
            case evaluatedArg of
                CONS (left, right) => (left, env) (*first element in pair*)
              | _ => raise LispError (*error - CAR on NIL or ATOM*)
        end

      (*CDR function*)
      | evalSExp (CONS (ATOM (SYMBOL "cdr"), CONS (arg, ATOM NIL))) env =
        let
            val (evaluatedArg, _) = evalSExp arg env
        in
            case evaluatedArg of
                CONS (left, right) => (right, env) (*second element in pair*)
              | _ => raise LispError (*error - CDR on NIL or ATOM*)
        end

      (*CONS function*)
      | evalSExp (CONS (ATOM (SYMBOL "cons"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            (*evaluating the arguments*)
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
        in
            (CONS (v1, v2), env) (*creating a new pair and returning*)
        end                        

      (* atom function *)
      | evalSExp (CONS (ATOM (SYMBOL "atom"), CONS (arg, ATOM NIL))) env =
        let val (v, _) = evalSExp arg env
        in case v of
               ATOM _ => (ATOM (SYMBOL "t"), env)
             | _ => (ATOM NIL, env)
        end

      (* null function *)
      | evalSExp (CONS (ATOM (SYMBOL "null"), CONS (arg, ATOM NIL))) env =
        let val (v, _) = evalSExp arg env
        in case v of
               ATOM NIL => (ATOM (SYMBOL "t"), env)
             | ATOM (SYMBOL "nil") => (ATOM (SYMBOL "t"), env)
             | ATOM (SYMBOL "NIL") => (ATOM (SYMBOL "t"), env)
             | _ => (ATOM NIL, env)
        end

      (* + function *)
      | evalSExp (CONS (ATOM (SYMBOL "+"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
            val res = sexp_to_int v1 + sexp_to_int v2
        in
            (ATOM (SYMBOL (Int.toString res)), env)
        end

      (* - function *)
      | evalSExp (CONS (ATOM (SYMBOL "-"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
            val res = sexp_to_int v1 - sexp_to_int v2
        in
            (ATOM (SYMBOL (Int.toString res)), env)
        end

      (* * function *)
      | evalSExp (CONS (ATOM (SYMBOL "*"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
            val res = sexp_to_int v1 * sexp_to_int v2
        in
            (ATOM (SYMBOL (Int.toString res)), env)
        end

      (* / function *)
      | evalSExp (CONS (ATOM (SYMBOL "/"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
            val res = (sexp_to_int v1) div (sexp_to_int v2)
        in
            (ATOM (SYMBOL (Int.toString res)), env)
        end

      (* mod function *)
      | evalSExp (CONS (ATOM (SYMBOL "mod"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
            val res = (sexp_to_int v1) mod (sexp_to_int v2)
        in
            (ATOM (SYMBOL (Int.toString res)), env)
        end

      (* = function *)
      | evalSExp (CONS (ATOM (SYMBOL "="), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
        in
            if sexp_to_int v1 = sexp_to_int v2 then
                (ATOM (SYMBOL "t"), env)
            else
                (ATOM NIL, env)
        end

      (* /= function (Not Equal) *)
      | evalSExp (CONS (ATOM (SYMBOL "/="), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
        in
            if sexp_to_int v1 <> sexp_to_int v2 then 
                (ATOM (SYMBOL "t"), env)
            else
                (ATOM NIL, env)
        end

      (* < function *)
      | evalSExp (CONS (ATOM (SYMBOL "<"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
        in
            if sexp_to_int v1 < sexp_to_int v2 then
                (ATOM (SYMBOL "t"), env)
            else
                (ATOM NIL, env)
        end

      (* > function *)
      | evalSExp (CONS (ATOM (SYMBOL ">"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
        in
            if sexp_to_int v1 > sexp_to_int v2 then
                (ATOM (SYMBOL "t"), env)
            else
                (ATOM NIL, env)
        end

      (* eq function *)
      | evalSExp (CONS (ATOM (SYMBOL "eq"), CONS (arg1, CONS (arg2, ATOM NIL)))) env =
        let
            val (v1, _) = evalSExp arg1 env
            val (v2, _) = evalSExp arg2 env
        in
            case (v1, v2) of
                (ATOM a1, ATOM a2) => 
                if eq_atoms a1 a2 then (ATOM (SYMBOL "t"), env) else (ATOM NIL, env)
              | _ => (ATOM NIL, env) (*if not both an ATOM return NIL*)
        end

      (* cond function *)
      | evalSExp (CONS (ATOM (SYMBOL "cond"), pairs)) env =
        let
            fun evalCond (ATOM NIL) = (ATOM NIL, env) (*if no pairs left - return NIL*)
              | evalCond (CONS (CONS (condition, CONS (exp, ATOM NIL)), restPairs)) =
                let
                    val (condVal, _) = evalSExp condition env
                in
                    case condVal of
                        ATOM NIL => evalCond restPairs (*if cond false we continue to rest*)
                      | ATOM (SYMBOL "nil") => evalCond restPairs
                      | ATOM (SYMBOL "NIL") => evalCond restPairs
                      | _ => evalSExp exp env          
                end
              | evalCond _ = raise LispError (*illegal pair structure*)
        in
            evalCond pairs
        end

      (*lambda function*)
      | evalSExp (CONS (CONS (ATOM (SYMBOL "lambda"), CONS (formals, CONS (body, ATOM NIL))), actuals)) env =
        let
            fun evalList (ATOM NIL) = ATOM NIL
              | evalList (CONS (arg, rest)) = 
                let val (v, _) = evalSExp arg env
                in CONS (v, evalList rest) end
              | evalList _ = raise LispError

            val evaluatedActuals = evalList actuals
            (*new environment and binding args*)
            val newLocalEnv = bindArgs formals evaluatedActuals (initEnv ())
            (*pushing to stack new environment*)
            val extendedEnvStack = pushEnv newLocalEnv env
            (*evaluating the body*)
            val (result, _) = evalSExp body extendedEnvStack
        in
            (*returning the result with original environment*)
            (result, env)
        end

      (* label function *)
      | evalSExp (CONS (CONS (ATOM (SYMBOL "label"), CONS (ATOM (SYMBOL funcName), CONS (lambdaExpr, ATOM NIL))), actuals)) env =
        let
            val localEnv = initEnv () 
            val localEnvWithFunc = define funcName localEnv lambdaExpr 
            val extendedEnvStack = pushEnv localEnvWithFunc env 
            val callExpression = CONS (lambdaExpr, actuals)
            val (result, _) = evalSExp callExpression extendedEnvStack
        in
            (result, env)
        end

      (* calling a function defined in the environment *)
      | evalSExp (CONS (ATOM (SYMBOL funcName), actuals)) env = 
        let
            val lambdaExpr = find funcName env handle _ => raise LispError 
            val callExpression = CONS (lambdaExpr, actuals)
        in
            evalSExp callExpression env
        end

      | evalSExp _ _ = raise LispError (*illegal structure - catch all*)

    fun eval string_exp env =
        let
            val parsedSExp = parse (tokenize string_exp)
        in
            evalSExp parsedSExp env
        end
        handle _ => (ATOM (SYMBOL "lisp-error"), env)
end;
