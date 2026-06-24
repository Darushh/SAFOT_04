datatype Atom = SYMBOL of string | NIL;
datatype SExp = ATOM of Atom | CONS of (SExp * SExp);

exception Undefined;
exception Empty;

fun initEnv () str = raise Undefined;

fun define str f bind = fn x => (if (x = str) then bind else f (x));

fun emptyNestedEnv () = [initEnv ()];

fun pushEnv env envList = env :: envList;

fun popEnv [] = raise Empty 
  | popEnv (env :: envList) = envList;

fun topEnv [] = raise Empty 
  | topEnv (env :: envList) = env;

fun defineNested _ [] _ = raise Empty
  | defineNested str (env :: envList) bind = (define str env bind) :: envList;

fun find _ [] = raise Undefined
  | find str (env :: envList) = (env str) handle Undefined => (find str envList); 
