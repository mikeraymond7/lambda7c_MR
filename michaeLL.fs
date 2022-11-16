module MichaeLL
open Recless.Base;
open Fussless;
open System;
open System.IO;
open System.Collections.Generic;

// semantic value type
type expr = 
  | Integer of int
  | Floatpt of float
  | Strlit of string
  | Var of string
  | TypedVar of lltype*string
  | Nil
  | Binop of string*expr*expr
  | Uniop of string*expr
  | Ifelse of expr*expr*expr
  | Whileloop of expr*expr
  | Define of LBox<string>*expr
  //| Declare of LBox<lltype*string>*(LBox<expr> list)
  | TypedDefine of LBox<lltype*string>*expr
  | Lambda of (string list)*expr
  | TypedLambda of (LBox<expr> list)*lltype*LBox<expr>
  | Let of LBox<string>*expr*LBox<expr>
  | TypedLet of LBox<lltype*string>*expr*LBox<expr>
  //| Quote of expr
  | Setq of LBox<string>*expr
  | SetVQ of LBox<string>*int*expr
  | VGet of LBox<string>*int
  | VMake of LBox<int*int>
  | Sequence of LBox<expr> list
  //| Beginseq of LBox<expr> list
  | TypeExpr of lltype
  | Typedval of (lltype*expr)
  | Label of string
  | Error

and lltype = 
  | LLint | LLfloat | LLstring 
  | LList of lltype | LLtuple of lltype list
  | LLfun of (lltype list)*lltype
  | LLvec of lltype
  | LLunknown | LLuntypable | LLvar of string | LLunit


// Semantic Actions

// Binop --> + | - | * | DIV | ...
// Uniop --> not | ~ | car | cdr | ...
let semactLab (rhs:Vec<Stackitem<expr>>) = 
  Label(rhs.[0].symbol)

// Expr --> Uniop Axpr
let semactUni (rhs:Vec<Stackitem<expr>>) = 
  match(rhs.[0].value, rhs.[1].value) with
    | (Label(z), a) -> Uniop(z, a)
    | _ -> Error

// GetBin --> Binop Axpr Axpr 
let semactBin (rhs:Vec<Stackitem<expr>>) = 
     // Lbox is alias for Stackitem... to use lbox, don't use ".value"
  match(rhs.[0].value, rhs.[1].value, rhs.[2].value) with
    | (Label(z), a, b) -> Binop(z, a, b)
    | _ -> Error

// Var --> var typeopt
// typeopt --> COLON Txpr
// typeopt --> 
let semactVar(rhs:Vec<Stackitem<expr>>) = 
    match (rhs.[0].value, rhs.[1].value) with
      | Var(x), TypeExpr(LLunknown) -> Var(x)
      | Var(x), TypeExpr(ty) -> TypedVar(ty,x)
      | _ -> Error

// Expr --> if ( GetBin ) Axpr Axpr
let semactIf(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[2].value,rhs.[4].value, rhs.[5].value) with
    | (Binop(s,a,b), axpr1,axpr2) -> Ifelse(Binop(s,a,b), axpr1, axpr2)
    | _ -> Error

// Expr --> while ( GetBin ) ( BeginSeq )
let semactWhl(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[2].value, rhs.[5].value) with
    | (Binop(s,a,b),Sequence(c)) -> Whileloop(Binop(s,a,b), Sequence(c))
    | _ -> 
      printfn "You imbecile! While loops require a binary operation followed by a sequence of expressions!"
      Error

// Expr --> DEF Var Axpr
let semactDef(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[1].value, rhs.[2].value) with 
    | (TypedVar(ty,a), axpr) -> TypedDefine(rhs.[1].tolbox((ty,a)), axpr)
    | (Var(a), axpr) -> 
      printfn "Line %d Column %d: expected type for var '%s'" (rhs.[0].line) (rhs.[0].column) a
      Define(rhs.[1].tolbox(a), axpr)
    //| (Var(a), axpr) -> Define(rhs.[1].tolbox(a), axpr)
    | _ -> Error

// Expr --> lambda ( SeqVar ) typeopt Axpr
let semactLam(rhs:Vec<Stackitem<expr>>) = 
  printfn "Seq: %A" (rhs.[2].value)
  match (rhs.[2].value, rhs.[4].value, rhs.[5]) with
    | (Sequence(a), TypeExpr(ty), axpr) -> 
      TypedLambda(a, ty, axpr)
    | _ -> Error

// Expr --> setq Var Axpr
let semactSetQ(rhs:Vec<Stackitem<expr>>) = 
  match rhs.[1].value, rhs.[2].value with
    | Var(a), b -> Setq(rhs.[1].tolbox(a),b)
    | TypedVar(ty,a), b -> 
      printfn "Line %d, Column %d: Cannot retype var '%s'" (rhs.[1].line) (rhs.[1].column) a
      Error
    //| TypedVar(ty,a), b -> Setq(rhs.[2].tolbox(a),b)
    | _ -> Error

// Expr --> LET ( Var Axpr ) Axpr
let semactLet(rhs:Vec<Stackitem<expr>>) = 
  match rhs.[2].value, rhs.[3].value, rhs.[5].value with
    //| Var(a), b, c -> Let(rhs.[2].tolbox(a), b, rhs.[5].tolbox(c))
    | Var(a), b, c -> 
      printfn "Line %d, Column %d: Expected type for var '%s'" (rhs.[2].line) (rhs.[2].column) a
      Error
    | TypedVar(ty,a), b, c -> TypedLet(rhs.[2].tolbox((ty,a)),b,rhs.[5].tolbox(c))
    | _ -> Error

let semactNil(rhs:Vec<Stackitem<expr>>) = // VarOpt -->
  Sequence([])

// SeqExpr --> Axpr SeqOpt
let semactList(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[0], rhs.[1].value) with
    | (a,Sequence(b)) -> Sequence(a::b)
    | _ -> Error

// SeqVar --> Var VarOpt 
let semactParam(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[0].value, rhs.[1].value) with
    //| (Var(a), Sequence(b)) -> Sequence(rhs.[0].tolbox(TypedVar(LLunknown,a))::b)
    | (Var(a), Sequence(b)) -> 
      printfn "Line %d, Column %d: Expected type for '%s' parameter declaration" (rhs.[0].line)(rhs.[0].column) a
      Error
    | (TypedVar(ty,a), Sequence(b)) -> Sequence(rhs.[0].tolbox(TypedVar(ty,a))::b)
    | _ -> Error

// declare x:int lambda (x:float y:string z:int)
// Expr --> declare var COLON Txpr lambda ( SeqVar )
(*let semactDec(rhs:Vec<Stackitem<expr>>) =
  match (rhs.[1].value, rhs.[3].value, rhs.[6].value) with
    | Var(s), TypeExpr(ty), Sequence(a) -> Declare(rhs.[1].tolbox((ty,s)),a)
    | _ -> 
      printfn "Line %d: Improper Syntax for 'declare' Expected something like 'declare var:type lambda (var list)'" rhs.[0].line
      Error *)

// Expr --> vec_setq Var integer Axpr
let semactVSetQ(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[1].value,rhs.[2].value,rhs.[3].value) with
    | Var(s), Integer(i), a -> SetVQ(rhs.[1].tolbox(s),i, a)
    | TypedVar(ty,s), Integer(i), a -> 
      printfn "Line %d, Column %d: Cannot retype var '%s'" (rhs.[1].line) (rhs.[1].column) s
      Error
    | _ -> 
      printfn "Line %d: 'vec_setq' expected variable name, index value, and expression" (rhs.[1].line)
      Error

// Expr --> vec_get Var integer 
let semactVGet(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[1].value, rhs.[2].value) with
    | Var(s), Integer(i) -> VGet(rhs.[1].tolbox(s),i)
    | TypedVar(ty,s), Integer(i) -> 
      printfn "Line %d, Column %d: Cannot retype var '%s'" (rhs.[1].line) (rhs.[1].column) s
      Error
    | _ -> 
      printfn "Line %d: 'vec_setq' expected variable name and index value" (rhs.[1].line)
      Error

// Expr --> vec_make integer integer 
let semactMake(rhs:Vec<Stackitem<expr>>) =
  match (rhs.[1].value, rhs.[2].value) with 
    | Integer(i), Integer(j) -> VMake(rhs.[1].tolbox((i,j)))
    | _ ->
      printfn "Line %d: 'vec_make' expexted two integer values" (rhs.[1].line)
      Error

// Grammar
let G = new_grammar<expr>("Program") 

G.terminals([
"(";")";
"+";"-";"*";">";"=";"<";">=";"<=";"%";"^";"cons";"neq";"eq";"and";"or";
"~";"car";"cdr";"not";"display";
"if";"while";
"define";"setq";"let";"lambda";"begin";"declare";
"float";"int";"string";
"vec_setq";"vec_get";"vec_make";])

G.lexterminal("DIV","/")
G.lexterminal("DEC","declare")
G.lexterminal("DEF","define")
G.lexterminal("COLON",":")
G.lexterminal("LBRACE","[")
G.lexterminal("RBRACE","]")

G.lexterminal("LET","let")
G.lexterminal("NEG","~")
G.valueterminal("integer","Num", fun n -> Integer(int n))
G.valueterminal("flt","Float", fun n -> Floatpt(float n))
G.valueterminal("var","Alphanum", fun n -> Var(n))
G.valueterminal("str","StrLit", fun n -> Strlit(string n))

// took out P (Liang)
G.nonterminals(["Program";"POpt";"Expr";"Axpr";"Binop";"Uniop";"SeqExpr";"SeqOpt";"BeginSeq";"SeqVar";"Txpr";"typeopt";"Lam";"Var";"VarOpt";"GetBin"]);

// Program Start
G.production("POpt --> Program", fun n -> n.[0].value)
G.production("POpt -->",semactNil)

// Expressions
G.production("Expr --> GetBin",fun n -> n.[0].value)
G.production("Expr --> Uniop Axpr",semactUni)
G.production("Expr --> if ( GetBin ) Axpr Axpr",semactIf)
G.production("Expr --> while ( GetBin ) ( BeginSeq )",semactWhl) 
//G.production("Expr --> DEC Var lambda ( SeqVar )",semactDec)
G.production("Expr --> DEF Var Axpr",semactDef)
G.production("Expr --> setq Var Axpr",semactSetQ)
G.production("Expr --> BeginSeq",fun n -> n.[0].value)
G.production("Expr --> vec_setq Var integer Axpr", semactVSetQ)
G.production("Expr --> vec_get Var integer", semactVGet)
G.production("Expr --> vec_make integer integer", semactMake)
// declare log2 = int lambda n:int;
// declare Var txpr lambda (SeqVar)

/// new productions added by Liang
G.production("Axpr --> ( Expr )",fun n -> n.[1].value)
G.production("Expr --> SeqExpr", fun n -> n.[0].value)
G.production("Program --> Axpr POpt", semactList)
G.production("Expr --> LET ( Var Axpr ) Axpr",semactLet)
// need to handle Typedval for TypedLambda; Typedval = lltype*expr = typeopt Axpr
G.production("Expr --> lambda ( SeqVar ) typeopt Axpr",semactLam)

// Bad Productions
//G.production("Program --> P POpt", semactList)
//G.production("P --> ( Expr )",fun r -> r.[1].value)
//G.production("Expr --> LET ( Var Axpr ) P",semactLet) //taken out by Liang
//G.production("Expr --> lambda ( SeqVar ) typeopt P",semactLam) //Liang
//G.production("Expr --> Axpr",fun n -> n.[0].value)  //Liang (OMG!)
//G.production("Axpr --> P",fun n -> n.[0].value)  //taken out by Liang


// Begin Sequence
G.production("BeginSeq --> begin SeqExpr", fun n -> n.[1].value) 

// Var
G.production("Var --> var typeopt", semactVar) 
G.production("typeopt --> COLON Txpr", fun n -> n.[1].value) 
G.production("typeopt -->", fun n -> TypeExpr(LLunknown)) 

// Types
G.production("Txpr --> float", fun n -> TypeExpr(LLfloat))
G.production("Txpr --> int", fun n -> TypeExpr(LLint))
G.production("Txpr --> string", fun n -> TypeExpr(LLstring))
//G.production("Txpr --> LBRACE RBRACE", fun n -> TypeExpr(LLvec))

// Function Parameters (Variable sequences) // same style as SeqExpr
G.production("SeqVar --> Var VarOpt", semactParam)
G.production("VarOpt --> SeqVar", fun n -> n.[0].value)
G.production("VarOpt -->",semactNil) // Return Sequence([])

// Expression Sequences
//As --> Axpr Asopt
//Asopt --> epsilon | As
G.production("SeqExpr --> Axpr SeqOpt", semactList)
G.production("SeqOpt --> SeqExpr",fun n -> n.[0].value)
G.production("SeqOpt -->",semactNil)

// Atomic Expressions, (int, var, string, etc.)
G.production("Axpr --> Var",fun n -> n.[0].value) 
G.production("Axpr --> integer", fun n -> n.[0].value) 
G.production("Axpr --> str",fun n -> n.[0].value)  
G.production("Axpr --> flt",fun n -> n.[0].value)

// Binary Operators
G.production("GetBin --> Binop Axpr Axpr", semactBin)
G.production("Binop --> +",semactLab)
G.production("Binop --> -",semactLab)  
G.production("Binop --> *",semactLab)  
G.production("Binop --> DIV",semactLab)  
G.production("Binop --> >",semactLab)  
G.production("Binop --> <",semactLab)  
G.production("Binop --> =",semactLab)  
G.production("Binop --> >=",semactLab)  
G.production("Binop --> %",semactLab) 
G.production("Binop --> neq",semactLab) 
G.production("Binop --> eq",semactLab) 
G.production("Binop --> and",semactLab) 
G.production("Binop --> or",semactLab) 
G.production("Binop --> ^",semactLab) // xor

// Uniary Operators
G.production("Uniop --> NEG",semactLab) // ~
G.production("Uniop --> car",semactLab)
G.production("Uniop --> cdr",semactLab)
G.production("Uniop --> not",semactLab)
G.production("Uniop --> display",semactLab)

// END Grammar

// main

let parse(trace:bool) =
  let mutable TRACE = trace
  if TRACE then G.printgrammar()
  printf "Enter Expression: "
  let lexer1 = Fussless.schemerlexer(Console.ReadLine()); // compile with schemer_lex.dll
  let parser1 = make_parser<expr>(G, lexer1, TRACE);

  let result = parser1.parse(trace)
  if not (parser1.errors) then 
    printfn "result = %A" result
    Some(result)
  else
    printfn "FAILED TO PARSE" 
    None
