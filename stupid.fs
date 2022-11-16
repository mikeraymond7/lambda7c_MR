module Recless.Base
open System;
open System.Collections.Generic;

// type aliases
type Vec<'A> = ResizeArray<'A>
type Conslist<'A> = 'A list   // not used much
type HashMap<'A,'B> = Dictionary<'A,'B>

let mutable TRACE = true  // global tracing flag

// LL(1) parser generator
// grammar symbols are just strings

// type of item on the valuestack (parse stack)
type Stackitem<'AT> =
  {
    symbol: string; // should correspond to a grammar symbol
    value: 'AT;   // semantic value
    line: int;   // lexical position
    column: int;
  }
  member this.tolbox<'BT>(v:'BT) = { Stackitem.symbol=this.symbol; value=v; line=this.line; column=this.column; }

type LBox<'AT> = Stackitem<'AT>

let new_stackitem<'T>(s,v:'T,l,c) =
  {Stackitem.symbol=s; value=v; line=l; column=c;}


// This is called an "active pattern": allows to look at just value with
// pattern matching: match t with | Item(v) -> ...
let (|Item|) (t:Stackitem<'AT>) = Item(t.value)


//////////////////// Grammar Representation

type Production<'T> =    // production rule
  {
    lhs: string
    rhs: Vec<string>;
    mutable action: Vec<Stackitem<'T>> -> 'T;
  }

type Grammar<'T> =
  {
    Symbols : HashSet<string>;
    Nonterminals : HashSet<string>;
    mutable startsymbol : string;
    Productions : Vec<Production<'T>>;
    //Rulesfor : HashMap<string,HashSet<int>>;
    Follow: HashMap<string,HashSet<string>>;
    First : HashMap<string,HashSet<string>>;
    Nullable : HashSet<string>;
    LL1Table : HashMap<string,HashMap<string,int>>;
    valueterminals : HashMap<string,string*(string -> 'T)>;
    lexterminals: HashMap<string,string>; // maps ":" to "COLON"
  }
  // impl Grammar:
  member this.terminal (s:string) =
    this.Symbols.Contains(s) && not(this.Nonterminals.Contains(s))
  member this.nonterminal (s:string) = this.Nonterminals.Contains(s)

  // function to add valueterminal to grammar, with function to convert
  member this.valueterminal(tname:string,tokenname:string,conv) =
    if not(this.Nonterminals.Contains(tname)) then
      this.Symbols.Add(tname) |> ignore
      this.valueterminals.[tokenname] <- (tname,conv)
    else
      printfn "CONFLICTING DEFINITION OF SYMBOL %s AS NONTERMINAL" tname

  member this.lexterminal(tname:string, tokenform:string) =
    if not(this.Nonterminals.Contains(tname)) then
      this.Symbols.Add(tname) |> ignore
      this.lexterminals.[tokenform] <- tname
    else
      printfn "CONFLICTING DEFINITION OF SYMBOL %s IGNORED" tname

  member this.terminals(tnames:string list) =
    for tname in tnames do 
      if not(this.Nonterminals.Contains(tname)) then
        this.Symbols.Add(tname) |> ignore
      else
        printfn "CONFLICTING DEFINITION OF SYMBOL %s IGNORED" tname

  member this.nonterminals(tnames:string list) =
    for tname in tnames do 
      if not(this.Symbols.Contains(tname) && not(this.Nonterminals.Contains(tname))) then
        this.Symbols.Add(tname) |> ignore
        this.Nonterminals.Add(tname) |> ignore
      else
        printfn "CONFLICTING DEFINITION OF SYMBOL %s IGNORED" tname

  // call like:
  // grammar1.production("E --> T E1", fun V -> ...)

  member this.production(rawrule:string, action) =
    let ruleform = rawrule.Split([|"-->"; ":"; " ";|],StringSplitOptions.RemoveEmptyEntries)
    if ruleform.Length>0 then
      if not(this.Nonterminals.Contains(ruleform.[0])) then
        raise(Exception(sprintf "%s is not recognized as a non-terminal symbol" (ruleform.[0])))
      let lhs = ruleform.[0] 
      let rhs = Vec<string>();
      for i in 1..ruleform.Length-1 do
        if not(this.Symbols.Contains(ruleform.[i])) then
          raise(Exception(sprintf "Symbol %s is not recognized" (ruleform.[i])))
        rhs.Add(ruleform.[i])
      let rule = {Production.lhs=lhs; rhs=rhs; action=action;}
      this.Productions.Add(rule) |> ignore
      //this.Productions.Count-1   // returns the index of the rule added
    else
      printfn "Malformed production rule rejected"
  //this.production

  // add a production with default semantic action
  member this.production_default(s) =
    this.production(s,fun v -> Unchecked.defaultof<'T>)

  member this.set_action(ri, action) =
    this.Productions.[ri].action <- action
  
  member this.set_start(s:string) =
    if not(this.Nonterminals.Contains(s)) then
      printfn "The start symbol must be declared as a non-terminal symbol"
    else
      this.startsymbol <-s

  member this.printrule(ri) =
    let rule = this.Productions.[ri]
    printf "(%d) %s --> " ri rule.lhs
    for sym in rule.rhs do printf "%s " sym
    printfn ""
  member this.printgrammar() =
    for i in 0..this.Productions.Count-1 do this.printrule(i)
    

  
  ///// LL parser setup: Nullable, First and Follow closures:
  member this.find_nullables() =
    let mutable progress = true
    while progress do
      progress <- false
      for rule in this.Productions do
        let mutable rhsnullable = true
        let mutable i = 0;
        // if all rhs are nullable, then the entire Nonterminal is nullable
        while i<rule.rhs.Count && rhsnullable do
          if not(this.Nullable.Contains(rule.rhs.[i])) then
            rhsnullable <- false // give me a break!
          i<-i+1
        // for each symbol on rhs of rule
        if rhsnullable then progress<-this.Nullable.Add(rule.lhs)||progress
      //for each rule
    //while progress
  //nullable

  member this.nullableseq (seq:Vec<string>, starti) =
    let mutable ax =true
    let mutable i = starti
    while i<seq.Count && ax do
      if not(this.Nullable.Contains(seq.[i])) then ax<-false
      i <- i+1
    ax

  member this.find_first() =
    let mutable progress = true
    while progress do
      progress <- false
      // find first set of rule.lhs using each rhs
      for rule in this.Productions do
        // get current HashSet
        if not(this.First.ContainsKey(rule.lhs)) then 
          this.First.Add(rule.lhs, HashSet<string>())
        let first:HashSet<string> = this.First.[(rule.lhs)]
 
        // add symbols to set
        let mutable i = 0
        let mutable cont = true
        let mutable fSym = ""
        while (i < rule.rhs.Count && cont) do
          cont <- false
          fSym <- rule.rhs.[i]
          if this.First.ContainsKey(fSym) then
            for sym in this.First.[fSym] do 
              progress <- first.Add(sym) || progress
 
          // if nullable, add next symbol's First set or terminal symbol
          if this.Nullable.Contains(fSym) then 
            cont <- true
          i <- i + 1
        // while
 
        if rule.rhs.Count > 0 && not(this.Nonterminals.Contains(fSym)) then 
          progress <- first.Add(fSym) || progress
      // for
    // while
  // first

  member this.firstseq (seq:Vec<string>, starti) = // stub only
    let setax = new HashSet<string>()
    let mutable cont = true
    let mutable i = starti
    while i < seq.Count && cont do
      let sym = seq.[i]
      cont <- this.Nullable.Contains(sym)
      if this.Nonterminals.Contains(sym) then
        for fSym in this.First.[sym] do
          setax.Add(fSym) |> ignore
      else setax.Add(sym) |> ignore
      i <- i + 1
    // while
    setax 
  // firstseq

  member this.find_follow() =  // stub only
    let mutable progress = true
    while progress do
      progress <- false

      for rule in this.Productions do
        for i = 0 to rule.rhs.Count-1 do
          let sym = rule.rhs.[i]
          if this.Nonterminals.Contains(sym) then
            if not(this.Follow.ContainsKey(sym)) then 
              this.Follow.Add(sym, HashSet<string>())
            let follow:HashSet<string> = this.Follow.[sym]
            for fSym in this.firstseq(rule.rhs,i+1) do 
              progress <- follow.Add(fSym) || progress
            if this.nullableseq(rule.rhs,i+1) && this.Follow.ContainsKey(rule.lhs) then 
              for fSym in this.Follow.[rule.lhs] do
                progress <- follow.Add(fSym) || progress
        // for
      // for
    // while
  //follow

  member this.make_table() =
    for nt in this.Nonterminals do
      this.LL1Table.Add(nt,HashMap<string,int>())

    // Must be free from left recursion
    // Cannot be ambiguous
    // Must be left-factored and deterministic
    // for each Production
    
    for i = 1 to this.Productions.Count - 1 do // 0th production is METASTART
      
      let rule = this.Productions.[i]
      let tableRow:HashMap<string, int> = this.LL1Table.[rule.lhs]
      let first:HashSet<string> = this.firstseq(rule.rhs, 0)
      for sym in first do
        tableRow.Add(sym, i)
      if this.nullableseq(rule.rhs, 0) then
        for sym in this.Follow.[rule.lhs] do
          tableRow.Add(sym, i)
  //create table


//////// end of impl Grammar

let new_grammar<'AT>(start:string) =
  let gmr = 
    { 
      Symbols = HashSet<string>();
      Grammar.Nonterminals = HashSet<string>();      
      startsymbol = start;
      Productions = Vec<Production<'AT>>();
      Follow = HashMap<string,HashSet<string>>();
      First = HashMap<string,HashSet<string>>();
      Nullable = HashSet<string>();
      LL1Table = HashMap<string,HashMap<string,int>>();
      valueterminals = HashMap<string,string*(string -> 'AT)>();
      lexterminals = HashMap<string,string>();
    }
  gmr.Nonterminals.Add(start) |> ignore
  gmr.Symbols.Add(start) |> ignore
  gmr.Symbols.Add("EOF") |> ignore
  gmr.Symbols.Add("METASTART") |> ignore
  gmr.Nonterminals.Add("METASTART") |> ignore
  gmr.production(sprintf "METASTART --> %s EOF" start, fun v->v.[0].value)
  gmr
//new_grammar with start symbol


/////////////////////// RUNTIME PARSER
  
type LLparser<'T> =
  {
    Gmr: Grammar<'T>;
    parsestack : Stack<string>;
    rulestack : Stack<int*int>; // rule number and position on VALUESTACK
    valuestack : Stack<Stackitem<'T>>;
    mutable errors : bool;  // determines if errors occurred
    lexer : Fussless.AbstractLexer<unit>; //returns RawTokens
  }
  //  member this.convert_token (rt:Fussless.RawToken) =
  //    new_stackitem("dummy",Unchecked.defaultof<'T>,0,0)

  member this.next_la() =  // get next input symbol, place on stack
    let rt = this.lexer.next_lt()  //RawToken
    // check valueterminal
    if this.Gmr.valueterminals.ContainsKey(rt.token_name) then
      let (tname,f) = this.Gmr.valueterminals.[rt.token_name]
      let v = f(rt.token_text)
      new_stackitem(tname,v,rt.line,rt.column)
    else if this.Gmr.lexterminals.ContainsKey(rt.token_name) then
      let tname = this.Gmr.lexterminals.[rt.token_name]
      new_stackitem(tname,Unchecked.defaultof<'T>,rt.line,rt.column)
    else
      new_stackitem(rt.token_name,Unchecked.defaultof<'T>,rt.line,rt.column)

  /////
  // parsing is done in two phases.  First phase is the pure, top-down parsing
  // phase. Upon success, the second phase goes bottom up, applying
  // semantic action functions and synthesizing semantic values.  The semantic
  // actions must be pure functions (stateless).  One possible enhancement
  // is to attach two semantic actions to each production: one to be applied
  // in the forward stage and can have side-effects.
  /////
  member this.parse_top_down() =
    this.parsestack.Push("EOF");
    this.parsestack.Push(this.Gmr.startsymbol);
    this.rulestack.Push(0,0); // 0 is always meta-start rule, position 0
    // push startrule number (0) on rulestack
    let mutable stop = false
    let mutable lookahead = this.next_la()
    while not(stop) do
      let next = this.parsestack.Pop(); // expected symbol, no value
      if this.Gmr.Nonterminals.Contains(next) then // nonterminal
        let row = this.Gmr.LL1Table.[next]
        if row.ContainsKey(lookahead.symbol) then
          let ri = row.[lookahead.symbol]
          this.rulestack.Push(ri,this.valuestack.Count)
          if TRACE then
              try (printf "lookahead %A, rule " lookahead.value) with | _ -> 
                 printf "lookahead %s, rule " lookahead.symbol
              this.Gmr.printrule(ri)
          // push rhs of rule ri on stack
          let rule = this.Gmr.Productions.[ri]
          let mutable i = rule.rhs.Count-1
          while i>=0 do
            this.parsestack.Push(rule.rhs.[i])
            i<-i-1
          //while
        else
          printfn "PARSE ERROR line %d column %d, UNEXPECTED SYMBOL %s" (lookahead.line) (lookahead.column) (lookahead.symbol)
          this.errors<-true
      else if this.Gmr.terminal(next) then
        if next=lookahead.symbol then
          this.valuestack.Push(lookahead);
          if TRACE then
            printfn "  pushed %s on valuestack " lookahead.symbol
          if lookahead.symbol="EOF" then stop <- true
          else lookahead <- this.next_la()
        else
          printfn "PARSE ERROR line %d column %d, EXPECTING %s BUT GOT %s" (lookahead.line) (lookahead.column) next (lookahead.symbol)
          this.errors<-true
    // parse loop
  // parse_top_down

  member this.compose_bottom_up() =
    let mutable line = 0
    let mutable column = 0
    let bustack = Stack<Stackitem<'T>>(); // new stack to push onto
    while this.rulestack.Count > 0 do
      let (ri,vsi) = this.rulestack.Pop()
      let rule = this.Gmr.Productions.[ri]
      // shove valuestack to bustack, form arg for sem action:
      let mutable i = this.valuestack.Count-1
      while i>=vsi do
        let last = this.valuestack.Pop()
        bustack.Push(last)
        i <-i-1
      //shove

      let semargs = Vec<Stackitem<'T>>();
      for _ in 1..rule.rhs.Count do
         let nextitem = bustack.Pop()
         if line=0 && column=0 then
            line <- nextitem.line
            column<-nextitem.column
         semargs.Add(nextitem)
         if TRACE then
             printf "   popped values "
             for arg in semargs do
               try (printf "%A, " arg.value) with | _ -> printf "%s(null), " arg.symbol
             printfn ""
      let newval = (rule.action semargs)
      bustack.Push(new_stackitem(rule.lhs,newval,line,column))
      if TRACE then
        printfn "pushed %A value for %s" newval rule.lhs
    //while stack not empty
    if bustack.Count=1 then
      this.valuestack.Clear()
      this.valuestack.Push(bustack.Pop())
    else
      printfn "PARSING FAILED: %d values left on stack" bustack.Count
      while TRACE && this.valuestack.Count>0 do
        printfn "stack item %A" (bustack.Pop())
  //compose_bottom_up

  member this.parse() =
    this.parse_top_down()
    if TRACE then printfn "top-down phase complete"
    if not(this.errors) then
      this.compose_bottom_up()
      if this.valuestack.Count=1 then 
        this.valuestack.Pop().value
      else
        Unchecked.defaultof<'T>
    else
      Unchecked.defaultof<'T>        
  //parse      

  /////////// end of impl LLparser

let make_parser<'AT>(gmr:Grammar<'AT>,lexer) =
  gmr.find_nullables()
  if TRACE then
    for n in gmr.Nullable do printf "Nullable %s, " n
    printfn ""
  gmr.find_first()
  if TRACE then
    for kvp in gmr.First do
      printf "First(%s)= " kvp.Key
      for s in kvp.Value do printf "%s, " s
      printfn ""
  gmr.find_follow()
  if TRACE then
    for kvp in gmr.Follow do
      printf "Follow(%s)= " kvp.Key
      for s in kvp.Value do printf "%s, " s
      printfn ""  
  gmr.make_table()
  {
    LLparser.Gmr = gmr;
    parsestack = Stack<string>();
    rulestack = Stack<int*int>();
    valuestack = Stack<Stackitem<'AT>>();
    errors = false;
    lexer = lexer;
  }
// make_parser


/////////////////////////////////////////////////////////////////////
////////////////// testing with a sample grammar and action functions

//TRACE <- true


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
  | TypedDefine of LBox<lltype*string>*expr
  | Lambda of (string list)*expr
  | TypedLambda of (LBox<expr> list)*lltype*LBox<expr>
  | Let of LBox<string>*expr*LBox<expr>
  | TypedLet of LBox<lltype*string>*expr*LBox<expr>
  | Quote of expr
  | Setq of LBox<string>*expr
  | Sequence of LBox<expr> list
  | Beginseq of LBox<expr> list
  | TypeExpr of lltype
  | Typedval of (lltype*expr)
  | Label of string
  | Error

and lltype = 
  | LLint | LLfloat | LLstring 
  | LList of lltype | LLtuple of lltype list
  | LLfun of (lltype list)*lltype
  | LLunknown | LLuntypable | LLcar of string | LLunit

let semactLab (rhs:Vec<Stackitem<expr>>) = // Binop | Uniop -> Label
  Label(rhs.[0].symbol)

let semactUni (rhs:Vec<Stackitem<expr>>) = // Expr --> Uniop Axpr
  match(rhs.[0].value, rhs.[1].value) with
    | (Label(z), a) -> Uniop(z, a)
    | _ -> Error

let semactBin (rhs:Vec<Stackitem<expr>>) = // Expr --> Binop Axpr Axpr
     // Lbox is alias for Stackitem... to use lbox, don't use ".value"
  match(rhs.[0].value, rhs.[1].value, rhs.[2].value) with
    | (Label(z), a, b) -> Binop(z, a, b)
    | _ -> Error

let semactVar(rhs:Vec<Stackitem<expr>>) = // Var --> var typeopt
  if rhs.Count = 2 then
    match (rhs.[0].value, rhs.[1].value) with
      | Var(x), TypeExpr(LLunknown) -> Var(x)
      | Var(x), TypeExpr(ty) -> TypedVar(ty,x)
      | _ -> Error
  elif rhs.Count = 1 then
    match (rhs.[0].value) with
      | Var(x) -> Var(x)
      | _ -> Error
  else Error

let semactIf(rhs:Vec<Stackitem<expr>>) = // Expr --> if Binop Axpr Axpr
  match (rhs.[2].value,rhs.[4].value, rhs.[5].value) with
    | (Binop(s,a,b), axpr1,axpr2) -> Ifelse(Binop(s,a,b), axpr1, axpr2)
    | _ -> Error

let semactWhl(rhs:Vec<Stackitem<expr>>) = // Expr --> while ( Binop ) ( BeginSeq )
  match (rhs.[2].value, rhs.[5].value) with
    | (Binop(s,a,b),Sequence(c)) -> Whileloop(Binop(s,a,b), Sequence(c))
    | _ -> 
      printfn "You imbecile! While loops require a binary operation followed by a sequence of expressions!"
      Error

let semactDef(rhs:Vec<Stackitem<expr>>) = // Expr --> DEF Var Axpr
  match (rhs.[1].value, rhs.[2].value) with 
    | (TypedVar(ty,a), axpr) -> TypedDefine(rhs.[1].tolbox((ty,a)), axpr)
    | (Var(a), axpr) -> Define(rhs.[1].tolbox(a), axpr)
    | _ -> Error

let semactLam(rhs:Vec<Stackitem<expr>>) = // Expr --> lambda ( SeqVar ) typeopt P
  printfn "Seq: %A" (rhs.[2].value)
  match (rhs.[2].value, rhs.[4].value, rhs.[5]) with
    | (Sequence(a), TypeExpr(ty), axpr) -> 
      TypedLambda(a, ty, axpr)
    | _ -> Error

let semactSetQ(rhs:Vec<Stackitem<expr>>) = // Expr --> setq Var Axpr
  match rhs.[1].value, rhs.[2].value with
    | Var(a), b -> Setq(rhs.[1].tolbox(a),b)
    | TypedVar(ty,a), b -> Setq(rhs.[2].tolbox(a),b)
    | _ -> Error

// Expr --> LET ( Var Axpr ) P
let semactLet(rhs:Vec<Stackitem<expr>>) = 
  match rhs.[2].value, rhs.[3].value, rhs.[5].value with
    | Var(a), b, c -> Let(rhs.[2].tolbox(a), b, rhs.[5].tolbox(c))
    | TypedVar(ty,a), b, c -> TypedLet(rhs.[2].tolbox((ty,a)),b,rhs.[5].tolbox(c))
    | _ -> Error

let semactNil(rhs:Vec<Stackitem<expr>>) = // VarOpt -->
  Sequence([])

// SeqVar --> Var VarOpt 
// SeqExpr --> Axpr SeqOpt
let semactList(rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[0], rhs.[1].value) with
    | (a,Sequence(b)) -> Sequence(a::b)
    | _ -> Error

let G = new_grammar<expr>("Program") 

G.terminals([
"(";")";
"+";"-";"*";">";"=";"<";">=";"<=";"%";"^";"cons";"neq";"eq";"and";"or";
"~";"car";"cdr";"not";"display";
"if";"while";
"define";"setq";"let";"lambda";"begin";
"float";"int";"string"])

G.lexterminal("DIV","/")
G.lexterminal("COLON",":")
G.lexterminal("DEF","define")
G.lexterminal("LET","let")
G.lexterminal("NEG","~")
G.valueterminal("integer","Num", fun n -> Integer(int n))
G.valueterminal("flt","Float", fun n -> Floatpt(float n))
G.valueterminal("var","Alphanum", fun n -> Var(n))
G.valueterminal("str","StrLit", fun n -> Strlit(string n))

G.nonterminals(["Program";"P";"POpt";"Expr";"Axpr";"Binop";"Uniop";"SeqExpr";"SeqOpt";"BeginSeq";"SeqVar";"Txpr";"typeopt";"Lam";"Var";"VarOpt";"GetBin"]);

// Program Start
G.production("Program --> P POpt", semactList)
G.production("POpt --> Program", fun n -> n.[0].value)
G.production("POpt -->",semactNil)

G.production("P --> ( Expr )",fun r -> r.[1].value)

// Expressions
G.production("Expr --> GetBin",fun n -> n.[0].value)
G.production("Expr --> Uniop Axpr",semactUni)
G.production("Expr --> if ( GetBin ) Axpr Axpr",semactIf)
G.production("Expr --> while ( GetBin ) ( BeginSeq )",semactWhl) 
G.production("Expr --> DEF Var Axpr",semactDef) // Return Define(LBox(Var), Lam | Axpr)
G.production("Expr --> setq Var Axpr",semactSetQ)
G.production("Expr --> LET ( Var Axpr ) P",semactLet)
// need to handle Typedval for TypedLambda
G.production("Expr --> lambda ( SeqVar ) typeopt P",semactLam)
G.production("Expr --> BeginSeq",fun n -> n.[0].value)
G.production("Expr --> Axpr",fun n -> n.[0].value) 
//G.production("Expr --> SeqExpr", fun n -> n.[0].value)

// Begin Sequence
G.production("BeginSeq --> begin SeqExpr", fun n -> n.[1].value) // move type from SeqExpr to BeginSeq

// Var
G.production("Var --> var typeopt", semactVar) // Returns Var or TypedVar
G.production("typeopt --> COLON Txpr", fun n -> n.[1].value) // Returns found Type
G.production("typeopt -->", fun n -> TypeExpr(LLunknown)) // Returns LLunknown

// Types
G.production("Txpr --> float", fun n -> TypeExpr(LLfloat))
G.production("Txpr --> int", fun n -> TypeExpr(LLint))
G.production("Txpr --> string", fun n -> TypeExpr(LLstring))

// Function Parameters (Variable sequences) // same style as SeqExpr
G.production("SeqVar --> Var VarOpt", semactList)
G.production("VarOpt --> SeqVar", fun n -> n.[0].value)
G.production("VarOpt -->",semactNil) // Return Sequence([])

// Expression Sequences
//As --> Axpr Asopt
//Asopt --> epsilon | As
G.production("SeqExpr --> Axpr SeqOpt", semactList)
G.production("SeqOpt --> SeqExpr",fun n -> n.[0].value)
G.production("SeqOpt -->",semactNil)

// Atomic Expressions, (int, var, string, etc.)
G.production("Axpr --> P",fun n -> n.[0].value) 
G.production("Axpr --> Var",semactVar) 
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
G.production("Binop --> ^",semactLab)

// Uniary Operators
G.production("Uniop --> NEG",semactLab)
G.production("Uniop --> car",semactLab)
G.production("Uniop --> cdr",semactLab)
G.production("Uniop --> not",semactLab)
G.production("Uniop --> display",semactLab)

if TRACE then G.printgrammar()

// main
printf "Enter Expression: "
let lexer1 = Fussless.schemerlexer(Console.ReadLine()); // compile with schemer_lex.dll
let parser1 = make_parser(G,lexer1);

let result = parser1.parse()
if not(parser1.errors) then printfn "result = %A" result

