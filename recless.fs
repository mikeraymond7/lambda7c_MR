module Recless.Base
open System;
open System.IO;
open System.Collections.Generic;
open Json

// This is an updated version of recless_skeleton. RENAME this file to
// recless.fs and compile to a .dll after you complete the Grammar.make_table
// function.

// It uses a Json parser built with Fussless and the same lexical scanner
// interface as Fussless

//fsharpc -a recless.fs -r absLexer.dll -r jsonparser.dll -r json_lex.dll

////////////////// json utilities

let rec json_dumps = function
  | jval.Integer(n) -> string(n)
  | Float(n) -> string(n)
  | Bool(n) -> string(n)
  | Null -> "null"
  | Str(s) -> "\"" + s + "\""
  | Seq(s) ->
     let mutable atleastone = false
     let mutable str = "["
     for v in s do
        if atleastone then str <- str + ", "
        atleastone <- true
        str <- str + (json_dumps v)
     str + "]"
  | Map(m) ->
     let mutable atleastone = false
     let mutable str = "{"
     for kvp in m do
        if atleastone then str <- str + ", "
        atleastone <- true
        str <- str + (sprintf "\"%s\":%s" kvp.Key (json_dumps kvp.Value))
     str + "}"

let getstr = function
  | jval.Str(s) -> s
  | jval.Null -> "null"
  | jval.Integer(n) -> string(n)
  | jval.Float(f) -> string(f)
  | jval.Bool(true) -> "true"
  | jval.Bool(false) -> "false"
  | x -> json_dumps x

let getint = function
  | jval.Integer(x) -> x
  | _ -> raise(Exception("not an int"))
  
let getseq = function
  | jval.Seq(s) -> s
  | _ -> raise(Exception("not a seq"))
let getmap = function
  | jval.Map(s) -> s
  | _ -> raise(Exception("not a map"))  

////////////////// json utilities


//////// type aliases
type Vec<'A> = ResizeArray<'A>
type Conslist<'A> = 'A list
type HashMap<'A,'B> = Dictionary<'A,'B>


// Used by both parser and AST to store lexical info.  The .tolbox member
// function takes a value of any type and places it in a Stackitem with
// the same lexical position information.
type Stackitem<'AT> =
  {
    symbol: string; // should correspond to a grammar symbol
    value: 'AT;   // semantic value
    line: int;   // lexical position
    column: int; //
  }
  member this.tolbox<'BT>(v:'BT) = { Stackitem.symbol=this.symbol; value=v; line=this.line; column=this.column; }

///// Alias for uniformity
type LBox<'AT> = Stackitem<'AT>  // type alias  

let new_stackitem<'T>(s,v:'T,l,c) =
  {Stackitem.symbol=s; value=v; line=l; column=c;}
let lbox = new_stackitem  // constructor
// active pattern to expose Stackitems as just values (like deref coercion
// in Rust).
let (|Item|) (t:Stackitem<'AT>) =  Item(t.value);
let (|Lbox|) (t:Stackitem<'AT>) =  Lbox(t.value);
// abstract syntax trees can be defined directly in terms of LBox/Stackitem


// LL(1) parser generator

let mutable TRACE = true;  // global tracing flag

(* Grammar production rule:
 Reach rule is loaded at runtime.  The semantic action function should
 have no side-effects outside of the context of the rule.  The arguments
 to each semantic action are the semantic values of each right-hand side
 symbol wrapped in an LBox/Stackitem.  For example, for rule E --> ( E )
 the semantic action takes a vector of three Stackitems, and the second
 item will correspond to the E on the right-hand side.  The action
 function must return a value (always of type 'T) that corresponds to
 what's expected for the left-hand side symbol.  The semantic action in this
 case should (most likely) be   fun rhs -> rhs.[1].value
*)
type Production<'T> =
  {
    lhs: string       // left-hand side
    rhs: Vec<string>; // right-hand side
    mutable action: Vec<Stackitem<'T>> -> 'T;  // semantic action
  }

(*  Grammar representation.
    A grammar defines a set of recognized symbols and a set of non-terminals.
    All grammar symbols are represented as strings.  One particular symbol
    is designated as the start symbol although the parser generator will
    also create a METASTART symbol and a EOF symbol.  The grammar contains
    a vector of productions, so productions are identified by indices.
    Two hashmap define how to interpret lexical tokens as terminal symbols.
    The other structures in the grammar: Nullable, First, Follow and LL1Table
    are for the purpose of generating a parser.

    The methods (member functions) of a grammar are divided into 2 categories:
    those that can be called by the user to set up a parser, and those that
    are only called by the parser generation code.  The methods that are
    intended to be called are:

    this.terminals: specifies a list of terminals symbols that are the same
                    as their textual form
    this.lexterminal: specifies a terminal symbol that's different from its
                    textual form
    this.valueterminal: specifies terminal symbols with values by naming the
                    token category and a function to convert text to values

    this.set_start: sets the start symbol.  This is rarely called: typically
                    the stand-alone function 'new_grammar' is called to
                    create a skeleton grammar with a given start symbol

    this.production: adds a production rule in string such as "E --> T E1" 
                    to a grammar along with its semantic action function.
                    This should only be called after terminals/nonterminals
                    have already been defined.
                    
    this.production_default: adds a production with a default action

    this.set_action: sets/changes the action of a rule named by index.
                     The first rule added has index 1.
    this.printrule/this.printgrammar: self-explanatory.

    Consult the sample llcalc_ast.fs for suggested workflow in terms of
    how and when to call these functions.

    The other methods of Grammar should NEVER be called by the user: they
    are called by the parser generation code.
*)

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

  member this.production_default(s) =
    this.production(s,fun v -> Unchecked.defaultof<'T>)

  member this.set_action(ri, action) =
    this.Productions.[ri].action <- action

(*
  member this.load_actions acts:Vec<'a> = //(acts:Vec<Vec<Stackitem<'T>> -> 'T>) = 
    if acts.Count = this.Productions.Count then
      for i in 0..acts.Count-1 do this.Productions.[i].action = acts.[i]
    else printfn "Number of actions must be equal to the number of production rules"
*)
  
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


  //////  The following functions should never be called by the user ***
  
  ///// LL parser setup: Nullable, First and Follow closures, make_table:
  ////  These functions are called ONCE by the LLparser generator
  member this.find_nullables() =
    let mutable progress = true
    while progress do
      progress <- false
      for rule in this.Productions do
        let mutable rhsnullable = true
        let mutable i = 0;
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

  // stubs of functions that must be completed for parser generation

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


  member this.firstseq (seq:Vec<string>, starti) =
    let setax = HashSet<string>()
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

  member this.find_follow() = 
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

    for i = 1 to this.Productions.Count - 1 do // 0th production is METASTART
      
      let rule = this.Productions.[i]
      let tableRow:HashMap<string, int> = this.LL1Table.[rule.lhs]
      let first:HashSet<string> = this.firstseq(rule.rhs, 0)

      for sym in first do
        if tableRow.ContainsKey(sym) then
          let conRule = tableRow.[sym]
          let conProd = this.Productions.[conRule]
          printfn "\nGRAMMAR CONFLICT:\nConflict with rule %d and rule %d with KEY '%s'" i (tableRow.[sym]) sym
          printfn "rule %d ==== '%s' --> %A" i (rule.lhs) (rule.rhs)
          printfn "rule %d ==== '%s' --> %A" (conRule) (conProd.lhs) (conProd.rhs)
          printfn "FROM firstseq of rule %d ==== '%s' --> %A : %A" i (rule.lhs) (rule.rhs) first
        else
          tableRow.Add(sym, i)
      if this.nullableseq(rule.rhs, 0) then
        for sym in this.Follow.[rule.lhs] do
          if tableRow.ContainsKey(sym) then
            let conRule = tableRow.[sym]
            let conProd = this.Productions.[conRule]
            printfn "\nConflict with rule %d and rule %d with KEY '%s'" i (tableRow.[sym]) sym
            printfn "rule %d ==== '%s' --> %A" i (rule.lhs) (rule.rhs)
            printfn "rule %d ==== '%s' --> %A" (conRule) (conProd.lhs) (conProd.rhs)
            printfn "FROM Follow of rule %d ==== '%s' --> %A : %A" i (rule.lhs) (rule.rhs) (this.Follow.[rule.lhs])
          else
            tableRow.Add(sym, i)
  //create table


//////// end of impl Grammar


// Creating a new grammar: must give starting non-terminal as it
// will add the 'METASTART' rule.
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


///////////////////////////////  THE RUNTIME PARSER  ////////////////////
(*
  An instance of the type LLparser<'T> is created either with the
  make_parser or the load_parser functions defined beneath.  It contains
  a pointer to a Grammar, a flag (errors) that determines if errors have
  occurred, and a pointer to a lexer.  The type of the lexer must conform
  to the interface AbstractLexer (defined in absLexer.cs).  Parsing occurs
  in two stages.  The forward stage determines if the input is parsable,
  records the left-most derivation and the values and positions of
  terminal symbols representing the input.  The 'synthetic' stage takes
  place after the forward stage and applies semantic actions to synthesize
  the semantic values of grammar symbols bottom-up.  There are three
  stacks that are uses in these stages.
  'parsestack' is the main parsing stack used only in the forward stage. 
  It consists of the right-hand side symbols of production rules, starting
  with the metastart rule.  For example, if the top of the stack contains
  nonterminal A, the next input (lookahead) is a terminal x, and the LL(1)
  parsing table says that rule (A --> x A B) is to be applied, then
  A is popped from the stack and replaced with B A x, with x on top.  If the
  top of the stack contains a terminal such as x, then it must match the
  lookahead, and will be popped from the stack.  The stacked terminal is
  what's expected and the lookahead is the actual input.  A parse error
  occurs when the expected symbol doesn't match the lookahead, or if
  there is no parsing table entry for the non-terminal on top of the stack
  and the lookahead.  There are two other stacks constructed in the
  forward stage.  The 'valuestack' contains the semantic values and their
  lexical line/column positions of each *terminal* symbol that's taken as
  input.  Only the value of terminals are stored in the forward stage.
  The 'rulestack' records each production rule applied as well as the
  position on the valuestack that correspond to the start of the rule.  In
  other words the rulestack records the leftmost derivation of the input.
  The forward stage is implemented in the function parse_top_down

  The bottom-up stage (function compose_bottom_up) takes place only if
  the forward stage was successful.  It applies the semantic actions of
  the rules recorded on the rulestack to the 'Stackitem' values on the
  valuestack, and (essentially) pushes the results back onto the stack
  (although in detail it uses another, internal stack).  At the end there
  should be but one value left on the valuestack and that is returned as
  the result of input.

  The detection of an error cancels all parsing.  Currently there is
  no error-recovery implemented.
*)

type LLparser<'T> =
  {
    Gmr: Grammar<'T>;
    parsestack : Stack<string>;
    rulestack : Stack<int*int>; // rule number and position on VALUESTACK
    valuestack : Stack<Stackitem<'T>>;
    mutable errors : bool;  // determines if errors occurred
    mutable lexer : Fussless.AbstractLexer<unit>; //returns RawTokens
  }
  //  member this.convert_token (rt:Fussless.RawToken) =
  //    new_stackitem("dummy",Unchecked.defaultof<'T>,0,0)

  member this.next_la() =
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
  member this.parse_top_down(trace) =
    let TRACE = trace
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
      if not(stop) && this.parsestack.Count < 1 then
        stop <- true
        this.errors <- true
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
         //if true||(line=0 && column=0) then
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

  member this.set_scanner scanner =
    this.lexer <- scanner

  member this.parse(trace) =
    TRACE <- trace
    try this.parse_top_down(TRACE) with
      | _ -> this.errors<-true
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


  // for saving to json file, returns a jval structure
  member this.to_json(gramname) =
    // create a jval structure as defined in jsonparser.dll
    let json = Dictionary<string,jval>()
    // collect symbols, nonterminals into lists
    let symbols = Vec<jval>()
    let nonterms = Vec<jval>()
    let productions = Vec<jval>()
    for s in this.Gmr.Symbols do symbols.Add(Str(s))
    for nt in this.Gmr.Nonterminals do nonterms.Add(Str(nt))
    json.["Symbols"] <- jval.Seq(symbols)
    json.["Nonterminals"] <- jval.Seq(nonterms)
    json.["startsymbol"] <- Str(this.Gmr.startsymbol)
    // skip first, which is metastart
    for prodi in 1..this.Gmr.Productions.Count-1 do
      let prod = this.Gmr.Productions.[prodi]
      let thisprod = HashMap<string,jval>()
      let thisrhs = Vec<jval>()
      for r in prod.rhs do thisrhs.Add(Str(r))
      thisprod.["lhs"] <- Str(prod.lhs)
      thisprod.["rhs"] <- Seq(thisrhs)
      // semantic actions loaded separately
      productions.Add(Map(thisprod))
    // for each production
    json.["Productions"] <- Seq(productions)
    let table = HashMap<string,jval>()
    for kvp in this.Gmr.LL1Table do
      let colmap = HashMap<string,jval>()
      for tp in kvp.Value do
        colmap.[tp.Key] <- jval.Integer(tp.Value)
      table.[kvp.Key] <- Map(colmap)
    json.["LL1Table"] <- Map(table)
    //lexterminals
    let lexterms = HashMap<string,jval>()
    for kvp in this.Gmr.lexterminals do
      lexterms.[kvp.Key] <- Str(kvp.Value)
    json.["lexterminals"] <- Map(lexterms)
    // valueterminals should be loaded separately
    let kson = jval.Map(json)
    System.IO.File.WriteAllText(sprintf "%s.json" gramname, json_dumps kson)
    //kson

  /////////// end of impl LLparser


// Function to generate LL(1) parsing table from a user-defined grammar
let make_parser<'AT>(gmr:Grammar<'AT>,lexer, trace) =
  let TRACE = trace
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


// Function to load parser from json, requires jsonparser.dll, json_lex.dll.
// The lexer argument is usually give null.  If the loadrules arg is false,
// then only the parsing table is loaded and it is assumed that the parser
// is otherwise set up manually.
let load_parser_into<'AT>(gm1:Grammar<'AT>,filename,lexer,loadrules:bool) =
  let fd = new System.IO.FileStream(filename ,System.IO.FileMode.Open);
  let jparser = Json.make_parser()
  let jlexer = Fussless.jsonlexer<unit>(fd)
  let jsonraw = Json.parse_with(jparser,jlexer)
  match jsonraw with
   | Some(Map(json)) ->
     printfn "Loaded JSON: %A\n" (jsondumps (Map(json)))
     let mutable Gmr:Grammar<'AT> = gm1
     if loadrules then
      let topsym = getstr json.["startsymbol"]
      Gmr <- new_grammar(topsym)
      for sym in (getseq json.["Symbols"]) do Gmr.Symbols.Add(getstr sym) |> ignore
      for sym in (getseq json.["Nonterminals"]) do Gmr.Nonterminals.Add(getstr sym) |> ignore
      for kvp in (getmap json.["lexterminals"]) do
        Gmr.lexterminals.[kvp.Key] <- getstr kvp.Value
      for prod in (getseq json.["Productions"]) do
        let jprod = getmap prod
        let prhs = Vec<string>()
        for s in (getseq jprod.["rhs"]) do prhs.Add(getstr s)
        let production =
          {
            Production.lhs = getstr jprod.["lhs"];
            Production.rhs = prhs
            Production.action = fun x -> Unchecked.defaultof<'AT>;
          }
        Gmr.Productions.Add(production)
     for kv in (getmap json.["LL1Table"]) do // aligns with if loadrules
        let column = HashMap<string,int>()
        Gmr.LL1Table.[kv.Key] <- column
        for kvp in (getmap kv.Value) do
          column.[kvp.Key] <- (getint kvp.Value)
     {
        LLparser.Gmr = Gmr;
        parsestack = Stack<string>();
        rulestack = Stack<int*int>();
        valuestack = Stack<Stackitem<'AT>>();
        errors = false;
        lexer = lexer;       
     }
    | _ -> raise (Exception("Error loading grammar from JSON file"))

let load_parser<'AT>(fn):LLparser<'AT> =
  load_parser_into(new_grammar<'AT>(""),fn,null,true)
