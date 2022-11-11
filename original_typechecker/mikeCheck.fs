module MikeCheck
open MichaeLL;
open System;
open System.Collections.Generic;
open Option;

(* QUESTIONS
Can I update types from LLunkown if I infer their type? If so, how?
How can I ensure that parameter names are not the same as function names?
What is the difference between 'define' and 'let'?
*)

//////// type aliases
type Vec<'A> = ResizeArray<'A>
type Conslist<'A> = 'A list
type HashMap<'A,'B> = Dictionary<'A,'B>

let rec grounded_type = function    // equiv. to match arg with ...
  | LLunknown | LLvar(_) | LLuntypable -> false
  | LList(t) -> grounded_type t
  | lltype.LLtuple(vs) -> List.forall grounded_type vs
  | LLfun(args,rtype) ->  List.forall grounded_type (rtype::args)
  | _ -> true

type table_entry =
  {
     mutable typeof: lltype;
     gindex: int;  // unqiue index for brute-force alpha-conversion
     ast_rep : expr option;  // link to ast representation (just pointer)
  }
and table_frame =
  {
    name : string;
    entries:HashMap<string, table_entry>;
    parent_scope:table_frame option;
  }

type TableEntry =
  | SimpleDef of typeof:lltype * gindex:int * ast_rep:option<expr>
  | LambdaDef of typeof:lltype *gindex:int * frame:table_frame * ast_rep:option<expr>

// let check_lambda
(*let check_def(lb:LBox, axpr:expr) = 
  match lb.value with 
    | (TypeExpr(a),Alphanum(s)) -> a
    | (Alphanum(s)) -> LLunknown
    | _ -> LLuntypable
*)

type SymbolTable = // wrapping structure for symbol table frames
  { 
    mutable current_frame: table_frame;
    mutable global_index: int;
    frame_hash:HashMap<(int*int),table_frame>;
  }
  member this.add_entry(s:string,t:lltype,a:expr option) = //overwrite
    this.global_index <- this.global_index + 1
    this.current_frame.entries.Add(s,{typeof=t; gindex=this.global_index; ast_rep=a;})

  member this.push_frame(n,line,column) =
    let newframe =
      {
        table_frame.name=n;
        entries=HashMap<string,table_entry>();
        parent_scope = Some(this.current_frame);
      }
    this.frame_hash.[(line,column)] <- newframe
    this.current_frame <- newframe

  member this.pop_frame() = 
    this.current_frame.parent_scope |> map (fun p -> this.current_frame<-p)


  member this.get_entry(var) =
    // follow parent pointer until find some or none
    // return some or none
    let mutable frame = Some(this.current_frame)
    let mutable entry = None
    while isSome (frame) && isNone entry do
      if frame.Value.entries.ContainsKey(var) then
        entry <- Some(frame.Value.entries.[var])
      else 
        frame <- frame.Value.parent_scope
    entry

  member this.infer_type (expression:expr, line:int) = 
    match expression with 
      | Integer(_) -> LLint
      | Floatpt(_) -> LLfloat
      | Strlit(_) -> LLstring
      // Binop does not allow interaction between int and float for simplicity
      | Binop("+",a,b) | Binop("-",a,b) | Binop("*",a,b) | Binop("/",a,b) | Binop("%",a,b) -> 
        let (atype,btype) = (this.infer_type(a,line), this.infer_type(b,line))
        if (atype=btype && (atype=LLint || atype=LLfloat)) || btype = LLunknown then 
          if atype=LLunknown then LLint
          else 
            atype
        else 
          printfn "Line %d: mathematical operator expected type 'LLint*LLint' or 'LLfloat*LLfloat' but has type '%A*%A'" line atype btype
          LLuntypable
      | Binop(s,a,b)-> 
        let (atype,btype) = (this.infer_type(a,line), this.infer_type(b,line))
        if atype=btype && (atype=LLint || atype=LLfloat) then LLint
        else 
          printfn "Line %d: boolean operator '%s' expected type 'LLint*LLint' or 'LLfloat*LLfloat' but has type '%A*%A'" line s atype btype
          LLuntypable
      | Uniop("NEG",a) ->
        let atype = this.infer_type(a,line)
        if atype = LLint || atype = LLfloat then atype
        else
          printfn "Line %d: '~' expected type 'LLint' or 'LLfloat' but has type '%A'" line atype
          LLuntypable
      | Uniop("not",a) ->
        let atype = this.infer_type(a,line)
        if atype = LLint then atype
        else
          printfn "Line %d: 'not' expected type 'LLint' but has type '%A'" line atype
          LLuntypable
      | Uniop("cdr",Sequence(a)) ->
        let atype = this.infer_type(Sequence(a),line)
        match atype with
        | LList(ty) -> LList(ty)
        | _ -> 
          printfn "Line %d: 'cdr' expected type 'LList' but has type '%A'" line atype
          LLuntypable 
      | Uniop("car", Sequence(a::b)) ->
        let (atype,btype) = (this.infer_type(a.value, a.line), this.infer_type(Sequence(a::b), line))
        if LList(atype) = btype then atype 
        else 
          printfn "Line %d: 'car' on 'LList' has type %A but 'LList' has type '%A'" line atype btype
          LLuntypable
      | Uniop("display",a) ->
        let atype = this.infer_type(a, line)
        if atype <> LLuntypable then 
          LLunit
        else
          printfn "Line %d: 'display' found type 'LLuntypable'" line
          LLuntypable
      | Ifelse(s,a,b) ->
        let (stype, atype, btype) = (this.infer_type(s,line),this.infer_type(a,line),this.infer_type(b,line))
        if stype <> LLint then 
          printfn "Line %d: If condition expected boolean operation with type 'LLint' but found '%A'" line stype
          LLuntypable
        elif atype = btype then
          atype
        else
          if atype = LLuntypable || btype = LLuntypable then 
            printfn "Line %d: If expression found 'LLuntypable'" line
            LLuntypable
          elif atype = LLunknown then btype
          elif btype = LLunknown then atype
          else 
            printfn "Line %d: If expression types ('%A' and '%A') do not match" line atype btype
            LLuntypable
      | Whileloop(a,b) -> 
        let atype = this.infer_type(a,line)
        if not(atype = LLint) then // don't accept floats either
          printfn "Line %d: while loop conditional statement expected type 'LLint' but has type '%A'" line atype
          LLuntypable
        else 
          this.infer_type(b,line)
      | Setq(x,a) ->
        let atype = this.infer_type(a,x.line)
        match this.get_entry(x.value) with
          | None ->
            printfn "Line %d: Variable '%s' has not been declared" line (x.value)
            LLuntypable
          | Some(v) -> 
            // can I change a type if it is previously LLunknown and I have inferred something to be a certain type in another function 
            if (v.typeof) = atype || (v.typeof) = LLunknown then
              atype
            else 
              printfn "Line %d: Expected type '%A' of '%s' but expression has type '%A'" line (v.typeof) (x.value) atype
              LLuntypable
      
            
      | Var(x) -> 
        match this.get_entry(x) with
          | None -> 
            printfn "Line %d: Variable '%s' has not been declared" line x
            LLuntypable
          | Some(x) -> (x.typeof)
      | TypedDefine(x,a) ->
        match x.value with 
          | (tyv,s) ->
            let entry = this.get_entry(s)
            match entry with
              | Some(a) -> 
                printfn "Line %d, Column %d: '%s' has already been defined" (x.line) (x.column) (s)
                LLuntypable
              | None -> 
                match a with 
                  | TypedLambda(param, ty, axpr) -> // NOT DONE
                    // x:LBox<expr> list
                    let mutable ptypes = []
                    this.add_entry(s, ty, Some(axpr.value))
                    this.push_frame(s, x.line, x.column)
                    let mutable isUntypable = false
                    for i in param do
                      match i.value with
                        | TypedVar(ty,s) -> 
                          if isSome(this.get_entry(s)) then
                            printfn "Line %d, Column %d: Parameter '%s' has already been defined" (i.line) (i.column) s // looks through all scopes but should only check current scope
                            isUntypable <- true
                            ptypes <- LLuntypable::ptypes
                          else 
                            this.add_entry(s, ty, None)
                            ptypes <- ty::ptypes
                        | _ -> 
                          printfn "Line %d: Expected typed variable in lambda parameters but found '%A'" (i.line) (i.value) // should never happen based on grammar
                          isUntypable <- true
                          ptypes <- LLuntypable::ptypes
                    
        
                    let mutable ptypes_rev = []
                    for i in ptypes do
                      ptypes_rev <- i::ptypes_rev
                    let atype = this.infer_type(axpr.value,axpr.line)
                    let ret = LLfun(ptypes_rev, atype)
                    if (isUntypable) then
                      LLuntypable
                    elif (ty = atype || ty =  LLunknown) && atype <> LLuntypable then
                      if (atype = tyv || atype = LLunknown || atype = LList(LLunknown)) then 
                        this.add_entry(s, ret, Some(axpr.value))
                        this.pop_frame() |> ignore
                        ret
                      else
                        printfn "Line %d: 'TypedDefine' type '%A' does not match expression with type '%A'" (x.line) tyv atype
                        LLuntypable
                    else 
                      printfn "Line %d: 'define' type of variable '%s' and associated lambda are not the same. '%s':'%A' but 'lambda':'%A'" (x.line) (s) (s) ret atype
                      this.pop_frame() |> ignore
                      LLuntypable
                  | _ -> 
                    let atype = this.infer_type(a,x.line)
                    if atype <> LLuntypable && (atype = tyv || atype = LLunknown) then
                      this.add_entry(s, tyv, Some(a))
                      LLvar(s)
                    else
                      printfn "Line %d: Expression 'define' of '%s' expected type '%A' but found type '%A'" line s tyv atype
                      LLuntypable
      | TypedLet(v,a,axpr) -> 
        match v.value with
          | (ty, s) -> 
            match this.get_entry(s) with
              | Some(a) -> 
                printfn "Line %d, Column %d: '%s' has already been defined" (v.line) (v.column) s
                LLuntypable
              | None -> 
                this.add_entry(s,ty,Some(a))
                let axtype = this.infer_type(axpr.value, axpr.line)
                axtype
      | TypedLambda(param, ty, axpr) -> 
        let axtype = this.infer_type(axpr.value, line) 
        let mutable ptypes = []
        this.push_frame("tempLam",axpr.line,axpr.column)
        for i in param do
          match i.value with
            | Var(s) -> 
              this.add_entry(s, LLunknown, None)
              ptypes <- LLunknown::ptypes
            | TypedVar(ty,s) -> 
              this.add_entry(s, ty, None)
              ptypes <- ty::ptypes
            | _ -> 
              printfn "Line %d: Expexted 'Var' or 'TypedVar' in lambda parameters but found '%A'" (i.line) (i.value)
        let mutable ptypes_rev = []
        for i in ptypes do
          ptypes_rev <- i::ptypes_rev
        let ret = LLfun(ptypes_rev, this.infer_type(axpr.value,axpr.line))

        if (ty = LLunknown || axtype = ty) && axtype <> LLuntypable then  
          this.pop_frame() |> ignore
          LLfun(ptypes_rev,axtype)
        else
          this.pop_frame() |> ignore
          printfn "Line %d: lambda function type '%A' and expression type '%A' do not match" line ty axtype
          LLuntypable
      | Sequence(a::b) when b.Length > 0  -> 
        let atype = this.infer_type(a.value, a.line)
        if atype = LLuntypable then 
          printfn "Line %d, Column %d: Unexpected type '%A' in Sequence" (a.line) (a.column) atype
          LLuntypable
        else 
          this.infer_type(Sequence(b),line)
      | Sequence(a::b) -> 
        let atype = this.infer_type(a.value, a.line)
        if atype <> LLuntypable then LList(atype)
        else LLuntypable


      // Vectors

      | _ -> LLuntypable 

// global symbol table
let mutable global_frame = // root frame
  {
    table_frame.name = "global";
    entries= HashMap<string,table_entry>();
    parent_scope = None;
  }
let symbol_table =
  {
    SymbolTable.current_frame=global_frame;
    global_index = 0;
    frame_hash = HashMap<(int*int),table_frame>();
  }

//parse(true); // TRACE = true
//let res = parse(true); // TRACE = true
let res = parse(false); // TRACE = false
match res with
  | Some(a) ->
    let t = symbol_table.infer_type(a,0);
    printfn "Type: %A" t
  | None -> printfn "CANNOT TYPECHECK... UNRECOVERABLE"








