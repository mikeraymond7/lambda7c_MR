module lambda7c
open System;
open System.Collections.Generic;
open Option;
open MichaeLL;
open MikeCheck;


(* Abstract representation of Simplified LLVM IR:  (includes assignment)

LLVM looks like a monster but actually a lot of the stuff you see in a file
produced by   clang -emit-llvm -S   is optional.  Also, not all possible
instructions are as important as others, especially all the possible forms
of the notorious 'getelementptr' instruction. So the abstract representation
types defined here also define a small subset of the LLVM instruction set.
It includes what I believe are the essential instructions that you will need
to write a basic compiler.  We may have to tweak or add a few new cases as
necessary.  The main types are:

LLVMtype : represents LLVM type expressions such as i32
LLVMexpr : expressions can be registers, %r1, or constants 1, etc
Instruction: this is the most important type to study and understand
LLVMdeclaration : for global declarations such as for string literals
BasicBlock : all instructions are in basic blocks, each block has a label
LLVMfunction : represents a function declaration, which is always global
LLVMprogram : top level structure consisting of declarations and functions

There is (currently) only one function in this file: 'destination', which
retrives the destination register of an instruction as an Option<string>.
Not all instructions have destinations.  For example, for %r1 = add i32 ...
the destination is Some("r1").
*)

type Vec<'T> = ResizeArray<'T>
type HashMap<'K,'V> = System.Collections.Generic.Dictionary<'K,'V>
type Conslist<'K> = 'K list

type LLVMtype =
  | Basic of string     // i32: Basic("i32")
  | Pointer of LLVMtype //i32*:  Pointer(Basic("i32"))
  | Array_t of int*LLVMtype  // [5 x i8], Array_t(5,Basic("i8"))
  | Userstruct of string
  | Ellipsis  // special case for optional args, like printf(i8*,...)
  | Void_t

type LLVMexpr =
  | Iconst of int
  | Sconst of string
  | Fconst of float
  | I1const of bool  // booleans of type i1 are true and false
  | Register of string   // %r1
  | Global of string     // @str1
  | Label of string      // not really used
  | Novalue     // default

type Instruction =  
  // terminator instructions that ends a Basic Block:
  | Ret of LLVMtype*LLVMexpr  // ret i32 4
  | Ret_noval                 // ret
  | Br_uc of string   // unconditional jump: br label %label1
  | Bri1 of LLVMexpr*string*string //always on i1
  // memory ops: store i32 3, i32* %a, optional "align 4", type i32* is omitted
  | Load of string*LLVMtype*LLVMexpr*Option<string>
  | Store of LLVMtype*LLVMexpr*LLVMexpr*Option<string>  //pointer type omitted
  | Alloca of string*LLVMtype*Option<string> // %r1 = alloca i64, align 8
  // ALU ops, always begins with a destination : %r1 = ...
  | Binaryop of string*string*LLVMtype*LLVMexpr*LLVMexpr
  | Unaryop of string*string*Option<string>*LLVMtype*LLVMexpr // fneg float %r1
  // casting operations like %r2 = zext i32 %r1 to i64
  | Cast of string*string*LLVMtype*LLVMexpr*LLVMtype
  // comparison and selection ops, phi
  | Icmp of string*string*LLVMtype*LLVMexpr*LLVMexpr //1st string is dest
  | Fcmp of string*string*LLVMtype*LLVMexpr*LLVMexpr
  | SelectTrue of string*LLVMtype*LLVMexpr*LLVMexpr
  | Phi2 of string*LLVMtype*LLVMexpr*string*LLVMexpr*string
  | Phi of string*LLVMtype*Conslist<(LLVMexpr*string)> //not as common as Phi2
  // function call: option dest,ret type,
  | Call of Option<string>*LLVMtype*Conslist<LLVMtype>*string*Conslist<(LLVMtype*LLVMexpr)>
  // simplified forms of getelementptr for array address and struct field
  | Arrayindex of string*int*LLVMtype*LLVMexpr*LLVMexpr
  | Structfield of string*LLVMtype*LLVMexpr*LLVMexpr
  // other llvm instructions not covered by above must be encoded as:
  | Verbatim of string //generic "other" instruction, default case, comments


let rec exToString(e:LLVMexpr) = 
  match e with
    | Iconst(i) -> i.ToString()
    //| Sconst(s) -> s.Replace("\"","")
    | Sconst(s) -> sprintf "c\"%s\\00\"" (s.Replace("\"",""))
    | Fconst(f) -> f.ToString()
    | I1const(b) -> b.ToString()
    | Register(r) -> "%" + r
    | Global(g) -> "@" + g
    | Label(l) -> "label %" + l
    | Novalue -> ""

let rec tyToString(t:LLVMtype) =
  match t with
    | Basic(b) -> b
    | Pointer(ty) -> tyToString(ty) + "*"
    | Array_t(i,ty) -> "[" + i.ToString() + " x " + tyToString(ty)
    | Userstruct(s) -> s
    | Ellipsis -> "..."
    | Void_t -> "void"

let rec iToString(i:Instruction) =
  match i with
    | Ret(ty, expr) -> "ret " + tyToString(ty) + " " + exToString(expr)
    | Ret_noval -> "ret"
    | Br_uc(s) -> "br label %" + s
    | Bri1(ex,l1,l2) -> "br i1 " + exToString(ex) + ", label %" + l1 + ", label %" + l2
    | Load(r,ty,ex,opt) ->
      let typ = tyToString(ty)
      let ret = "%" + r + " = load " + typ + ", " + typ + "* " + exToString(ex)
      match opt with
        | Some(a) -> ret + ", " + a
        | None -> ret
    | Store(ty,ex1,ex2,opt) -> 
      let typ = tyToString(ty)
      let ret = "store " + typ + " " + exToString(ex1) + ", " + typ + "* " + exToString(ex2)
      match opt with
        | Some(a) -> ret + ", " + a
        | None -> ret
    | Alloca(s,ty,opt) -> 
      let ret = "%" + s + " = alloca " + tyToString(ty)
      match opt with
        | Some(a) -> ret + ", " + a.ToString()
        | None -> ret
    | Binaryop(s,binop,ty,ex1,ex2) -> 
      "%" + s + " = " + binop + " " + tyToString(ty) + " " + exToString(ex1) + ", " + exToString(ex2)
    | Unaryop(s,uniop,opt,ty,ex) -> 
      let ret = "%" + s + " = " + uniop 
      match opt with
        | Some(a) -> ret + " " + a + " " + tyToString(ty) + " " + exToString(ex)
        | None -> ret + " " + tyToString(ty) + " " + exToString(ex)
    | Cast(s,cast,ty1,ex,ty2) -> 
      "%" + s + " = " + cast + " " + tyToString(ty1) + " " + exToString(ex) + " to " + tyToString(ty2)
    | Icmp(s,op,ty,ex1,ex2) -> 
      "%" + s + " = icmp " + op + " " + tyToString(ty) + " " + exToString(ex1) + ", " + exToString(ex2)
    | Fcmp(s,op,ty,ex1,ex2) -> 
      "%" + s + " = fcmp " + op + " " + tyToString(ty) + " " + exToString(ex1) + ", " + exToString(ex2)
    | SelectTrue(s,ty,ex1,ex2) -> 
      let typ = tyToString(ty)
      "%" + s + " = select il true, " + typ + " " + exToString(ex1) + ", " + typ + " " + exToString(ex2)
    | Phi2(s,ty,ex1,blk1,ex2,blk2) -> 
      "%" + s + " = phi " + tyToString(ty) + " [" + exToString(ex1) + ", %" + blk1 + "], [" + exToString(ex2) + ", %" + blk2 + "]" 
    | Phi(s,ty,cl) -> // should have trailing "," at end check to see if goes out of bounds first
      let mutable ret = "%" + s + " = phi " + tyToString(ty) + " " 
      if cl.Length > 0 then // this should always be true
        for i = 0 to (cl.Length - 2) do
          let (ex,blk1) = cl.[i]
          ret <- ret +  "[" + exToString(ex) + ", %" + blk1 + "], "
        let (ex,blk1) = cl.[cl.Length-1]
        ret <- ret +  "[" + exToString(ex) + ", %" + blk1 + "]"
      ret
    | Call(opt,ty,pty,func,param) ->
      let mutable ret = ""
      match opt with 
        | Some(s) -> ret <- "%" + s + " = call "
        | None -> ret <- "call "
      ret <- ret + tyToString(ty) 
      if pty.Length > 0 then 
        ret <- ret + " ("
        for i = 0 to (pty.Length-2) do
          ret <- ret + tyToString((pty.[i])) + ", "
        ret <- ret + tyToString((pty.[(pty.Length-1)])) + ")"

      ret <- ret + " @" + func + "("
      for i = 0 to param.Length-2 do 
        let (ty,ex) = param.[i]
        ret <- ret + tyToString(ty) + " " + exToString(ex) + ", "
      if param.Length > 0 then  
        let (ty,ex) = param.[(param.Length-1)]
        ret <- ret + tyToString(ty) + " " + exToString(ex) 

      ret + ")"
    | Arrayindex(s,i,ty,ex1,ex2) -> 
      let typ = tyToString(ty)
      let exStr = exToString(ex2)
      sprintf "%%%s = getelementptr inbounds [%d x %s], [%d x %s]* %s, i64 %s, i64 %s" s i typ i typ (exToString(ex1)) exStr exStr
    | Structfield(s,ty,ex1,ex2) -> 
      let typ = tyToString(ty)
      sprintf "%%%s = getelementptr inbounds %%%s, %%%s* %s i32 0, i32 %s" s typ typ (exToString(ex1)) (exToString(ex2))
    | Verbatim(s) -> s
    //| _ -> ""

// TODO
let translate_type(ty:lltype) = // returns LLVMtype
  match ty with
    | LLint -> Basic("i32")
    | LLfloat -> Basic("double")
    | LLstring -> Pointer(Basic("i8")) 
    | _ -> Void_t




// extracts the destination register from an instruction, returns string option
let destination = function
  | Load(s,_,_,_) | Alloca(s,_,_) | Unaryop(s,_,_,_,_) -> Some(s)
  | Binaryop(s,_,_,_,_) | Call(Some(s),_,_,_,_) -> Some(s)
  | Cast(s,_,_,_,_) | Icmp(s,_,_,_,_) | Fcmp(s,_,_,_,_) -> Some(s)
  | SelectTrue(s,_,_,_) | Phi2(s,_,_,_,_,_) | Phi(s,_,_) -> Some(s)
  | Arrayindex(s,_,_,_,_) | Structfield(s,_,_,_) -> Some(s) 
  | _ -> None  // includes case for Verbatim


type LLVMdeclaration =
  | Globalconst of string*LLVMtype*LLVMexpr*Option<string>
  | Externfunc of LLVMtype*string*Vec<LLVMtype>
  | Structdec of string*Vec<LLVMtype>
  | Verbatim_dec of string // cases not covered by above

(*
   LLVM declarations are special cases of globally scoped instructions.
   LLVM allows forward reference

 Globalconst("str1",Array_t(6,Basic("i8")),Sconst("hello\00"),Some("align 1")) :
    @str1 = constant [6 x i8] c"hello\00", align 1
 Externfunc(Basic("i32"),"printf",[Pointer(Basic("i8")),Ellipsis]) :
    declare i32 @printf(i8*, ...)
 Structdec("bigstruct",[Basic("i32"),Basic("i8"),Basic("double")]) :
    %struct.bigstruct = type { i32, i8, double }
*)


type BasicBlock =
  {
     label: string;
     body: Vec<Instruction>; // last instruction must be a terminator
     //predecessors : Vec<BasicBlock>; //control-flow graph, not used for now
  }

type LLVMFunction =
  {
     name: string;
     formal_args : Vec<(LLVMtype*string)>;
     return_type : LLVMtype;  // only basic and pointer types can be returned
     body: Vec<BasicBlock>;
     attributes : Vec<string>; // like "dso_local", "#1", or ["","#1"]
  }
  // block is closed iff it branches to another bb or is the very last block and has "ret"

  // add instruction to the last basic block
  member this.add_inst(i:Instruction) = 
    // add instruction to last basic block
    let len = this.body.Count
    let lastBB = this.body.[len-1]
    lastBB.body.Add(i)

  member this.new_bb(lab:string) = 
    let newBB:BasicBlock = {
      label = lab
      body = Vec()
    }
    this.body.Add(newBB)

   member this.currentBBlabel() = 
     let len = this.body.Count
     let curBB = this.body.[len-1]
     curBB.label

type LLVMprogram =
  {
     mutable preamble : string;   // arbitrary stuff like target triple
     global_declarations : Vec<LLVMdeclaration>;
     functions: Vec<LLVMFunction>;
     mutable postamble : string;  // stuff you don't want to know about
  }
  
  member this.add_new_func(fname, args, rty) = 
    let func = {
      name = fname
      formal_args = args 
      return_type = rty
      body = Vec()
      attributes = Vec()
    }
    this.functions.Add(func)
    func

  member this.to_string() = 
    let mutable code_gen = this.preamble 
    for fn in this.functions do

      let mutable def = sprintf "\ndefine %s @%s(" (tyToString(fn.return_type)) (fn.name) 
      let num_params = fn.formal_args.Count

      if num_params > 0 then

        for i = 0 to  (num_params - 2) do
          let (ty, p) = fn.formal_args.[i]
          def <- def + sprintf "%s %%%s," (tyToString(ty)) p

        let (ty, last) = fn.formal_args.[num_params-1]
        def <- def + sprintf "%s %%%s" (tyToString(ty)) last
      def <- def + ") {"
      code_gen <- code_gen + def

      for b in fn.body do

        code_gen <- code_gen + sprintf "\n\n%s:"  (b.label)

        for i in b.body do

          code_gen <- code_gen + "\n  " + iToString(i)

      code_gen <- code_gen + "\n}"
    code_gen <- code_gen + "\n" + this.postamble
    code_gen
  
  member this.add_preamble(s) = 
    this.preamble <- this.preamble + "\n" + s

type LLVMCompiler =
  {
    mutable symbol_table : SymbolTable;
    mutable program: LLVMprogram;  // to be built
    mutable gindex: int; // global counter
    mutable lindex: int; // local counter
    // .. other stuff omitted
  }//struct LLVMCompiler

  member this.newid(s) = 
    let x = sprintf "%s_%d" s (this.gindex)
    this.gindex <- this.gindex + 1
    x

  member this.compile_expr(ex:expr, func:LLVMFunction) = // return LLVMexpr
    match ex with 
      | Integer(x) -> Iconst(x)
      | Floatpt(f) -> Fconst(f)
      | Strlit(s) -> 
        let str = exToString(Sconst(s))
        let byte_str = str.Replace("\\n","\\0A")
        let num_0A = byte_str.Length-str.Length
        let sname = this.newid(".str")
        // remove length for 2 quotation marks, char c, ending \00, and all \0A

        let len = byte_str.Length - (5+num_0A * 2)
        let gconst = this.program.add_preamble(sprintf "@%s = constant [%d x i8] %s, align 1" sname len (byte_str))
        let r = this.newid("r")
        let arr = Arrayindex(r,(len), Basic("i8"), Global(sname), Iconst(0))
        func.add_inst(arr)
        Register(r)

//      | TypedVar(ty,s) -> // no case for this
//      | Nil ->
//      | Uniop(s,a) ->
      | Binop("+",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Binaryop(r1, "fadd", rtype, adest, bdest))
        else
          func.add_inst(Binaryop(r1, "add", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("-",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Binaryop(r1, "fsub", rtype, adest, bdest))
        else
          func.add_inst(Binaryop(r1, "sub", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("*",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Binaryop(r1, "fmul", rtype, adest, bdest))
        else
          func.add_inst(Binaryop(r1, "mul", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("DIV",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Binaryop(r1, "fdiv", rtype, adest, bdest))
        else
          func.add_inst(Binaryop(r1, "div", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop(">",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "ogt", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "sgt", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("<",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "olt", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "slt", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("=",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "oeq", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "eq", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("<=",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "ole", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "sle", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop(">=",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "oge", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "sge", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("%",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "frem", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "srem", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("neq",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "une", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "ne", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("eq",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "oeq", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "eq", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("and",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Fcmp(r1, "and", rtype, adest, bdest))
        else
          func.add_inst(Icmp(r1, "and", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("or",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Binaryop(r1, "fmul", rtype, adest, bdest))
        else
          func.add_inst(Binaryop(r1, "mul", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Binop("^",a,b) -> 
        let adest = this.compile_expr(a, func)
        let bdest = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = translate_type(this.symbol_table.infer_type(a,0))
        if rtype = Basic("double") then
          func.add_inst(Binaryop(r1, "fmul", rtype, adest, bdest))
        else
          func.add_inst(Binaryop(r1, "mul", rtype, adest, bdest))
        Register(r1)
      // Binop *
      | Ifelse(bl,t,f) -> 
        let cdest = this.compile_expr(bl, func)
        //let ccast = this.newid("r")
        //func.add_inst(Cast(ccast,"trunc",Basic("i32"),cdest,Basic("i1"))); // cdest is bool
        let label1 = this.newid("iftrue")
        let label0 = this.newid("iffalse")
        let endif = this.newid("endif")
        let brinst = Bri1(cdest,label1,label0)
        //let brinst = Bri1(Register(ccast),label1,label0)
        let predlabel = func.currentBBlabel()
        func.add_inst(brinst) // close block

        func.new_bb(label1) //|> ignore // open BB1
        let dest1 = this.compile_expr(t,func)
        let realabel1 = func.currentBBlabel()
        func.add_inst(Br_uc(endif)) // close BB1

        func.new_bb(label0) //|> ignore // open BB0
        let dest0 = this.compile_expr(f,func)
        let realabel0 = func.currentBBlabel()
        func.add_inst(Br_uc(endif)) // close BB0

        func.new_bb(endif) //|> ignore // open newBB
        let desttype = translate_type(this.symbol_table.infer_type(t,0))
 
        if desttype <> Void_t then 
          let fdest = this.newid("r")
          let phiinst = Phi2(fdest,desttype,dest1,realabel1,dest0,realabel0)
          func.add_inst(phiinst)
          Register(fdest)
        else 
          Novalue 
      // Ifelse        
      | Whileloop(bl,seq) ->
        // basic block labels for function
        let label_check = this.newid("check")
        let label_loop = this.newid("loop")
        let label_endloop = this.newid("endloop")

        let br_uc = Br_uc(label_check)
        func.add_inst(br_uc) // close current label

        func.new_bb(label_check) // open check:
        let bdest = this.compile_expr(bl,func)

        let brinst = Bri1(bdest, label_loop,label_endloop)
        let predlabel = func.currentBBlabel()
        func.add_inst(brinst) // close check:

        func.new_bb(label_loop) // open loop:
        let destloop = this.compile_expr(seq,func)
        let realooplabel = func.currentBBlabel()
        func.add_inst(Br_uc(label_check)) // close loop:

        func.new_bb(label_endloop) // open endloop:
        
        Novalue
      // Whileloop
(* --------------------------------------
      | App(xbox, seq) ->
        let x = sbox.value
        // compile each item in params and use compiled registers for function call
        //let reg_params = 
        Iconst(0)
      // App
   -------------------------------------- *)
      | TypedDefine(stbox, TypedLambda(param, tyv, a)) ->
        printfn "HEREHERE\n\n\n\n"
        let (ty, s) = stbox.value
        let entry = this.symbol_table.get_entry(s).Value
        let funname = this.newid((sprintf "%s_%d" s (entry.gindex)))
        let args:Vec<LLVMtype*string> = Vec() // convert parameters to Vec<(LLVMtype*string)>
        for i = 0 to param.Length - 1 do
          match param.[i].value with
            | Var(x) -> 
              let entry = this.symbol_table.get_entry(x).Value
              args.Add((translate_type(entry.typeof),x))
            | TypedVar(ty,x) ->
              let entry = this.symbol_table.get_entry(x).Value
              args.Add((translate_type(entry.typeof),x))
            | _ -> 
              printfn "If you somehow got here, you are hopeless"

        let new_func = this.program.add_new_func(funname, args, translate_type(ty))
        let cdest = this.compile_expr(a.value,new_func)
        let rty = translate_type(tyv)
        match rty with 
          | Void_t -> new_func.add_inst(Ret_noval)
          | _ -> new_func.add_inst(Ret(rty, cdest))
        cdest 
 
      // TypedDefine Lambdas
      | Define(sbox,a) -> // cannot be lambda 
        let x = sbox.value
        let cdest = this.compile_expr(a, func)
        let entry = this.symbol_table.get_entry(x).Value // will error out in typechecking phase if None
        let desttype = translate_type(entry.typeof)
        let new_x = sprintf "%s_%d" x (entry.gindex) // will crash if user declares "r"
        //let new_x = this.newid((sprintf "%s_%d" x (entry.gindex)))
        func.add_inst(Alloca(new_x,desttype,None))
        func.add_inst(Store(desttype,cdest,Register(new_x),None))
        Register(new_x) 
      // Define
      | Var(s) ->  
        let entry = this.symbol_table.get_entry(s).Value
        let new_x = sprintf "%s_%d" s (entry.gindex)
        let desttype = translate_type(entry.typeof)
        let r = this.newid(s)
        func.add_inst(Load(r, desttype, Register(new_x), None))
        //Register(new_x)
        Register(r)
      // Var
//      | TypedDefine(tsbox,a) ->
//      | Lambda -> Iconst(1)
//      | TypedLambda -> Iconst(1)
//      | Let(sbox,a,bbox) -> Iconst(1)
//      | TypedLet(tsbox,a,bbox) -> Iconst(1)
      | Setq(sbox,a) -> 
        let x = sbox.value
        let entry = this.symbol_table.get_entry(x).Value
        let new_x = sprintf "%s_%d" x (entry.gindex)
        let cdest = this.compile_expr(a, func)
        let desttype = translate_type(entry.typeof)
        func.add_inst(Store(desttype, cdest, Register(new_x), None))
        Register(new_x)
      // Setq
      | Uniop("display", a) ->
          let bdest = this.compile_expr(a,func)
          let rtype = translate_type(this.symbol_table.infer_type(a,0))
          match rtype with 
            | Basic("i32") ->
              func.add_inst(Call(None, Void_t,[], "lambda7c_printint", [(Basic("i32"), bdest)]))
              Novalue
            | Basic("double") -> 
              func.add_inst(Call(None, Void_t,[], "lambda7c_printfloat", [(Basic("double"), bdest)]))
              Novalue
            | Pointer(Basic("i8")) -> 
              func.add_inst(Call(None, Void_t,[], "lambda7c_printstr", [(Pointer(Basic("i8")), bdest)]))
              Novalue
            | t -> 
              printfn "Unsupported print type '%A'" t
              Novalue
              
              
(*
          match a with 
            | Var(x) -> 
              let entry = this.symbol_table.get_entry(x).Value
              let desttype = entry.typeof
              let printtype = translate_type(desttype)
              let mutable printfun = "lambda7c_print"
              if printtype = Basic("i32") then
                printfun <- printfun + "int"
              else if printtype = Basic("double") then
                printfun <- printfun + "float"
              else if printtype = Basic("double") then 
                printfun <- printfun + "str"
              let cdest = this.compile_expr(Var(x),func)
              func.add_inst(Call(None, Void_t,[], printfun, [(printtype, cdest)]))
              Novalue
            | Integer(i) ->
              func.add_inst(Call(None, Void_t,[], "lambda7c_printint", [(Basic("i32"), Iconst(i))]))
              Novalue
            | Floatpt(f) ->
              func.add_inst(Call(None, Void_t,[], "lambda7c_printfloat", [(Basic("double"), Fconst(f))]))
              Novalue
            | Strlit(s) ->
              let str = this.newid(".str")
              // @str1 = constant [16 x i8] c"gcd(16,6) = %d\0a\00", align 1
              this.program.add_preamble(sprintf "@%s = constant [%d x i8] %s, align 1" str (s.Length - 1) (exToString(Sconst(s))))
              let r = this.newid("r")
              func.add_inst(Arrayindex(r, (s.Length-1), Basic("i8"), Global(str), Iconst(0)))
              func.add_inst(Call(None, Void_t,[], "lambda7c_printstr", [(Pointer(Basic("i8")), Register(r))]))
              Novalue
              
            | axpr -> 
              let atype = this.symbol_table.infer_type(axpr,0)
              Novalue
 *)
      | Sequence(a) -> 
        this.compile_seq(Sequence(a), func)
      | _ -> Iconst(0)

  member this.compile_seq(seq:expr, fn:LLVMFunction) = 
    match seq with
      | Sequence(a) ->
        let mutable res = Novalue
        for ex in a do
          res <- this.compile_expr(ex.value, fn) 
        res
      | _ -> 
        printfn "I don't know if I can help you"
        Novalue


  member this.compile_mainseq(seq:expr, fn:LLVMFunction) = 
    match seq with
      | Sequence(a) ->
        let mutable res = Novalue
        for ex in a do
          //printfn "Compilation Successful"
          res <- this.compile_expr(ex.value, fn) 
        this.program.functions.Add(fn)
        res
      | _ -> 
        printfn "I don't know if I can help you"
        Novalue

  member this.compile_program(mainseq:expr) = 
    match mainseq with 
      | Sequence(a) ->
        let ptype = this.symbol_table.infer_type(mainseq,0)
        if ptype = LLuntypable then
          sprintf "Program failed to type check. No output produced\n"
        else 
          this.gindex <- this.symbol_table.global_index + 1 // NOT SURE but will fix issue
          this.program.add_preamble("target triple = \"x86_64-pc-linux-gnu\"") 
          this.program.add_preamble("declare void @lambda7c_printint(i32)")
          this.program.add_preamble("declare void @lambda7c_printfloat(double)")
          this.program.add_preamble("declare void @lambda7c_printstr(i8*)")
          let mutable mainfunc:LLVMFunction = {
            name = "main"
            formal_args = Vec()
            return_type = Basic("i32")
            body = Vec()
            attributes = Vec()
          }
          mainfunc.new_bb("beginmain") //|> ignore
    

          let mainres = this.compile_mainseq(mainseq, mainfunc)
          let ret:Instruction = Ret(Basic("i32"),Iconst(0))
          mainfunc.add_inst(ret)
          
          this.program.to_string()
      | _ -> sprintf "Expected to Compile Sequence but found %A" mainseq
          

// http://cs.hofstra.edu/~cscccl/csc124/fall22/test0.ll 
// shows output of Liang's compiler

// ifelse and display need lbox


let compile(trace) = 
  let res = parse(trace)
  match res with 
    | Some(a) -> 
      let complr:LLVMCompiler = {
        symbol_table = {
          SymbolTable.current_frame = {
            table_frame.name = "global"
            entries = HashMap<string,table_entry>()
            parent_scope = None
          }
          global_index = 0
          frame_hash = HashMap<(int*int),table_frame>()
        } 
        program = {
          preamble = ""
          global_declarations = Vec()
          functions = Vec()
          postamble = ""
        }
        gindex = 0
        lindex = 0
      }
      let gen_code = complr.compile_program(a)
      printfn "%s" gen_code

    | None -> printfn "CANNOT COMPILE ---- FAILED TO PARSE"

//compile(true)
compile(false)
