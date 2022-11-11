module llcompiler
open MichaeLL;
open lambda7c;
open MikeCheck;
open System;
open Option;
open System.Collections.Generic;

// http://cs.hofstra.edu/~cscccl/csc124/fall22/test0.ll 
// shows output of Liang's compiler

// ifelse and display need lbox

let translatetype(ty:lltype) = // returns LLVMtype
  match ty with
    | _ -> Iconst(0)

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
      | Strlit(s) -> Sconst(s)
//      | Var(s) ->
//      | TypedVar(ty,s) ->
//      | Nil ->
//      | Binop(s,a,b) -> 
//      | Uniop(s,a) ->
      | Ifelse(bl,t,f) -> 
          let cdest = this.compile_expr(bl, func)
          let ccast = this.newid("r")
          func.add_inst(Cast(ccast,"trunc",Basic("i32"),cdest,Basic("i1")));
          let label1 = this.newid("iftrue")
          let label0 = this.newid("iffalse")
          let endif = this.newid("endif")
          let brinst = Bri1(register(ccast),label1,label0)
          let predlabel = func.currentBBlabel()
          func.add_inst(brinst) // close block
          func.new_bb(label1) // open BB1
          let dest1 = this.compile_expr(t,func)
          let realabel1 = func.currentBBlabel()
          func.add_inst(Br_uc(endif)) // close BB1
          func.new_bb(label0) // open BB0
          let dest0 = this.compile_expr(f,func)
          let realabel0 = func.currentBBlabel()
          func.add_inst(Br_uc(endif)) // close BB0
          func.new_bb(endif) // open newBB
          let desttype = translate_type(this.symbol_table.check_type(t,0))
 
          if desttype <> Void_t then 
            let fdest = this.newid("r")
            let phiinst = Phi2(fdest,desttype,realabel1,dest0,realabel0)
            func.add_inst(phiinst)
            Register(fdest)
          else 
            Novalue 
         
//      | Whileloop(bl,seq) ->
//      | Define(sbox,a) ->
//      | TypedDefine(tsbox,a) ->
//      | Lambda -> Iconst(1)
//      | TypedLambda -> Iconst(1)
//      | Let(sbox,a,bbox) -> Iconst(1)
//      | TypedLet(tsbox,a,bbox) -> Iconst(1)
//      | Setq(sbox,a) -> 
//      | Sequence(abox::b) -> 
      | _ -> Iconst(0)
