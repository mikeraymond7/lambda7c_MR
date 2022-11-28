
target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
@.str_1 = private unnamed_addr constant [14 x i8] c"HERHKLJFK\n\00", align 1
define i32 @main() {

beginmain:
  call void @lambda7c_printstr(i8* @.str_1)
  ret i32 0
}

