target triple = "x86_64-pc-linux-gnu"target triple = "x86_64-pc-linux-gnu"

define i32 @main() {
beginmain:
%r_0 = icmp sgt i32 1, 2
br il %r_1, label %iftrue_2, label %iffalse_3
iftrue_2:
br label %endif_4
iffalse_3:
br label %endif_4
endif_4:
%r_5 = phi i32 [4, %iftrue_2], [5, %iffalse_3]
ret 0 i32
}

