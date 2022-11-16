(0) METASTART --> Program EOF 
(1) POpt --> Program 
(2) POpt --> 
(3) Expr --> GetBin 
(4) Expr --> Uniop Axpr 
(5) Expr --> if ( GetBin ) Axpr Axpr 
(6) Expr --> while ( GetBin ) ( BeginSeq ) 
(7) Expr --> DEF Var Axpr 
(8) Expr --> setq Var Axpr 
(9) Expr --> BeginSeq 
(10) Expr --> vec_setq Var integer Axpr 
(11) Expr --> vec_get Var integer 
(12) Expr --> vec_make integer integer 
(13) Axpr --> ( Expr ) 
(14) Expr --> SeqExpr 
(15) Program --> Axpr POpt 
(16) Expr --> LET ( Var Axpr ) Axpr 
(17) Expr --> lambda ( SeqVar ) typeopt Axpr 
(18) BeginSeq --> begin SeqExpr 
(19) Var --> var typeopt 
(20) typeopt --> COLON Txpr 
(21) typeopt --> 
(22) Txpr --> float 
(23) Txpr --> int 
(24) Txpr --> string 
(25) SeqVar --> Var VarOpt 
(26) VarOpt --> SeqVar 
(27) VarOpt --> 
(28) SeqExpr --> Axpr SeqOpt 
(29) SeqOpt --> SeqExpr 
(30) SeqOpt --> 
(31) Axpr --> Var 
(32) Axpr --> integer 
(33) Axpr --> str 
(34) Axpr --> flt 
(35) GetBin --> Binop Axpr Axpr 
(36) Binop --> + 
(37) Binop --> - 
(38) Binop --> * 
(39) Binop --> DIV 
(40) Binop --> > 
(41) Binop --> < 
(42) Binop --> = 
(43) Binop --> >= 
(44) Binop --> % 
(45) Binop --> neq 
(46) Binop --> eq 
(47) Binop --> and 
(48) Binop --> or 
(49) Binop --> ^ 
(50) Uniop --> NEG 
(51) Uniop --> car 
(52) Uniop --> cdr 
(53) Uniop --> not 
(54) Uniop --> display 
Enter Expression: 