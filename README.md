# Lambda7c 
#### Compiler for programming language defined by Dr. Chuck Liang at Hofstra University


## Current Scope:
1. Parser - Complete
2. Type Checker - Complete
3. Code Generation - In-progress  

## Walkthrough:
- Parser --> recless.fs
- Grammar --> michaeLL.fs
- Typechecker --> mikeCheck.fs
- Code Generation / Compiler --> llvmir.fs

#### To compile all files:
1. ``` ./recompile_dll.sh```  --> also generates .exe to run most recent addition to scope
OR
2. ``` ./test3.sh``` --> compiles all .dll's and runs testcases/test3_oneline.7c
  
Note: To run with Tracing, enter llvmir.fs, go to bottom of file, and turn ``` compile(false)``` --> ``` compile(true)```

## Current Higher Level Abilities:
- Recursive Functions

## Currently Unimplemented:
- Vectors (Still not parsing properly)
- Comments 
- Calculation of Closure

## Needs Fix
- Reading Multiline Functions (Reads one line from STDIN)
- TypedDefine code generation for non-functions
