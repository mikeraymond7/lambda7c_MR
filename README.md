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
1. ```bash ./recompile\_dll.sh```  --> also generates .exe to run most recent addition to scope
OR 
2. ```bash ./compile\_X``` where **X** is the name of the file to recompile

## Currently Unimplemented:
- Vectors
