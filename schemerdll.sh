#!
rustlr schemer.grammar -fsharp
lex.exe schemer
mcs /t:library schemer_lex.cs /r:absLexer.dll
