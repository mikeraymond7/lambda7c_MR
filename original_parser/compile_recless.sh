#!
#fsharpc original_working_parser.fs -r schemer_lex.dll -r absLexer.dll -r recless.dll
fsharpc recless.fs -r schemer_lex.dll -r absLexer.dll -r recless.dll
