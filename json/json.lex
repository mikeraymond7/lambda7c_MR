//CsLex file generated from grammar json
#pragma warning disable 0414
using System;
using System.Text;

public class jsonlexer<ET> : AbstractLexer<ET>  {
  Yylex lexer;
  ET shared_state;
  public jsonlexer(string n) { lexer = new Yylex(new System.IO.StringReader(n)); }
  public jsonlexer(System.IO.FileStream f) { lexer=new Yylex(f); }
  public RawToken next_lt() => lexer.yylex();
  public void set_shared(ET shared) {shared_state=shared;}
}//lexer class

%%
%namespace Fussless
%type RawToken
%eofval{
  return new RawToken("EOF","EOF",yyline,yychar);
%eofval}  
%{
private static int comment_count = 0;
private static int line_char = 0;
%}
%line
%char
%state COMMENT

ALPHA=[A-Za-z]
DIGIT=[0-9]
DIGITS=[0-9]+
FLOATS = [0-9]*\.[0-9]+([eE]([+-]?){DIGITS})?
HEXDIGITS=(0x)[0-9A-Fa-f]*
NEWLINE=((\r\n)|\n)
NONNEWLINE_WHITE_SPACE_CHAR=[\ \t\b\012]
WHITE_SPACE_CHAR=[{NEWLINE}\ \t\b\012]
STRING_TEXT=(\\\"|[^{NEWLINE}\"]|{WHITE_SPACE_CHAR}+)*
COMMENT_TEXT=([^*/\r\n]|[^*\r\n]"/"[^*\r\n]|[^/\r\n]"*"[^/\r\n]|"*"[^/\r\n]|"/"[^*\r\n])*
ALPHANUM=[A-Za-z_][A-Za-z0-9_]*

%% 
<YYINITIAL> {NEWLINE}+ { line_char = yychar+yytext().Length; return null; }
<YYINITIAL> {NONNEWLINE_WHITE_SPACE_CHAR}+ { return null; }
<YYINITIAL> "{" { return new RawToken("{",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "[" { return new RawToken("[",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "null" { return new RawToken("null",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "-" { return new RawToken("-",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "," { return new RawToken(",",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "true" { return new RawToken("true",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "(" { return new RawToken("(",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "}" { return new RawToken("}",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> ")" { return new RawToken(")",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "]" { return new RawToken("]",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> "false" { return new RawToken("false",yytext(),yyline,yychar-line_char,yychar); }
<YYINITIAL> ":" { return new RawToken(":",yytext(),yyline,yychar-line_char,yychar); }

<YYINITIAL> "//".*\n { line_char=yychar+yytext().Length; return null; }
<YYINITIAL,COMMENT> [(\r\n?|\n)] { line_char=yychar+yytext().Length; return null; }

<YYINITIAL> "/*" { yybegin(COMMENT); comment_count = comment_count + 1; return null;
}
<COMMENT> "/*" { comment_count = comment_count + 1; return null; }
<COMMENT> "*/" { 
	comment_count = comment_count - 1;
	if (comment_count == 0) {
            yybegin(YYINITIAL);
        }
        return null;
}

<COMMENT> {COMMENT_TEXT} { return null; }

<YYINITIAL> \"{STRING_TEXT}\" {
        return new RawToken("StrLit",yytext(),yyline,yychar-line_char,yychar);
}
<YYINITIAL> \"{STRING_TEXT} {
	String str =  yytext().Substring(1,yytext().Length);
	Utility.error(Utility.E_UNCLOSEDSTR);
        return new RawToken("Unclosed String",str,yyline,yychar-line_char,yychar);
}

<YYINITIAL> {DIGIT}+ { 
  return new RawToken("Num",yytext(),yyline,yychar-line_char,yychar);
}
<YYINITIAL> {HEXDIGITS} { 
return new RawToken("Hexnum",yytext(),yyline,yychar-line_char,yychar);  
}
<YYINITIAL> {FLOATS} { 
  return new RawToken("Float",yytext(),yyline,yychar-line_char,yychar);
}	
<YYINITIAL> ({ALPHA}|_)({ALPHA}|{DIGIT}|_)* {
        return new RawToken("Alphanum",yytext(),yyline,yychar-line_char,yychar);
}	
<YYINITIAL,COMMENT> . {
	StringBuilder sb = new StringBuilder("Illegal character: <");
	String s = yytext();
	for (int i = 0; i < s.Length; i++)
	  if (s[i] >= 32)
	    sb.Append(s[i]);
	  else
	    {
	    sb.Append("^");
	    sb.Append(Convert.ToChar(s[i]+'A'-1));
	    }
        sb.Append(">");
	Console.WriteLine(sb.ToString());	
	Utility.error(Utility.E_UNMATCHED);
        return null;
}
