/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// .java file initial declarations
package JSim.expr;
import JSim.util.*;
import java_cup.runtime.Symbol;

%%
%class ExprScanner
%cup
%line
%char
%{
	// code to include in Yylex class
	public int line() { return yyline+1; }
	public String tokText() { return new String(yytext()); }
	public void lexError() {
	}
%}

%eofval{
	return new Symbol(ExprParserSym.EOF);
%eofval}
 
%yylexthrow{
	Xcept
%yylexthrow}


%%
"PI"    { return new Symbol(ExprParserSym.PI, tokText()); }
"true"  { return new Symbol(ExprParserSym.TRUE, tokText()); }
"false" { return new Symbol(ExprParserSym.FALSE, tokText()); }
"="     { return new Symbol(ExprParserSym.EQ, tokText()); }
"<>"    { return new Symbol(ExprParserSym.NE, tokText()); }
"<"     { return new Symbol(ExprParserSym.LT, tokText()); }
">"     { return new Symbol(ExprParserSym.GT, tokText()); }
"<="    { return new Symbol(ExprParserSym.LE, tokText()); }
">="    { return new Symbol(ExprParserSym.GE, tokText()); }
"if"    { return new Symbol(ExprParserSym.IF, tokText()); }
"else"  { return new Symbol(ExprParserSym.ELSE, tokText()); }
"and"   { return new Symbol(ExprParserSym.AND, tokText()); }
"or"    { return new Symbol(ExprParserSym.OR, tokText()); }

"assign" { throw new Xcept("Keyword reserved for future use"); }
"solver" { throw new Xcept("Keyword reserved for future use"); }

"."?([A-Za-z][0-9A-Za-z_]*".")*[A-Za-z][0-9A-Za-z_]* { return new Symbol(ExprParserSym.IDENT, tokText()); }
([0-9]+)?"."?[0-9]+([Ee]([+-])?[0-9]+)? { return new Symbol(ExprParserSym.NUMBER, tokText()); }
[0-9]+"."?([0-9]+)?([Ee]([+-])?[0-9]+)? { return new Symbol(ExprParserSym.NUMBER, tokText()); }

"+" { return new Symbol(ExprParserSym.PLUS); }
"-" { return new Symbol(ExprParserSym.MINUS); }
"^" { return new Symbol(ExprParserSym.POWER); }
"*" { return new Symbol(ExprParserSym.TIMES); }
"/" { return new Symbol(ExprParserSym.DIVIDE); }

"(" { return new Symbol(ExprParserSym.LPAREN); }
")" { return new Symbol(ExprParserSym.RPAREN); }
"," { return new Symbol(ExprParserSym.COMMA); }
":" { return new Symbol(ExprParserSym.DERIV); }

[ \t\r\f\n] { /* ignore white space. */ }
. { throw new Xcept("Illegal character"); }
