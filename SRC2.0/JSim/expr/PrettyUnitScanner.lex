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
%class PrettyUnitScanner
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
	return new Symbol(PrettyUnitParserSym.EOF);
%eofval}
 
%yylexthrow{
	Xcept
%yylexthrow}


%%
[A-Za-z][0-9A-Za-z_]* { return new Symbol(PrettyUnitParserSym.NAME, tokText()); }
([0-9]+)?"."?[0-9]+([Ee]([+-])?[0-9]+)? { return new Symbol(PrettyUnitParserSym.NUMBER, tokText()); }
[0-9]+"."?([0-9]+)?([Ee]([+-])?[0-9]+)? { return new Symbol(PrettyUnitParserSym.NUMBER, tokText()); }

"-" { return new Symbol(PrettyUnitParserSym.MINUS); }
"^" { return new Symbol(PrettyUnitParserSym.POWER); }
"*" { return new Symbol(PrettyUnitParserSym.TIMES); }
"/" { return new Symbol(PrettyUnitParserSym.DIVIDE); }

"(" { return new Symbol(PrettyUnitParserSym.LPAREN); }
")" { return new Symbol(PrettyUnitParserSym.RPAREN); }

[ \t\r\f\n] { /* ignore white space. */ }
. { throw new Xcept("Illegal character"); }
