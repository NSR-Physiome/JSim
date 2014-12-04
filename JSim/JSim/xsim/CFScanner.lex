/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XSim configuration file scanner
package JSim.xsim;

import JSim.util.*;
import java_cup.runtime.Symbol;

%%
%class CFScanner
%cup
%line
%char
%{
	// code to include in Yylex class
	public int getLineNo() { return yyline+1; }
	public int getCharNo() { return yychar; }
	public String tokText() { return new String(yytext()); }
	public void lexError() throws Xcept {
	    throw new Xcept("Illegal character");
	}
%}

%eofval{
	return new Symbol(CFParserSym.EOF);
%eofval}
 
%yylexthrow{
	Xcept
%yylexthrow}

%%

"model" { return new Symbol(CFParserSym.MODEL); }
"MODEL" { return new Symbol(CFParserSym.MODEL); }
"Model" { return new Symbol(CFParserSym.MODEL); }
"menu" { return new Symbol(CFParserSym.MENU); }
"MENU" { return new Symbol(CFParserSym.MENU); }
"Menu" { return new Symbol(CFParserSym.MENU); }
"group" { return new Symbol(CFParserSym.GROUP); }
"GROUP" { return new Symbol(CFParserSym.GROUP); }
"Group" { return new Symbol(CFParserSym.GROUP); }
"box" { return new Symbol(CFParserSym.BOX); }
"BOX" { return new Symbol(CFParserSym.BOX); }
"Box" { return new Symbol(CFParserSym.BOX); }

\!.*\n { /* ignore Unix comments */ }
\!.*\r { /* ignore DOS comments */ }

\"[^\"]*\"   { String s = tokText();
	   s = s.substring(1, s.length()-1);
	   return new Symbol(CFParserSym.STRING, s); 
	}
\'[^\']*\'   { String s = tokText();
	   s = s.substring(1, s.length()-1);
	   return new Symbol(CFParserSym.STRING, s); 
	}

([A-Za-z_][0-9A-Za-z_]*".")*[A-Za-z_][0-9A-Za-z_]* { return new Symbol(CFParserSym.IDENT, tokText()); }
([0-9]+)?"."?[0-9]+([Ee]([+-])?[0-9]+)? { return new Symbol(CFParserSym.NUMBER, tokText()); }
[0-9]+"."?([0-9]+)?([Ee]([+-])?[0-9]+)? { return new Symbol(CFParserSym.NUMBER, tokText()); }

"=" { return new Symbol(CFParserSym.EQ, tokText()); }
"+" { return new Symbol(CFParserSym.PLUS, tokText()); }
"-" { return new Symbol(CFParserSym.MINUS, tokText()); }
"*" { return new Symbol(CFParserSym.TIMES, tokText()); }
"/" { return new Symbol(CFParserSym.DIVIDE, tokText()); }

"(" { return new Symbol(CFParserSym.LPAREN, tokText()); }
")" { return new Symbol(CFParserSym.RPAREN, tokText()); }
"{" { return new Symbol(CFParserSym.LPAREN, tokText()); }
"}" { return new Symbol(CFParserSym.RPAREN, tokText()); }
"," { return new Symbol(CFParserSym.COMMA, tokText()); }

[ \t\r\f\n] { /* ignore white space. */ }
. { lexError(); }
