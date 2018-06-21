/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// .java file initial declarations
package JSim.mml;

import JSim.util.*;
import JSim.expr.*;
import java_cup.runtime.Symbol;

import java.util.Hashtable;

%%
%class ModelScanner
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

	Hashtable<Integer,String> commentsHT = new Hashtable<Integer,String>(); // Contains all of the comments and associated line numbers.
	public Hashtable<Integer,String> getComments() { return commentsHT; }  
	int lastSemiColLineNumb; // line number of last semicolon scanned;
	public int getSemiColLineNumb() { return lastSemiColLineNumb; }
%}

    LineTerminator = \r|\n|\r\n
    InputCharacter = [^\r\n]
    WhiteSpace     = {LineTerminator} | [ \t\f]
    EndOfLineComment     = "//" {InputCharacter}* {LineTerminator}?
 
%eofval{
	return new Symbol(ModelParserSym.EOF);
%eofval}
 
%yylexthrow{
	Xcept
%yylexthrow}

Identifier = ([A-Za-z][0-9A-Za-z_]*".")*[A-Za-z][0-9A-Za-z_]* 

%%
"{{" ~"}}" { /* double brace enclosed code blocks */
	   String s = tokText();
	   s = s.substring(2, s.length()-2);
	   return new Symbol(ModelParserSym.STRING, s);
	}
// "//".*\n { /* ignore Unix comments */ }

{EndOfLineComment} {String s = tokText();s = s.substring(2, s.length()); 
 		  if(commentsHT == null){
			commentsHT = new Hashtable<Integer,String>();
		  }
		  commentsHT.put(this.getLineNo(),s.trim()); 
}
 
"//".*\r { String s = tokText();s = s.substring(2, s.length()); 
		  if(commentsHT == null){
			commentsHT = new Hashtable<Integer,String>();
		  }
		  commentsHT.put(this.getLineNo(),s.trim());		
  }
"/*" ~"*/" {String s = tokText(); s = s.substring(2, s.length()-2);   
		  if(commentsHT == null){
			commentsHT = new Hashtable<Integer,String>();
		  }
		  commentsHT.put(this.getLineNo(),s.trim());

}
 
"JSim" { return new Symbol(ModelParserSym.JSIM); }
"unit" { return new Symbol(ModelParserSym.UNIT); }
"fundamental" { return new Symbol(ModelParserSym.FUNDAMENTAL); }
"prefix" { return new Symbol(ModelParserSym.PREFIX); }
"prefixable" { return new Symbol(ModelParserSym.PREFIXABLE); }
"conversion" { return new Symbol(ModelParserSym.CONVERSION); }
"property" { return new Symbol(ModelParserSym.PROPERTY); }
"class" { return new Symbol(ModelParserSym.CLASS); }
"override" { return new Symbol(ModelParserSym.OVERRIDE); }
"public" { return new Symbol(ModelParserSym.PUBLIC); }
"private" { return new Symbol(ModelParserSym.PRIVATE); }
"extern" { return new Symbol(ModelParserSym.EXTERN); }
"template" { return new Symbol(ModelParserSym.TEMPLATE); }
"event" { return new Symbol(ModelParserSym.EVENT); }
"function" { return new Symbol(ModelParserSym.FUNCTION); }
"procedure" { return new Symbol(ModelParserSym.PROCEDURE); }
"native" { return new Symbol(ModelParserSym.NATIVE); }
"source" { return new Symbol(ModelParserSym.SOURCE); }
"when" { return new Symbol(ModelParserSym.WHEN); }
"import" { return new Symbol(ModelParserSym.IMPORT); }
"integral" { return new Symbol(ModelParserSym.INTEGRAL); }
"sum" { return new Symbol(ModelParserSym.SUM); }
"to" { return new Symbol(ModelParserSym.TO); }
"antimony" { return new Symbol(ModelParserSym.ANTIMONY); }

v[0-9]+"."[0-9]+ { return new Symbol(ModelParserSym.VERSION, tokText()); }

\"[^\"]*\" { String s = tokText();
	   s = s.substring(1, s.length()-1);
	   return new Symbol(ModelParserSym.STRING, s); 
	}

"assign" { throw new Xcept("Keyword reserved for future use"); }
"solver" { throw new Xcept("Keyword reserved for future use"); }

"PI"     { return new Symbol(ModelParserSym.PI, tokText()); }
"true"   { return new Symbol(ExprParserSym.TRUE, tokText()); }
"false"  { return new Symbol(ExprParserSym.FALSE, tokText()); }
"="      { return new Symbol(ModelParserSym.EQ, tokText()); }
"<>"     { return new Symbol(ModelParserSym.NE, tokText()); }
"~="     { return new Symbol(ModelParserSym.APPROX, tokText()); }
"=~"     { return new Symbol(ModelParserSym.APPROX, tokText()); }
"<"      { return new Symbol(ModelParserSym.LT, tokText()); }
">"      { return new Symbol(ModelParserSym.GT, tokText()); }
"<="     { return new Symbol(ModelParserSym.LE, tokText()); }
">="     { return new Symbol(ModelParserSym.GE, tokText()); }
"if"     { return new Symbol(ModelParserSym.IF, tokText()); }
"else"   { return new Symbol(ModelParserSym.ELSE, tokText()); }
"and"    { return new Symbol(ModelParserSym.AND, tokText()); }
"or"     { return new Symbol(ModelParserSym.OR, tokText()); }

//([A-Za-z][0-9A-Za-z_]*".")*[A-Za-z][0-9A-Za-z_]* { return new Symbol(ModelParserSym.IDENT, tokText()); }

{Identifier} { String s = tokText(); return new Symbol(ModelParserSym.IDENT, tokText()); }

([0-9]+)?"."?[0-9]+([Ee]([+-])?[0-9]+)? { return new Symbol(ModelParserSym.NUMBER, tokText()); }
[0-9]+"."?([0-9]+)?([Ee]([+-])?[0-9]+)? { return new Symbol(ModelParserSym.NUMBER, tokText()); }

"+" { return new Symbol(ModelParserSym.PLUS); }
"-" { return new Symbol(ModelParserSym.MINUS); }
"^" { return new Symbol(ModelParserSym.POWER); }
"*" { return new Symbol(ModelParserSym.TIMES); }
"/" { return new Symbol(ModelParserSym.DIVIDE); }
"@" { return new Symbol(ModelParserSym.FORALL); }

"(" { String s = tokText(); 
      return new Symbol(ModelParserSym.LPAREN); }
")" { return new Symbol(ModelParserSym.RPAREN); }
"{" { return new Symbol(ModelParserSym.LBRACE); }
"}" { return new Symbol(ModelParserSym.RBRACE); }
";" { this.lastSemiColLineNumb = this.getLineNo();  // pass in line number here 
      return new Symbol(ModelParserSym.SEMI); }
"," { return new Symbol(ModelParserSym.COMMA); }
":" { return new Symbol(ModelParserSym.DERIV); }

[ \t\r\f\n] { /* ignore white space. */ }
. { lexError(); }
