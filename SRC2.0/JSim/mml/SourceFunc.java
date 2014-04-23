/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// source-code based external function

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.PrintStream;
import java.lang.reflect.*;
import java.util.ArrayList;

public class SourceFunc extends CommonFunc {
	private String topCode; // initializing source code
	private String mainCode; // main routine source code
	private String bottomCode; // sub-routine source code
	private String flags; // compiler flags
	private String library;  // assigned native library 
	private String name;	// assigned function name in library

	// constructor
	public SourceFunc(String n, int dtype, StringList pstr) 
	throws Xcept {
	    super(n, SOURCE, dtype, pstr);
	}

	// add code
	public void addCode(String n, String code) throws Xcept {
	    if (n.equals("language"))
		setLang(code);	
	    else if (n.equals("topcode")) 
		topCode = code;
	    else if (n.equals("maincode")) 
		mainCode = code;
	    else if (n.equals("bottomcode")) 
		bottomCode = code;
	    else if (n.equals("flags")) 
		flags = code;
	    else throw new Xcept(this,
		"unrecognized keyword \"" + n + "\"");
	}

	// set language code
	private void setLang(String s) throws Xcept {
	    int l = UNDEFINED;
	    if (s.equals("java"))
		l = JAVA;
	    else if (s.equals("C"))
		l = C;
	    else throw new Xcept(this,
		"Language \"" + s + 
		"\" not supported");
	    if (lang() != UNDEFINED && lang() != l) throw new Xcept(this,
		"ambiguous language specification");
	    setLang(l); 
	}

	// validate
	public void validate() throws Xcept {
	    if (lang() == UNDEFINED) throw new Xcept(this,
		"language undefined");
	    if (mainCode == null) throw new Xcept(this,
		"maincode undefined");
	    if (lang() == JAVA && flags != null) throw new Xcept(this,
		"java compiler flags are not supported");
	}

	// query
	public String topCode() { return topCode; }
	public String mainCode() { return mainCode; }
	public String bottomCode() { return bottomCode; }
	public String flags() { return flags; }

	// same function definition?
	public boolean sameAs(XFunc f) {
	    if (! (f instanceof SourceFunc)) return false;
	    SourceFunc f1 = (SourceFunc) f;
	    if (! sameStr(topCode, f1.topCode)) return false;
	    if (! sameStr(mainCode, f1.mainCode)) return false;
	    if (! sameStr(bottomCode, f1.bottomCode)) return false;
	    if (! sameStr(flags,  f1.flags)) return false;
	    return super.sameAs(f1);
	}

	// set native library and function name
	public void setLibrary(String l, String f) {
	    libName = l;
	    funcName = f;
	}

	// write flat 
	public void writeFlat(PrintStream out) {
	    out.println(flatHdr());
	    out.println("\tlanguage=\"" + langName() + "\";");
	    if (flags != null)
	        out.println("\tflags=\"" + flags + "\";");
	    if (topCode != null)
	    	out.println("\ttopcode={{" + topCode + "}};");
	    out.println("\tmaincode={{" + mainCode + "}};");
	    if (bottomCode != null)
	    	out.println("\tbottomcode={{" + bottomCode + "}};");
	    out.println("}");
	}

}
