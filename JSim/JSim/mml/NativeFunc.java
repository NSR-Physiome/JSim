/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// native library based external function

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.PrintStream;
import java.lang.reflect.*;
import java.util.ArrayList;

public class NativeFunc extends CommonFunc {

	// constructor
	public NativeFunc(String n, int dtype, StringList pstr) 
	throws Xcept {
	    super(n, NATIVE, dtype, pstr);
	}

	// add code
	public void addCode(String n, String s) throws Xcept {
	    if (n.equals("language"))
		setLang(s);	
	    else if (n.equals("library")) { 
		libName = s;
		validateName(s);
	    } else if (n.equals("name")) { 
		funcName = s;
		validateName(s);
	    } else throw new Xcept(this,
		"unrecognized keyword \"" + n + "\"");
	}

	// set language code
	private void setLang(String s) throws Xcept {
	    int l = UNDEFINED;
	    if (s.equals("C"))
		l = C;
	    else throw new Xcept(this,
		"Language \"" + s + 
		"\" not supported for native functions");
	    if (lang() != UNDEFINED && lang() != l) throw new Xcept(this,
		"ambiguous language specification");
	    setLang(l); 
	}

	// validate
	public void validate() throws Xcept {
	    if (lang() == UNDEFINED) throw new Xcept(this,
		"language undefined");
	    if (libName == null) throw new Xcept(this,
		"library undefined");
	    if (funcName == null) throw new Xcept(this,
		"name undefined");
	}

	// same function definition?
	public boolean sameAs(XFunc f) {
	    if (! (f instanceof NativeFunc)) return false;
	    NativeFunc f1 = (NativeFunc) f;
	    if (! sameStr(libName, f1.libName)) return false;
	    if (! sameStr(funcName, f1.funcName)) return false;
	    return super.sameAs(f1);
	}

	// validate name
	public void validateName(String s) throws Xcept {
	    if (! Util.onlyLettersAndDigits(s))
		throw new Xcept(this, "name \"" + s + 
		    "\" may contain only alphanumerics");
	    Comp.validateName(this, s);
	}

	// write flat 
	public void writeFlat(PrintStream out) {
	    out.println(flatHdr());
	    out.println("\tlanguage=\"" + langName() + "\";");
	    out.println("\tlibrary=\"" + libName + "\";");
	    out.println("\tname=\"" + funcName + "\";");
	    out.println("}");
	}

}
