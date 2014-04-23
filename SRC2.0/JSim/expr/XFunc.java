/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// external function

package JSim.expr;
import JSim.util.*;

import java.io.PrintStream;

public abstract class XFunc implements Named, DiagInfo {

	// func types
	public static final int CLASS = 1;
	public static final int NATIVE = 2;
	public static final int SOURCE = 3;

	// language types
	public static final int UNDEFINED = 0;
	public static final int JAVA = 4;
	public static final int C = 5;
	public static final int CC = 6; // C++
	public static final int FORTRAN = 7;
	public static final int MATLAB = 8;
	public static final int OCTAVE = 9;

	// instance variables
	protected String name;	// assigned name
	private int funcType; // see above
	private int lang; // language, see above
	private int lastSeq; // last XFuncCall seq#
	protected String libName; // native library, if any
	protected String funcName; // func name within native library
	private boolean isReentrant; // false unless MML tagged

	// constructor
	public XFunc(String n, int ftype) throws Xcept {
	    name = n;
	    funcType = ftype;
	    lang = UNDEFINED;
	    isReentrant = false;
	}

	// set language/reentrant
	public void setLang(int i) { lang = i; }
	public void setReentrant(boolean b) { isReentrant = b; }
		
	// create XFuncCall
	abstract public XFuncCall createCall(Expr.List list) 
	throws Xcept;

	// validate declaration during parse
	abstract public void validate() throws Xcept;

	// write flat version
	abstract public void writeFlat(PrintStream out) throws Xcept;

	// query
	public String diagInfo() { return "function " + name; }
	public String name() { return name; }
	public int funcType() { return funcType; }
	public int lang() { return lang; }
	public boolean sameAs(XFunc f) {
	    return name.equals(f.name) && 
		funcType == f.funcType &&
		lang == f.lang;
	}
	public boolean needed() { return lastSeq > 0; }
	public int nextSeq() { return ++lastSeq; }
	public boolean isReentrant() { return isReentrant; }
	public String libName() { return libName; }
	public String funcName() { return funcName; }
	public String langName() {
	    switch (lang) {
	    case JAVA : return "java";
	    case C : return "C";
	    case CC : return "C++";
	    case FORTRAN : return "fortran";
	    case MATLAB : return "matlab";
	    case OCTAVE : return "octave";
	    }
	    return "UNDEFINED";
	}

	// same string
	public static boolean sameStr(String s1, String s2) {
	    if (Util.isBlank(s1) && Util.isBlank(s2)) 
		return true;
	    if (s1 == null || s2 == null) return false;
	    return s1.equals(s2);
	}

	// XFunc.NList
	public static class NList extends NamedList {
            public NList(int i) { super(i); }
            public XFunc xfunc(int i) { return (XFunc) get(i); }
            public XFunc xfunc(String n) { return (XFunc) getByName(n); }
        }

}
