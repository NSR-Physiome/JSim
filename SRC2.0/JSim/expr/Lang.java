/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// computer language text generator e.g. jsim, java, C, FORTRAN

package JSim.expr;
import JSim.util.*;

abstract public class Lang {
	public String[] chars;  // operator or function chars
	public boolean[] func;  // functional form
	public String dotStr;	
	public String derivStr;
	public char[] dotChars;	  // dot (sub-component) separator
	public char[] derivChars;	  // deriv (:) separator

	// constructor
	public Lang(String dot, String deriv) {
	    chars = new String[IExpr.OPMAX];
	    func = new boolean[IExpr.OPMAX];
	    dotStr = dot;
	    derivStr = deriv;
	    dotChars = dot.toCharArray();
	    derivChars = deriv.toCharArray();
	}

	// add ops to table
	public void addop(int i, String c, boolean f) {
	    chars[i] = c;
	    func[i] = f;
	}
	public void addop(int i, String chars) {
	    int ct = IExpr.opct(i);
	    boolean func = !( ct == 2 && 
		!Character.isLetter(chars.charAt(0)));
	    addop(i, chars, func);
	}

	// query
	public String chars(int i) { return chars[i]; }
	public boolean func(int i) { return func[i]; }

	// get op for name
	public int getOp(String n) {
	    for (int i=0; i<chars.length; i++) {
		String s = chars[i];
		if (s == null) continue;
		if (s.equals(n)) return i;
	    }
	    return 0;
	}

	// adjust named entity for this language
	public String newName(String name) {
	    char[] narr = name.toCharArray();
	    int mct = dotChars.length;
	    if (mct<derivChars.length) mct = derivChars.length;
	    mct*=narr.length;
	    char[] arr = new char[mct];
	    int j=0;
	    char[] tarr = new char[1];
	    char[] add;
	    for (int i=0; i<narr.length; i++) {
		char ch = narr[i];
		switch (ch) {
		case ':':  
		    add = derivChars; 
		    break;
		case '.':  
		    add = dotChars; 
		    break;
	        default: 
		    tarr[0] = ch; 
		    add = tarr; 
		    break;
		}  
		for (int k=0; k<add.length; k++)
		    arr[j++] = add[k];
	    }	
	    return new String(arr, 0, j);
	}
	    
	// cast one type to another
	public String castString(Expr a, String as) {
	    return as;
	}

	// if statement
	abstract public String ifStr(String as, String bs, String cs);

	// swap 1st & 2nd arguments for toString (e.g. atan)
	abstract public boolean swapXY(int op);

	// indent # spaces
	public int indent(int i) { return 8 + 4*i; }
}

