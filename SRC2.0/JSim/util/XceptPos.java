/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.util;

// parser position for diagnosing errors

public class XceptPos {

	public String filename;	// flat or import file name, else null
	public int lineno;	// line # (1st line is 1)
	public int charno;	// char # from start of file
	public String token;	// near this token
	public XceptPos child;	// import, flat(?) or null

	// constructor based on current scanner position
	public XceptPos(String f, int l, int c, String t) {
	    filename = f;
	    lineno = l;
	    charno = c;
	    token = t;
	}

	// diagnostic message
	public String toString() {
	    String s = (filename == null) ? "" : filename;
	    if (lineno > 0) s = s + " near line " + lineno;
//DEBUGONLY    if (charno > 0) s = s + " char " + charno;
	    if (token != null) s = s + " token \"" + token + "\"";
	    if (child != null) 
		s = s + " calls " + child;
	    return s;
	}
	
}
