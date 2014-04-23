/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration item

package JSim.xsim;

import java.util.ArrayList;
import JSim.util.*;

abstract public class CFItem {
	public CF cf;

	// constructor
	public CFItem(CF c) {
	    cf = c;
	    cf.items.add(this);
	}

	// set attribute
	public void set(String key) {
	    set(key, null);
	}
	public void set(String key, String value) {
	    // nothing by default
	}
	public void setList(String key, StringList value) {
	    // nothing by default
	}

	// post parser processing
	protected void post() {
	    // nothing by default
	}

	// to integer
	public int toInt(String s) {
	    try {
	        return Util.toInt(s);
	    } catch (Xcept e) {
		error("Error parsing expected integer");
	    }
	    return 0;
	}

	// safe name for variable or page
	protected static String safeName(String n) {
	    StringBuffer buf = new StringBuffer();
	    for (int i=0; i<n.length(); i++) {
		char c = n.charAt(i);
		if (! Character.isLetterOrDigit(c))
		    c = '_';
		buf.append(c);
	    }
	    n = buf.toString();
	    return n;
	}   

	// text attribute
	public static String textAttr(String s) {
	    StringBuffer buf = new StringBuffer(" text=\"");
	    for (int i=0; i<s.length(); i++) {
		char c = s.charAt(i);
		switch (c) {
		case '"': buf.append("&quot;"); break;
		case '&': buf.append("&amp;"); break;
		default: buf.append(c); break;
		}
	    }
	    buf.append('"');
	    return buf.toString();
	}		

	// messages
	public void println(String msg) { cf.println(msg); }
	public void error(String msg) { cf.error(msg); }
	public void warn(String msg) { cf.warn(msg); }

	// write MML/RTML
	public void writeMMLVar() { }
	public void writeMMLEqn() { }
	public void writeRTML() { }

	// CFItem.List
	public static class List extends ArrayList<CFItem> {
	    public List() { super(); }
	    public CFItem item(int i) { 
		return (CFItem) get(i);
	    }
	    public CFGroupItem gitem(int i) { 
		return (CFGroupItem) get(i);
	    }
	}
}

