/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// scalar choice variable

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

public class ChoiceNVar extends IntNVar {
	private StringList labels;
	private StringList values;
	private static String argErr = 
	    "choice variables support only string & integer arguments";
	
	// constructor
	public ChoiceNVar(Comp p, String n, Expr.List d) 
	throws Xcept {
	    super(p, n, null);
	    args = d;
	    labels = new StringList(d.size());
	    values = new StringList(d.size());
	    int vbase = 1;
	    for (int i=0; i<d.size(); i++) {
		Expr a = d.expr(i).simplify();
		if (a instanceof StringConst) {
		    String s = ((StringConst) a).constStringVal();
		    if (labels.containSame(s)) 
			throw new Xcept(this,
		            "duplicate choice label \"" + s + "\"");
		    labels.add(s);
		    String v = "" + vbase++;
		    if (values.containSame(v)) 
			throw new Xcept(this,
		            "duplicate choice value \"" + v + "\"");
		    values.add(v);
		} else if (a instanceof RealConst) {
		    double ar = ((RealConst) a).constRealVal();
		    vbase = (int) ar;
	 	    if (vbase != ar) throw new Xcept(this, argErr);
		} else 
		    throw new Xcept(this, argErr);
	    }
	    if (labels.size() < 2) throw new Xcept(this,
		"choice variables require at least 2 labels");
	}

	// query
	public StringList labels() { return labels; }
	public StringList values() { return values; }
}

