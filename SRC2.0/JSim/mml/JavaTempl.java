/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// java class template

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.lang.reflect.*;
import java.io.PrintStream;

public class JavaTempl extends Templ {
	protected Class<?> clss;	// java class
	protected Constructor con;// java constructor for child components

	// constructor
	public JavaTempl(Model m, String n, String cname) throws Xcept {
	    super(m, n);

	    // find class
	    try {
	    	clss = Class.forName(cname);
	    } catch (ClassNotFoundException e) {
		throw new Xcept(this, "Class not found: " + cname);
	    }

	    // find constructor
	    Class[] args = new Class[3];
	    args[0] = Comp.class;
	    args[1] = String.class;
	    args[2] = Expr.List.class;	
	    try {
	    	con = clss.getConstructor(args);
	    } catch (Exception e) {
		throw new Xcept(
		"No appropriate java constructor found for " + clss.getName());
	    }
	}

	// java class
	public Class clss() { return clss; }

	// test whether compatible with another template
	public boolean isCompatible(Templ t) throws Xcept {
	    if (! JavaTempl.class.isInstance(t)) return false;
	    JavaTempl jt = (JavaTempl) t;
	    return (jt.clss == clss);
	}

	// clone (use same object)
	public Templ clone(Model m) throws Xcept { 
	    m.registerTempl(this);
	    return this; 
	}	

	// dump contents
	public void dump(PrintStream out, String indent)
	throws Xcept {
	    out.println(indent + "java template " + name + 
		" class=" + clss.getName());
	}

	// create Component for this template
	public Comp createComp(Comp p, String n, Expr.List args) 
	throws Xcept {
	    if (con == null) throw new Xcept(this,
		"Cannot find child component java constructor");

	    Object obj[] = new Object[3];
	    obj[0] = p;
	    obj[1] = n;
	    obj[2] = args;

	    try {
		Comp c = (Comp) con.newInstance(obj);
		c.templ = this;
		return c;
	    } catch (InvocationTargetException e) {
		throw (Xcept) e.getTargetException();
	    } catch (Exception e) {
		throw new Xcept(
		"Error instantiating " + clss.getName() + 
		": " + e.toString());
	    }

	}
}
