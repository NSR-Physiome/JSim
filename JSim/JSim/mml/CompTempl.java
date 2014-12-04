/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Component extension template

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.PrintStream;

public class CompTempl extends Templ {
	Templ parent;	// extends this template
	Comp comp;	// component to copy
	StringList parms; // parameter names

	// constructor
	public CompTempl(Model m, String n, String tname, StringList p) throws Xcept {
	    super(m, n);
	    parms = (p==null) ? new StringList(1) : p;

	    // find template
	    parent = m.getTempl(tname);

	    // create accumulation component
	    Templ jtempl = parent;
//	    while (jtempl instanceof CompTempl) 
//		jtempl=((CompTempl) jtempl).parent;
	    comp = jtempl.createComp(null, n + ".template", null);
	    comp.templ = this;
	}

	// blank constructor for clone() method below
	private CompTempl(Model m, String n) throws Xcept {
	    super(m, n);
	}

	// get Java class
	public Class clss() { return parent.clss(); }

	// methods for import not implemented yet
	public boolean isCompatible(Templ t) throws Xcept { 
	    throw new Xcept(this, 
		"Cannot yet import component templates");
	}

	// clone this template in new model (for import)
	public Templ clone(Model m) throws Xcept {
	    CompTempl templ = new CompTempl(m, name);
	    templ.parent = parent;
	    templ.comp = comp;	// not very safe
	    templ.parms = parms;
	    return templ;
	} 

	// dump contents
	public void dump(PrintStream out, String indent) throws Xcept {
	    out.println(indent + parent.name + " template " + name);
	    comp.dump(out, indent + "  ");
	}

	// create component
	public Comp createComp(Comp p, String n, Expr.List args) 
	throws Xcept {
	    Context ctxt = new CompContext(JSLang.lang, model);
	    Util.verbose(name() + ".createComp(" + p + ", " + n +
		", " + args + ")");
	    Comp c = parent.createComp(p, n, args); // parent always non-null for CompTempl 
	    int ct = (args==null) ? 0 : args.size();
	    if (parms.size() != ct) throw new Xcept(this,
		"Template requires exactly " + parms.size() + " parameters");
	    Util.verbose("  " + name() + " comp.child=" + comp.child);

	    // create replacement lists for parms
	    Expr.List list1 = new Expr.List(comp.child.size());
	    Expr.List list2 = new Expr.List(comp.child.size());
	    for (int i=0; i<parms.size(); i++) {
		String n1 = parms.str(i);
		Comp c1 = comp.getChild(n1);
		if (c1 == null) throw new Xcept(this, 
		    "Template does not define parameter " + n1);
		if (c1.builtin) throw new Xcept(this, 
		    "Template parameter " + n1 + " conflicts with built-in");
		if (parms.containSame(c1.name())) {
		    list1.add(c1);
		    list2.add(args.expr(i));
		    continue;
		}
	    }

	    // clone non-parm children
	    for (int i=0; i<comp.child.size(); i++) {
		Comp c1 = comp.child.comp(i);
		if (parms.containSame(c1.name())) continue;
		if (c1.builtin) {
		    Comp c2 = c.getChild(c1.name());
		    if (c2 != null) {
			list1.add(c1);
			list2.add(c2);
		    }
		    continue;
		}
		Util.verbose("  " + name() + " clone " + c1.toString(ctxt) + 
		    " inside " + c.toString(ctxt));

		// clone c1 inside c with replacements
	    	if (c1.templ == null) throw new Xcept(c1,
		"Cannot find clone template");
	    	Expr.List nargs = c1.args;
	    	if (nargs != null && nargs.size()>0) {
		    nargs = new Expr.List(c1.args.size());
		    for (int j=0; j<c1.args.size(); j++) {
			Expr e = c1.args.expr(j).replace(list1, list2);
		 	nargs.add(e);
		    }
		}
	    	Comp c2 = c1.templ.createComp(c, c1.name(), nargs);
	    	c2.access = c1.access;
	    	if (c1.hasUnit()) c2.setUnit(c1.unit());
		Util.verbose("  " + name() + " result " + c2.toString(ctxt));	

		// update lists
		list1.add(c1);
		list2.add(c2);
	    }
	    Util.verbose("  " + name() + " parms=" + parms + " comp.child=" + comp.child +
		" list1=" + list1 + " list2=" + list2);

	    // clone eqns
	    for (int i=0; i<comp.eqn.size(); i++) {
		Eqn e1 = comp.eqn.eqn(i);
		if (e1.builtin) continue;
		Util.verbose("  " + name() + " clone " + e1.toString(ctxt) + 
		    " inside " + c.toString(ctxt));
		Expr sdom = e1.sdom.expr.replace(list1, list2);
		Expr lhs = e1.lhs.replace(list1, list2);
		Expr rhs = e1.rhs.replace(list1, list2);
		Eqn eqn = new Eqn(c, sdom, lhs, e1.op, rhs); 
		Util.verbose("  " + name() + " result " + eqn.toString(ctxt));	
	    }
	    return c;
	}
}
