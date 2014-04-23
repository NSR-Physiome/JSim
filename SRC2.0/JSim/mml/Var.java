/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// numeric variable

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.PrintStream;
import java.util.ArrayList;

public abstract class Var extends Comp {
	protected Unit	unit;	// physical units
	protected int derivOrder; // order of derivative

	// 1.6 compiler
	protected int flatInx;	// flat var index (within MathSys)
	public int flatInx() { return flatInx; }

	// 1.7 compiler
	private int hashCode; 
	public int hashCode() { return hashCode; }

	// constructor
	public Var(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    for (int i=0; i<getModel().defProps.size(); i++) {
	    	CompProp prop = getModel().defProps.prop(i);
	    	addProp(prop.name(), prop.dataType());
	    }
	    MathSys topMath = getTopMath();
	    if (topMath != null) {
		hashCode = topMath.vars.size() + 1;
	    	topMath.vars.add(this);
	    } else {
	    	hashCode = super.hashCode();
	    }
	    derivOrder = Util.count(n, ':');
	}

	public int nDeriv() {
	    char[] c = name.toCharArray();
	    int ct = 0;
	    for (int i=0; i<c.length; i++)
		if (c[i] == ':') ct++;
	    return ct;
	}

	// query
	public int dataType() { return REAL; }
	public boolean isVar() { return true; }
	public boolean isInt() { return false; }
	public boolean isState() { return false; }
	public boolean hasUnit() { return true; }
	public Unit unit() { return unit; }
	public void setUnit(Unit u) throws Xcept { 
	    if (u != null && getModel().unitControl == Model.NONE)
	        throw new Xcept("Units not allowed in this model");
	    unit = u; 
	}

	abstract public int ndim();
	abstract public Domain domain(int i); // null if invalid inx
	abstract public boolean hasDomain(Var v);

	// domain list
	public Domain.List domainList() {
	    Domain.List dlist = new Domain.List(ndim());
	    addDomains(dlist);
	    return dlist;
	}

	// add domains to list
	public void addDomains(Expr.List dlist) {
	    for (int i=0; i<ndim(); i++) dlist.addUniq(domain(i));
	}

	// find derivative variable,  create if makeit, otherwise null
	public Var deriv(Var t) throws Xcept { return deriv(t, true); }
	public Var deriv(Var t, boolean makeit) throws Xcept {
	    String n = name + ":" + t.name;
	    Comp c = parent.getChild(n);
	    if (c == null) {
		if (! t.isDomain()) throw new Xcept(this,
		    t.name + " is not domain,  cannot make deriv var");
		return makeit ? createDeriv((Domain) t) : null;
	    }
	    if (!c.isVar()) throw new Xcept(this,
		"Var.deriv() is not Var");
	    return (Var) c;
	}
	
	// other derivative stuff
	public Var createDeriv(Domain t) throws Xcept {
	    throw new Xcept(this, "Cannot create derivative for this variable class");
	}
	public Var unDeriv() throws Xcept {
	    throw new Xcept(this, "no unDeriv() for this var");
	}
	public boolean isDeriv() {
	    return name.indexOf(':') >= 0;
	}
	public Var zeroDeriv() throws Xcept {
	    Var v = this;
	    while (v.isDeriv()) v = v.unDeriv();
	    return v;
	}
	public Domain derivDomain() throws Xcept {
	    throw new Xcept(this, "no derivDomain() for this var");
	}
	public int derivOrder() { return derivOrder; }

	// domain control?
  	public Domain auxForDomain() {
	    if (parent instanceof Domain) 
	    	return (Domain) parent;
	    else
	        return null;
	}

	// write flatmath 
	public void writeFlat(PrintStream out, Context ctxt) 
	throws Xcept {
	    Model m = getModel();
	    String s = null;
	    if (m.realTempl.clss().isInstance(this)) 
		s = "real";
	    else if (m.realDomainTempl.clss().isInstance(this))
		s = "realDomain";
	    else if (m.choiceTempl.clss().isInstance(this)) 
		s = "choice";
	    else if (m.intTempl.clss().isInstance(this)) 
		s = "int";
	    if (isDeriv()) s = null;
	    if (s != null)
		out.println("\t" + accessString() + s + " " + toString(ctxt) +
		((args == null) ? "" : args.toString(ctxt)) + 
		((unit == null) ? "" : (" " + unit.pubName())) +  
		";");
	    for (int i=0; i<getModel().defProps.size(); i++) {
	    	String pname = getModel().defProps.prop(i).name();
	    	String pval = prop(pname).constStringVal();
	    	if (pval != null)
		    out.println("\t" + toString(ctxt) + "." +
		    pname + "=\"" + pval + "\";");
	    }
	    super.writeFlat(out, ctxt);
	}

	// Var.List
	public static class List extends Comp.List {
	    public List(int n) { super(n); }
	    public List(Expr e) throws Xcept { super(e); }
	    public Var var(int i) { return (Var) get(i); }
	    public Domain.List domainList() {
		Domain.List list = new Domain.List(1);
		for (int i=0; i<size(); i++) {
		    Var v = var(i);
		    list.addUniq(v.domainList());
		}
		return list;
	    }
	    public List zeroDeriv() throws Xcept {
		List list = new List(size());
		for (int i=0; i<size(); i++) 
		    list.addUniq(var(i).zeroDeriv());
		return list;
	    }
	}	    
}
