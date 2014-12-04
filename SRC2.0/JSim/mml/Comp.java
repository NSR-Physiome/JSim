/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// component

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.PrintStream;
import java.util.ArrayList;
import java.lang.reflect.*;

public abstract class Comp extends NamedExpr {
	public static final String sep = "."; // heir. separator
	public static final char sepchar = '.'; // heir. separator
	public static final int PUBLIC = 0; // normal visibility
	public static final int EXTERN = 1; // set by external comp
	public static final int PRIVATE = 2; // invisibie to external comp

	protected Comp parent;	// parent component, 
				// null if Model or template
	protected boolean builtin; // this child in parent tempate?
	protected Templ templ;	// Template
	protected String name;	// name
	protected int access;	// PUBLIC, EXTERN, PRIVATE
	public Comp.NList child; // child sub-components 
    	public CompProp.List props; //property list
	public Eqn.List eqn;	// equation for this comp
	public Event.List events; // events for this comp
	public Expr.List voidFuncCalls; // void function calls
	protected Expr.List args; // arguments used to create list

	// constructor
	public Comp(Comp p, String n, Expr.List e) throws Xcept {
	    parent = p;
	    name = n;
	    args = e;
	    child = new Comp.NList(1);
	    props = new CompProp.List(2);
	    eqn = new Eqn.List(1);
	    events = new Event.List(1);
	    voidFuncCalls = new Expr.List(1);
	    if (parent == null) return;
	    if (parent.getNamed(n) != null)
		throw new Xcept (this, "duplicate component or property name");
	    validateName(this, name);
	    parent.registerChild(this); 
	    builtin = true;
	}

	// validate component name
	public static void validateName(DiagInfo dinfo, String n) throws Xcept {
	    String msg = "Illegal name \"" + n + "\": "; 
	    for (int i=0; i<n.length(); i++) {
		char c = n.charAt(i);
		if (Character.isLetterOrDigit(c)) continue;
		if (c == '_') continue;
		if (c == '.') continue;
		if (c == ':') continue;
		throw new Xcept(dinfo, msg + 
		    "contains illegal character '" + c + "'");
	    }
	    if (n.length()<1) throw new Xcept(dinfo, msg + "blank");
	    char c1 = n.charAt(0);
	    if (! Character.isLetter(c1)) throw new Xcept(dinfo, 
		msg + "must start with letter");
	    if (n.length()>1) {
		String s2 = n.substring(0,2).toUpperCase();
		if (s2.equals("JS")) throw new Xcept(dinfo, msg +
		    "names starting with JS are reserved for JSim internal use");
	    }
	}    

	// register child (subclasses may restrict components)
	public void registerChild(Comp c) throws Xcept {
	    child.add(c);
	}
	// register eqn (subclasses may restrict allowable stmts)
	public void registerEqn(Eqn e) throws Xcept {
	    eqn.add(e);
	}
	// register event (subclasses may restrict allowable stmts)
	public void registerEvent(Event e) throws Xcept {
	    events.add(e);
	}

	// public info common methods
	public String name() { return name; }
	public String fullName() { 
	    if (parent == null) return name;
	    return parent.fullName() + sep + name;
	}

	// Expr query methods
	public int dataType() { return Expr.UNDEFINED; }
	public boolean sameAs(Expr ex) {
	    return this == ex;
//	    if (! Comp.class.isInstance(ex)) return false;
//	    return (this == (Comp) ex);
	}
	public Comp getComp() { return this; }
	public String toString() { return toString(getSys().ctxt); }
	public String toString(Context ctxt) 
	    { return ctxt.newName(this); }
	public String diagInfo() {  
	    String s = fullName();
	    return "Component " + 
	      (s.startsWith("top.") ? s.substring(4) : s);
	}
	public Sys getSys() { return parent.getSys(); }

	// Expr query methods
	public Var getVar() { return isVar() ? (Var) this : null; }
	
	// lists
	public void addDomains(Expr.List list) { 
	    Var v = getVar();
	    if (v != null) v.addDomains(list);
	}
	public void addSubUnits(Unit.List list, UnitNList modelunits) 
	throws Xcept {
	    if (unit() != null) 
		unit().addSubUnits(list, modelunits);
	    for (int i=0; i<child.size(); i++) {
		Comp c = child.comp(i);
		c.addSubUnits(list, modelunits);
	    }
	}

	public Comp getParent() { return parent; }
	public Comp getChild(String n) {
	    if (child == null) return null;
	    return (Comp) child.getByName(n);
	}
	public Comp.NList getChild(Class clss) {
	    Comp.NList list = new Comp.NList(4);
	    for (int i=0; i<child.size(); i++) {
		Comp c = child.comp(i);
		if (c.getClass() == clss) list.add(c);
	    }
	    return list;
	}
	public Model getModel() throws Xcept {
	    Comp c = this;
	    while (c.parent != null) c = c.parent;
	    if (c instanceof Model) return (Model) c;
	    if (c.templ == null) throw new Xcept(c,
		"templ null, cannot getModel() obj=" +
		c.hashCode());
	    return c.templ.model;
	}
	public MathSys getTopMath() {
	    Comp c = this;
	    while (c.parent != null && ! (c instanceof Model))
	    	c = c.parent;
	    if (c instanceof MathSys) return (MathSys) c;
	    return null;
	}
	public boolean isPublic() { return access == PUBLIC; }
	public boolean isPrivate() { return access == PRIVATE; }
	public boolean isExtern() { return access == EXTERN; }
	public boolean isVar() { return false; }
	public boolean isNVar() { return false; }
	public boolean hasUnit() { return false; }
	public void setUnit(Unit u) throws Xcept { 
	    throw new Xcept(this, "setUnit() not supported");
	}
	public Unit unit() { return null; } 
	public Expr unitCorrect() { return this; }

	// set access
	public void setAccess(int a) throws Xcept {
	    access = a;
	}

	// add property to this component
	public void addProp(String n, int dtype) throws Xcept {
	    if (prop(n) != null) throw new Xcept(this,
		"duplicate property");
	    CompProp p = new CompProp(this, n, dtype);
	    props.add(p);
	}

	// find named sub-component or property
	public NamedExpr getNamed(String n) {
	    Comp c = getChild(n);
	    if (c != null) return c;

	    int inx = n.indexOf(sepchar);
	    if (inx>=0) {
	    	String n1 = n.substring(0, inx);
	    	String n2 = n.substring(inx+1);
	    	c = getChild(n1);
	    	if (c != null) {
		    NamedExpr cn = c.getNamed(n2);
		    if (cn != null) return cn;
		}
	    }

	    inx = n.lastIndexOf(sepchar);
	    if (inx>=0) {
	    	String n1 = n.substring(0, inx);
	    	String n2 = n.substring(inx+1);
	    	c = getChild(n1);
	    	if (c != null) {
		    NamedExpr cn = c.getNamed(n2);
		    if (cn != null) return cn;
		}
	    }

	    return prop(n);
	}

	// get specified property of this component
	public CompProp prop(String n) {
	    for (int i=0; i<props.size(); i++) {
		CompProp p = (CompProp) props.expr(i);
		if (p.name().equals(n)) return p;
	    }

	    return null;
	}

	// expand derivatives 
	protected void expandChildDerivs() throws Xcept {
	    for (int i=0; i<eqn.size(); i++) 
		eqn.eqn(i).expandDeriv();
	    for (int i=0; i<events.size(); i++) 
		events.event(i).expandDeriv();
	    for (int i=0; i<child.size(); i++) 
		child.comp(i).expandChildDerivs();
	}

	// unit correct all equations
	public void unitCorrectAll() throws Xcept {
	    Xcept xcept = null;
	    for (int i=0; i<eqn.size(); i++) {
		Eqn e = eqn.eqn(i);
		Util.verbose("Unit correcting: " + e.toString());
		try {
		    e.unitCorrect();
		    Util.verbose("             to: " + e.toString() + "\n");
		} catch (Xcept x) {
		    if (! Util.verbose) throw x;
		    if (xcept == null) xcept = x;
		    Util.verbose("Unit correction error" + x.getMessage());
		}
	    }
	    for (int i=0; i<events.size(); i++) {
		Event e = events.event(i);
		Util.verbose("Unit correcting: " + e.toString());
		try {
		    e.unitCorrect();
		    Util.verbose("             to: " + e.toString() + "\n");
		} catch (Xcept x) {
		    if (! Util.verbose) throw x;
		    if (xcept == null) xcept = x;
		    Util.verbose("Unit correction error" + x.getMessage());
		}
		Util.verbose("             to: " + e.toString() + "\n");
	    }
	    if (xcept != null) throw xcept;
	    for (int i=0; i<child.size(); i++) 
		child.comp(i).unitCorrectAll();
	}

	// simplify contents
	public void simplifyAll() throws Xcept {
	    for (int i=0; i<eqn.size(); i++) 
		eqn.eqn(i).simplify();
	    for (int i=0; i<events.size(); i++) 
		events.event(i).simplify();
	    for (int i=0; i<child.size(); i++) 
		child.comp(i).simplifyAll();
	}   

	// deriv
	public Expr takeDomDeriv(NamedExpr tt) throws Xcept {
//	    if (! Domain.class.isInstance(tt)) throw new Xcept(tt, 
//		"takeDomDeriv() requires RealDomain");
	    Var t = (Var) tt;
	    Var v = getVar();
	    if (v == null) throw new Xcept(this,
		"takeDeriv() not supported for this component");
	    if (v == t) return one;
	    if (t.ndim() == 1 && ! v.hasDomain(t)) 
		return zero;
	    return v.deriv(t);
	}
	public Expr expandDeriv() { return this; }

	// linear factor
	public Expr linearFactor(NamedQueryExpr q, boolean keep) throws Xcept {
	    if (keep) 
		return (this == q) ? one : zero;
	    else 
		return (this == q) ? ((Expr) zero) : this;
	}
	
	// write flatmath 
	public void writeFlat(PrintStream out, Context ctxt) 
	throws Xcept {
	    Model m = getModel();
	    if (! m.realDomainTempl.clss().isInstance(this)) // KLUDGE
	    	for (int i=0; i<child.size(); i++) 
		    child.comp(i).writeFlat(out, ctxt);
	    for (int i=0; i<eqn.size(); i++) 
		out.println("\t" + eqn.eqn(i).toString(ctxt));
	}

	// access string
	public String accessString() {
	    switch (access) {
	    case PUBLIC: return "";
	    case PRIVATE: return "private ";
	    case EXTERN: return "extern ";
	    default: return "";
	    }
	}
	    
	// dump contents 
	public void dump(PrintStream out, String indent) 
	throws Xcept {
	    Unit u = hasUnit() ? unit() : null;
	    out.println(indent + 
		(builtin ? "builtin " : "") + accessString() + 
		((templ == null) ? getClass().getName() : templ.name())
		+ " " +	name() + 
		((args != null) ? args.toString() : "") + " " +
		((u != null) ? u.name + "=" + u.toString() : ""));
	    for (int i=0; i<child.size(); i++) 
		child.comp(i).dump(out, indent + "  ");
	    for (int i=0; i<eqn.size(); i++) 
		out.println(indent + "  " + eqn.eqn(i).toString());
	}
	
	// Comp.NList
	public static class NList extends NamedList {
            public NList(int i) { super(i); }
            public Comp comp(int i) { return (Comp) get(i); }
            public Comp comp(String n) { return (Comp) getByName(n); }
        }

	// Comp.List
	public static class List extends Expr.List {
	    public List(int n) { super(n); }
	    public List(Expr e) throws Xcept {
		this(1);
		e.addNamedExpr(this);
	    }
	    public Comp comp(int i) { return (Comp) get(i); }

	}

}
