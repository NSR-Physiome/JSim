/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

public class Domain extends Var {

	public static final String[] SFXS = 
	    new String[] { "min", "max", "ct", "delta" };
	public static final int NAUX = SFXS.length;
	public static final int MIN = 0;
	public static final int MAX = 1;
	public static final int CT = 2;
	public static final int DELTA = 3;

	// domain control fields
	public RealNVar vmin;
	public RealNVar vmax;
	public IntNVar vct;
	public RealNVar vdelta;		// optional
	public int maxct;	// runtime max for vct
	public boolean setDelta;// true  = user set vdelta, calc vct
				// false = user set vct, calc vdelta


	// 1.7 compiler
	private int domInx;
	public int domInx() { return domInx; }

	// constructor
	public Domain(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    vmin = new RealNVar(this, "min", null);
	    vmax = new RealNVar(this, "max", null);
	    vct = new IntNVar(this, "ct", null); 
	    vdelta = new RealNVar(this, "delta", null);
	    if (e != null && e.size() > 0) 
		throw new Xcept(this,
		    "domain declaration takes no arguments");   
	    MathSys topMath = getTopMath();
	    if (topMath != null) {
	    	domInx = topMath.domains.size();
		topMath.domains.add(this);
	    }
	}

	// set unit
	public void setUnit(Unit u) throws Xcept {
	    super.setUnit(u);
	    if (u != null) {
	        vmin.setUnit(u);
	    	vmax.setUnit(u);
	    	vdelta.setUnit(u);
	    }
	}

	// Var query methods
	public int ndim() { return 1; }
	public Domain domain(int i) {
	    if (i==0) return this;
	    return null;
	}
	public boolean isDomain() { return true; }
	public boolean hasDomain(Var v) { return v==this; }

	// get control variable from index
	public Var vaux(int i) throws Xcept {
	    switch(i) {
	    case MIN: return vmin;
	    case MAX: return vmax;
	    case CT: return vct;
	    case DELTA: return vdelta;
	    }
	    throw new Xcept(this, "Illegal vaux() index: " + i);
	}

	// get index from control variable
	public int auxInx(Var v) throws Xcept {
	    if (v.parent != this) throw new Xcept(v,
	    	"Variable not a control for domain " + this);
	    for (int i=0; i<SFXS.length; i++) 
	    	if (v.name().equals(SFXS[i]))
		    return i;
	    throw new Xcept(v,
	    	"Variable is not a control for domain " + this);
	}	    	

	// BCs
	public SubDom lhbc() throws Xcept { return new SubDom(this.eq(vmin)); }
	public SubDom rhbc() throws Xcept { return new SubDom(this.eq(vmax)); }
	public Expr lhbExpr() throws Xcept { return this.eq(vmin); }
	public Expr rhbExpr() throws Xcept { return this.eq(vmax); }

	// Domain.List
	public static class List extends Var.List {
	    public List(int n) { super(n); }
	    public List(Expr e) throws Xcept { 
		this(1);
		e.addDomains(this);
	    }
	    public Domain domain(int i) { return (Domain) var(i); }
	}
}
