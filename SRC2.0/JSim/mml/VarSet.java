/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// set of variables

package JSim.mml; import JSim.util.*;
import JSim.expr.*; 

public class VarSet {
	public MathSys math;
	public boolean[] hasVar;		// Var included,  by flatInx()
	public int size;

	// constructors
	public VarSet(MathSys m)  {	// empty
	    math = m;
	    hasVar = new boolean[math.nVar()];
	    size = 0;
	}
	public VarSet(VarSet v) {		// copy another VarSet
	    math = v.math;
	    hasVar = (boolean[]) v.hasVar.clone();
	    size = v.size;
	}
	
	// resize set
	public void resize(int sz) {
	    if (sz <= hasVar.length) return;
	    boolean[] nhasVar = new boolean[sz];
	    for (int j=0; j<hasVar.length; j++) 
		nhasVar[j] = hasVar[j];
	    hasVar = nhasVar;
	}

	// add Variable(s) to VarSet
	public void add(Var v) { 
	    int i = v.flatInx();
	    if (i > hasVar.length) resize(i+1);
	    if (!hasVar[i]) size++;
	    hasVar[i] = true; 
	}
	public void add(VarSet v) {
	    resize(v.hasVar.length);
	    for (int i=0; i<hasVar.length; i++) {
		if (!v.hasVar[i] || hasVar[i]) continue;
		size++;
		hasVar[i] = true;
	    }
	}
	public void add(Var.List vlist) {
	    for (int i=0; i<vlist.size(); i++) 
		add((Var) vlist.get(i));
	}

	// subtract var from VarSet
	public void sub(Var v) {
	    int i = v.flatInx();
	    if (i >= hasVar.length) return;
	    hasVar[i] = false;
	}

	// has a variable
	public boolean hasVar(Var v) {
	    int inx = v.flatInx();
	    return (inx >= hasVar.length) ? 
		false : hasVar[v.flatInx()];
	}
	public boolean hasVars(Var.List vlist) {
	    for (int i=0; i<vlist.size(); i++) {
		Var v = (Var) vlist.get(i);
		if (! hasVar(v)) return false;
	    }
	    return true;
	}

	// count vars in this not in v
	public int subct(VarSet v) {
	    resize(v.hasVar.length);
	    int ct = 0;
	    for (int i=0; i<hasVar.length; i++) 
		if (hasVar[i] && !v.hasVar[i]) ct++;
	    return ct;
	}

	// query
	public int size() { return size; }
	public Var.List varList() {
	    Var.List list = new Var.List(size());
	    for (int i=0; i<hasVar.length; i++) 
		if (hasVar[i]) list.add(math.var(i));
	    return list;
	}
	public String toString() { return varList().toString(); }
}	    
