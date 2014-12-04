/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// set of equations

package JSim.mml; import JSim.util.*;

public class EqnSet implements DiagInfo {
	public MathSys math;
    	public boolean[] hasEqn;	// Eqn included,  by flatInx()
	public int size;

	// blank constructor
	public EqnSet(MathSys m) {	// 
	    math = m;
	    hasEqn = new boolean[math.eqn.size()];
	    size = 0;
	}
	
	// clone constructor
	public EqnSet(EqnSet e) {
	    math = e.math;
	    hasEqn = (boolean []) e.hasEqn.clone();
	    size = e.size;
	}

	// add Eqn
	public void add(Eqn e) { 
	    int i = e.flatInx();
	    if (!hasEqn[i]) size++;
	    hasEqn[i] = true; 
	}

	// has Eqn?
	public boolean hasEqn(Eqn e) {
	    int i = e.flatInx();
	    if (i >= hasEqn.length) return false;
	    return hasEqn[i];
	}

	// # eqns in set (not length of hasEqn table)
	public int size() { return size; }

	// easy query
	public String toString() {
	    String s = "";
	    for (int i=0; i<hasEqn.length; i++) {
		if (!hasEqn[i]) continue;
		Eqn eqn = math.eqn.eqn(i);
		 s = s + "\t" + eqn.toString() + "\n";
	    }
	    return s;
	}	
	public String diagInfo() { return toString(); }

}	    

