/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Chemical Equation

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.mml.*;
import java.util.StringTokenizer;

class ChemEqn {
	private String eqn;
	protected Chem.List chem, lchem, rchem;
	protected double[] lcoef, rcoef;

	// String constructor
	protected ChemEqn(MFSys sys, String e) throws Xcept {
	    eqn = e;

	    // parse eqn
	    StringTokenizer tokz = new StringTokenizer(eqn, "+= ", true);
	    int ct = tokz.countTokens();
	    lchem = new Chem.List(ct);
	    rchem = new Chem.List(ct);
	    lcoef = new double[ct];
	    rcoef = new double[ct];
	    float coef = -1;
	    boolean nPending = false;	// pending number
	    double mult = 0;
	    for (int i=0; i<ct; i++) {
		String tok = tokz.nextToken();
		if (tok.equals("+")) continue;
		if (tok.equals(" ")) continue;
		if (tok.equals("=")) {
		    if (coef != -1) throw new Xcept ( 
			"Ill-formed Reaction equation");
		    coef = 1;
		    continue;
		}

		// separate number and name
		int k=0;
		while(k<tok.length() && !Character.isLetter(tok.charAt(k))) 
		    k++;
		if (nPending) { 
		    if (k>0) throw new Xcept ( 
			"Ill-formed Reaction equation");
		    nPending = false;
		} else if (k==0) {
		    mult = 1;
	 	} else {
		    mult = Util.toDouble(tok.substring(0,k));
		    if (Double.isNaN(mult)) throw new Xcept(
			"malformed chemical equation: " + e);
		    if (k == tok.length()) {
			nPending = true;
		    	continue;
		    }
		}
		String chname = tok.substring(k);
		    
		Comp c1 = sys.getChild(chname);
		if (c1 == null || !Chem.class.isInstance(c1)) 
		    throw new Xcept ("Unknown Chem " + chname);
		if (coef < 0) {
		    lchem.addUniq(c1);
		    int j = lchem.indexOf(c1);
		    lcoef[j] -= coef * mult;
		} else {
		    rchem.addUniq(c1);
		    int j = rchem.indexOf(c1);
		    rcoef[j] += coef * mult;
		}
 	    }

	    if (coef != 1) throw new Xcept(eqn + 
		": Ill-formed Chemical equation");
	    chem = new Chem.List(ct);
	    chem.addUniq(lchem);
	    chem.addUniq(rchem);
	}
	
	// count chem on one side
	public double factor(Chem c, boolean left) {
	    Chem.List chem = left ? lchem : rchem;
	    double[] coef = left ? lcoef : rcoef;
	    int j = chem.indexOf(c);
	    return (j<0) ? 0 : coef[j];
	}
	public double lfactor(Chem c) { return factor(c, true); }
	public double rfactor(Chem c) { return factor(c, false); }
	public double factor(Chem c) { return rfactor(c) - lfactor(c); }
	public double rtot() {
	    double tot = 0;
	    for (int i=0; i<rchem.size(); i++) 
		tot += rcoef[i];
	    return tot;
	}
	public double ltot() {
	    double tot = 0;
	    for (int i=0; i<lchem.size(); i++) 
		tot += lcoef[i];
	    return tot;
	}

	// diagnostics
	public String diagStr() {
	    String s = eqn + ":";
	    for (int i=0; i<lchem.size(); i++) 
		s = s + " " + lchem.comp(i).name() + "*" + lcoef[i];
	    for (int i=0; i<rchem.size(); i++) 
		s = s + " " + rchem.comp(i).name() + "*" + rcoef[i];
	    return s;
	}

	// String rep
	public String toString() { return eqn; }
}

