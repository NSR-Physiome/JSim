/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// PDE BC block of sequenced calculations

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

public class BCBlock extends MuBlock {
	private PDEBlock parent;
	private TSubDom subdom;
	
	// constructor
	public BCBlock(PDEBlock parent, TSubDom subdom) throws Xcept {
	    super(parent, parent.vuTools);
	    this.parent = parent;
	    this.subdom = subdom;
	    muDoms.add(parent.t);
	    title = "PDE BC " + subdom;
	    for (int i=0; i<parent.detools.size(); i++)
	    	addDE(parent.detools.get(i));
	}

	// add DETool
	protected void addDE(DETool detool) throws Xcept {
	    super.addDE(detool);
	    if (parent.nx() != 1) return;
	    PDE1Factors factors = 
	    	(PDE1Factors) parent.factors(detools.size() - 1);
	    boolean left = subdom.isLH();
	    Expr[] fs = new Expr[] {
	    	factors.coefF1(left), 
		factors.coefF2(left),
		factors.coefF3(left)
	    };
	    VarUsages vus = new VarUsages(model(), fs);
	    vsols.add(vus);
	}

	// simple query
	public String toString() { 
	    return title() + " " + parent.vstate; 
	}
	public TSubDom subdom() { return subdom; }
}
