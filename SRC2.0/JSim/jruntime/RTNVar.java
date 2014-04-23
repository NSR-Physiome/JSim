/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// n-dimensional real variable

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.aserver.*; 
import java.util.ArrayList;
import java.lang.Math;

abstract public class RTNVar extends RTVar {
	protected RTRealDomain[] doms;	// domains
	protected boolean[] reqDoms; // domains req'd for calc?

	// constructor
	public RTNVar(RTModel m, String n, String u, int ph,
	RTRealDomain[] d) throws Xcept {
	    super(m, n, u, ph);
	    doms = (d==null) ? new RTRealDomain[0] : d;	
	}

	// abstract implementations
	public final int ndim() { return doms.length; }
	public final boolean isDomain() { return false; }
	public final ASVar domain(int i)  { return doms[i]; }

	// add reqDoms
	public void addReqDom(RTRealDomain x) {
	    if (reqDoms == null)
	    	reqDoms = new boolean[doms.length];
	    for (int j=0; j<doms.length; j++)
	    	if (doms[j] == x) 
		    reqDoms[j] = true;
	}	

	// bookeeping
	public void addDomains(Expr.List list) {
	    if (doms == null) return;
	    for (int i=0; i<doms.length; i++) {
		RTRealDomain x = doms[i];
		list.addUniq(x);
	    }
	}

	// domain names
	public StringList getDomainNames() {
	    if (domainNames == null) {
	    	domainNames = new StringList();
		for (int i=0; i<doms.length; i++) 
		    domainNames.add(doms[i].name);
	    }
	    return domainNames;
	}
}


