/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time domain variable

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 
import java.util.ArrayList;

public final class RTRealDomain extends RTVar {
	public RTRealNVar vmin, vmax, vdelta; // controlling vars
	public RTIntNVar vct; // grid size
	private int domainID; // index to RTModel.domains

	// constructor
	public RTRealDomain(RTModel m, String n, String u, int ph,
	RTRealNVar v1, RTRealNVar v2, RTRealNVar v3, RTIntNVar v4) throws Xcept {
	    super(m, n, u, ph);
	    vmin = v1;
	    vmax = v2;
	    vdelta = v3;
	    vct = v4;
	    domainID = model.domains.size();
	    model.domains.add(this);
	}

	// basic query
	public int ndim() { return 1; }
	public boolean isDomain() { return true; }
	public ASVar domain(int i) { return this; }
	public int domainID() { return domainID; }

	// real value for Expr
	public double realVal(Context ctxt0) throws Xcept {
	    if (! (ctxt0 instanceof RTContext)) throw new Xcept(this,
		"realVal(ctxt) requires RTContext");
	    RTContext ctxt = (RTContext) ctxt0;
	    return ctxt.realVal(this); 
	}

	// other abstract implementations
	public void addDomains(Expr.List list) { 
	    list.addUniq(this); 
	} 

	public StringList getDomainNames() {
	    if (domainNames == null)
	    	domainNames = new StringList(name);
	    return domainNames;
	}
}

