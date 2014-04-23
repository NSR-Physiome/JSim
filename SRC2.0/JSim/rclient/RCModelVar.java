/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// ongoing connection to remote model run-time

package JSim.rclient;

import java.io.*;
import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 

public class RCModelVar extends ASVar {
	private RCModel model;	// attached model
	private RCInfo.ModelVar info; // initialization info
	private ASVar[] domains; // domains
	private String defAssign;  // default assignment
	private String assign;  // current assignment
	private Unit unit;  // unit
	private double finalRealVal; // final real value
	private StringList domainNames;

	// constructor
	public RCModelVar(RCModel m, RCInfo.ModelVar i) throws Xcept {
	    model = m;
	    info = i;

	    // for now,  defaults blank or numeric
	    defAssign = info.defAssign;
	    assign = defAssign;
	    unit = (info.unit == null) ? 
		Unit.scalar() : info.unit.unit();
	    finalRealVal = Double.NaN;
	}

	// set domains once all vars created
	protected void setDomains() throws Xcept {
	    if (ndim() < 1) return;
	    domains = new ASVar[ndim()];
	    for (int i=0; i<ndim(); i++)
		domains[i] = model.getASVar(info.domains[i]);
	}

	// set value after run
	protected void setFinalVal(double d) { finalRealVal = d; }

	// query
	public String name() { return info.name; }
	public int dataType() { return info.dataType; }
	public boolean isInput() { return info.isInput; }
	public boolean isDomain() { return info.isDomain; }
	public int ndim() { 
	    if (info.domains == null) return 0;
	    return info.domains.length;
	}
	public ASVar domain(int i) { return domains[i]; }
	public double realVal(Context ctxt) { return finalRealVal; }	
 
	public Expr unitCorrect() { return this; }
	public String toString() { return name(); }
	public String toString(Context ctxt) { return name(); }
	public Unit unit() { return unit; } 
	public Expr.List domainList() {
	    Expr.List list = new Expr.List(2);
	    for (int i=0; i<ndim(); i++)
		list.add(domain(i));
	    return list;
	}
	public double finalRealVal() throws Xcept {
	    return finalRealVal;
	}
	public String[] labels() { return info.labels; }
	public String[] labelValues() { return info.labelValues; }

	// domain names
	public StringList getDomainNames() {
	    if (domainNames == null) {
	    	domainNames = new StringList();
		for (int i=0; i<ndim(); i++) 
		    domainNames.add(domain(i).name());
	    }
	    return domainNames;
	}

	// new assign methods
	public String getDefault() { return defAssign; }
	public String getAssign() { return assign; }
	public void setAssign(String s) throws Xcept {
	    assign = null;
	    if (s != null) 
		model.setAssign(this, s);
	    assign = s;
	}

}

