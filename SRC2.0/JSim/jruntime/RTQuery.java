/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// data query from model

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*;

public class RTQuery implements ASQuery, DiagInfo {
	private RTModel model;
	private String desc;
	private Expr expr;  // from RTModel.parseExpr()
	private StringList domainNames;
	
	// constructor
	public RTQuery(RTModel model, String s) throws Xcept {
	    this.model = model;
	    desc = s;
	    expr = model.parseExpr(s);
	}
	
	// simple query
	public String toString() { return desc; }
	public String diagInfo() { return "Query " + desc; }
	public Expr expr() { return expr; }
	public String unitString() { 
	    Unit u = expr.unit();
	    if (u == null) return "";
	    return u.pubName();
	}
	public int ndim() { 
	    if (expr instanceof ASVar)
	    	return ((ASVar) expr).ndim();
	    if (domainNames == null)
	        loadDomainNames();
	    return domainNames.size();
	}
	public StringList getDomainNames() {
	    if (expr instanceof RTVar)
	    	return ((RTVar) expr).getDomainNames();
	    if (domainNames == null)
	    	loadDomainNames();
	    return domainNames;
	}
	public boolean isConst() { return expr.isConst(); }
	public double constRealVal() throws Xcept {
	    return Double.NaN;
	    // return expr.constRealVal(); needed???
	    // above causes ConstContext failure in RTVarFuncCall
	}
		
	// loadDomainNames
	private void loadDomainNames() {
	    Expr.List doms = new Expr.List();
	    expr.addDomains(doms);
	    domainNames = new StringList();
	    for (int i=0; i<doms.size(); i++) 
	    	domainNames.add(doms.expr(i).toString());
	}
	    
}
