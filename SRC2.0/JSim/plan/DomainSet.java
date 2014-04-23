/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plan-time set of domains

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public class DomainSet extends LinkedHashSet<Domain> implements DiagInfo {
	
	// constructors
	public DomainSet() { super(); }
	public DomainSet(ArrayList<Domain> arr) { super(arr); }
	public DomainSet(DomainSet set) { super(set); }
	public DomainSet(TModel model, Expr.List exprs) throws Xcept {
	    this();
	    for (int i=0; i<exprs.size(); i++) {
	    	Var v = model.var((Var) exprs.expr(i));
		if (v != null && v.isDomain())
		    add((Domain) v);
	    }    
	}
	public DomainSet(Domain x) {
	    this();
	    add(x);
	}

	// operators
	public DomainSet minus(DomainSet xs) {
	    DomainSet ret = new DomainSet(this);
	    ret.removeAll(xs);
	    return ret;
	}
	public DomainSet xsect(DomainSet xs) {
	    DomainSet ret = new DomainSet(this);
	    ret.retainAll(xs);
	    return ret;
	}

	// query
	public Domain first() { return iterator().next(); }
	public String toString() { return toString(""); }
	public String toString(String sfx) {
	    String s = "(";
	    int ct = 0;
	    Iterator<Domain> iter = iterator();
	    while (iter.hasNext()) {
		if (ct>0) s = s + ",";
	        s = s + iter.next() + sfx;
		ct++;
	    }
	    return s + ")";
	}
	public String atString() {
	    String s = "";
	    Iterator<Domain> iter = iterator();
	    while (iter.hasNext()) {
		s = s + "@" + iter.next();
	    }
	    return s;
	}
	public String diagInfo() { return "DomainSet " + this; }
	public LinkedHashSet<String> stringSet() {
	    LinkedHashSet<String> sset = new LinkedHashSet<String>();
	    Iterator<Domain> iter = iterator();
	    while (iter.hasNext()) 
	    	sset.add(iter.next().toString());
	    return sset;
	}
	public boolean containsAny(DomainSet xset) {
	    Iterator xs = iterator();
	    while (xs.hasNext()) 
	    	if (xset.contains(xs.next()))
		    return true;
	    return false;
	}
	public boolean containsAll(DomainSet xset) {
	    Iterator xs = xset.iterator();
	    while (xs.hasNext()) 
	    	if (! contains(xs.next()))
		    return false;
	    return true;
	}
	
}
