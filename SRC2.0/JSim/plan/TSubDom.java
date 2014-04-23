/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plan-time equation subdomain

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public class TSubDom implements DiagInfo {
	private TModel model;
	private int type; 
	private Domain x;
	private int hashCode;
	
	public static final int ENTIRE = VarUsage.CURR;
	public static final int LHBC = VarUsage.MIN;
	public static final int RHBC = VarUsage.MAX;
	
	// constructors
	public TSubDom(TModel model, int type, Domain x) throws Xcept {
	    this.model = model;
	    this.type = type;
	    this.x = x;
	    hashCode = type;
	    if (x != null) hashCode += 10 * x.domInx();
	}
	public TSubDom(TModel model, SubDom sd) throws Xcept {
	    this.model = model;
	    if (sd.isEntire()) {
	    	type = ENTIRE;
	        return;
	    }
	    type = sd.isLHBC() ? LHBC : RHBC;
	    if (sd.bcDomain() == null) throw new AbortXcept(
	    	"Unsupported when clause: " + sd);
	    x = model.domain(sd.bcDomain());
	    hashCode = type;
	    if (x != null) hashCode += 10 * x.domInx();
	}	        

	// query
	public TModel model() { return model; }
	public boolean isEntire() { return type == ENTIRE; }
	public boolean isLH() { return type == LHBC; }
	public boolean isRH() { return type == RHBC; }
	public Domain domain() { return x; }
	public int hashCode() { return hashCode; }	    
	public boolean equals(TSubDom sd) {
	    return type == sd.type && x == sd.x;
	}
	public String toString() {
	    switch (type) {
	    case ENTIRE: return "entire";
	    case LHBC: return "" + x + "=" + x + ".min";
	    case RHBC: return "" + x + "=" + x + ".max";
	    default: return "unknown_subdom";
	    }
	}
	public String diagInfo() { return toString(); } 
  
}
	    
		
	
