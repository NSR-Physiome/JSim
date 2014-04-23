/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// remote data query

package JSim.rclient;

import JSim.aserver.*;
import JSim.util.*;
import java.rmi.*;

public class RCQuery implements ASQuery, DiagInfo {
	private RCModel model;
	private String desc;
	private RCInfo.Query info;
	
	// constructor
	public RCQuery(RCModel model, String desc) throws Xcept {
	    this.model = model;
	    this.desc = desc;
	    model.request();
	    try {
	    	info = model.rclient().parseQuery(model.id(), desc);
   	    } catch (RemoteException e) {
	    	throw Xcept.wrap(e);
	    } 	        
	}

	// query
	public String toString() { return desc; }
	public String diagInfo() { return "Query " + desc; }
	public String unitString() { return info.unitString; }
	public int ndim() { return info.ndim; }
	public boolean isConst() { return info.isConst; }
	public double constRealVal() { return info.constRealVal; }

	// domain names
	public StringList getDomainNames() {
	    return info.domainNames;
	}

}

