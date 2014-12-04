/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// nested graph local axis domain control

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;

public class PNestedDomainControl extends StringControl {
	private StringList pickList;
	
	public static final String NO_VARIATION = "no variation";
	public static final String INNER_LOOP = "inner loop";
	public static final String OUTER_LOOP = "outer loop";
	
	// contructor
	public PNestedDomainControl(PNestedAxis.Local paxis,
	String name) throws Xcept {
	    super(paxis, name);
	    setVal(NO_VARIATION);
	}

	// recalculate valid flag
	public void revalidate() {
	    valid = false;
	    validMsg = null;
	    pickList = new StringList();
	    pickList.add(NO_VARIATION);
	    try {
	    	DataControl c = paxis().item().expr;
		StringList doms = c.getDomainNames();
		if (doms != null)
		    pickList.addAll(doms);
		if (c.getExpr() != null) {
		    pickList.add(INNER_LOOP);
		    pickList.add(OUTER_LOOP);
		}
		valid = pickList.containSame(value);
		if (! valid)
		    validMsg = "Unsupport domain: " + value;
	    } catch (Xcept e) {
	    	validMsg = e.cleanMessage();
	    }
	}	    

	// simple query
	public PNestedAxis.Local paxis() { 
	    return (PNestedAxis.Local) parent(); 
	}
	public StringList pickList() {
	    return pickList; 
	}
}
