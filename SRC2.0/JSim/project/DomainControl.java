/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Control holding name of a model domain

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;

public class DomainControl extends StringControl {
	private ASVar domain;

	// constructor
	public DomainControl(PNamed p, String n, boolean def) 
	throws Xcept {
	    super(p, n);
	    if (!def) return;
	    StringList doms = pickList();
	    if (doms.size()>=1) 
		value = defaultValue = doms.str(0);
	}

	// recalculate valid flag
	public void revalidate() {
	    valid = false;
	    validMsg = null;
	    domain = null;
	    if (isBlank()) return;
	    try {
	        domain = pmodel().rt().getASVar(value);
	        if (domain.isDomain()) {
		    valid = true;
		} else {
		    domain = null;
		    validMsg = value + " is not a realDomain";
		}
	    } catch (Xcept e) {
		validMsg = e.cleanMessage();
	    }
	}

	// query
	public ASVar domain() { return domain; }	    

	// pickable list of domains
	public StringList pickList() {
	    StringList list = new StringList(4);
	    try {
	    	ASVar.List asvars = pmodel().rt().getASVars();
		for (int i=0; i<asvars.size(); i++) 
		    if (asvars.asvar(i).isDomain())
			list.add(asvars.asvar(i).name());
	    } catch (Xcept e) { }
	    return list;
	}

}

