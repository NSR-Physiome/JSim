/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Control pointing to another PNamed

package JSim.project;

import JSim.util.*;

public class PNamedControl extends StringControl {
	protected PNamed root; // root of search
	protected Class[] classes; // allowable classes
	protected PNamed pnamed; // where control points
	protected Control.List slaves; // slaves to this control

	// constructor
	public PNamedControl(PNamed p, String n, 
	PNamed r, Class[] c) throws Xcept {
	    super(p, n);
	    root = r;
	    classes = c;
	    pnamed = null;
	    valid = false;
	    slaves = new Control.List(4);
	}

	// get & set expr
	public PNamed pnamed() { return pnamed; }

	// recalculate valid flag
	public void revalidate() {
	    pnamed = null;
	    valid = false;
	    validMsg = null;
	    String val = isBlank() ? singlePick() : val();
	    if (Util.isBlank(val)) return;
	    pnamed = root.nestedChild(val);
	    for (int i=0; i<classes.length; i++)
		if (pnamed != null && 
		    classes[i].isInstance(pnamed))
		    valid = true;
	    if (pnamed == null)
		validMsg = "\"" + val + "\" not found in " +
		    root.name();
	    else
		validMsg = "\"" + val + "\" of improper type";
	    for (int i=0; i<slaves.size(); i++)
	    	slaves.control(i).revalidate();
	}

	// list of pickable values
	public StringList pickList() {
	    StringList list = new StringList(4);
	    for (int i=0; i<root.nChild(); i++) {
		PNamed pnamed = root.child(i);
		boolean keep = false;
		for (int j=0; j<classes.length; j++) 
		    if (classes[j].isInstance(pnamed))
			keep = true;
		if (keep) list.add(pnamed.name());
	    }
	    return list;
	}

	// single pick,  or null 
	public String singlePick() {
	    String single = null;
	    if (root == null) return single;
	    for (int i=0; i<root.nChild(); i++) {
		PNamed pnamed = root.child(i);
		boolean keep = false;
		for (int j=0; j<classes.length; j++) 
		    if (classes[j].isInstance(pnamed))
			keep = true;
		if (!keep) continue;
		if (single != null) return null;
		single = pnamed.name();
	    }
	    return single;
	}
	    
}

