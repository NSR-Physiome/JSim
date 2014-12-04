/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Control pointing to a model input parameter

package JSim.project; import JSim.aserver.*;

import JSim.util.*;

public class ModelParControl extends StringControl {
	protected PModelVars vars; // model vars
	protected Control control; // returned input control

	// constructor
	public ModelParControl(PNamed p, String n, 
	PModel pmodel) throws Xcept {
	    super(p, n);
	    vars = pmodel.vars();
	    control = null;
	    valid = false;
	}

	// get & set expr
	public Control control() { return control; }

	// recalculate valid flag
	public void revalidate() {
	    control = null;
	    valid = false;
	    validMsg = null;
	    if (Util.isBlank(val())) return;
	    control = vars.control(val());
	    if (control == null)
		validMsg = "no such input variable \"" + val() + "\"";
	    else
		valid = true;
	}

	// list of pickable values
	public StringList pickList() {
	    StringList list = new StringList(4);
	    list.add(" ");
	    PNamed root = vars.assign();
	    if (root == null) return list;
	    for (int i=0; i<root.nChild(); i++) {
		PNamed pnamed = root.child(i);
		list.add(pnamed.name());
	    }
	    return list;
	}
	    
}

