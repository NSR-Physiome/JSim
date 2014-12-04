/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Run-time parm assignments for model

package JSim.project; import JSim.aserver.*;

import JSim.util.*;

public class PModelAssn extends PNamed {
	// internals
	private PModelVars vars; // allows skipping null parent
	private ASModel rt;		// RT model connected
	private ASVar.List inputVars; // input vars

	// constructor
	public PModelAssn(PModelVars p, String n, ASModel r) throws Xcept {
	    super(null, n);
	    vars = p;
	    rt = r;
	    if (rt == null) return; // if not built

	    // create assign controls with default vals
	    ASVar.List list = rt.getASVars();
	    inputVars = new ASVar.List(8);
	    for (int i=0; i<list.size(); i++) {
		ASVar v = list.asvar(i);
		if (! v.isInput()) continue;
		inputVars.add(v);
		new AssignControl(this, v.name(), v);
	    }
	}

	// query
	public String diagInfo() { return "PModelAssn " + name; }
	public String xmlLabel() { return "assign"; }
	public PModel pmodel() { return vars.pmodel(); }
	public Project project() { return vars.project(); }

	// needed?
	public PNamed parent() { return vars.parent(); }

	// not change to assigns
	public void childChanged(PNamed c) throws Xcept {
	    vars.childChanged(c);
	}
}

