/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// ODE1 Problem controls for project

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import org.w3c.dom.Element;

public class PSolverSettings extends PNamed {

	// internals
	public ChoiceControl ode_which; // which ODE solver
	public ChoiceControl pde_which; // which PDE solver

	// constructor
	public PSolverSettings(PNamed p, String n) 
	throws Xcept {
	    super(p, n);

	    // generate control based on server defaults
	    NamedVal.NList nvals = server().getSolverDefaults();
	    for (int i=0; i<nvals.size(); i++) {
	    	NamedVal nval = nvals.nval(i);
		String name = nval.name().substring(7);
		String[] labels = null;
		if (name.equals("ode_which"))
		    labels = new String[] { "Auto", "Dopri5", "Radau",
			"KM", "Fehlberg", "Euler", "RK2", "RK4", "CVode" };
		if (name.equals("pde_which"))
		    labels = ASModel.PDE_Solvers;		
		if (name.equals("fzero_unbound"))
		    labels = server().optimAlgs().getNames(OptimAlg.UNBOUND_FZERO).array();
		if (name.equals("fzero_bound"))
		    labels = server().optimAlgs().getNames(OptimAlg.FZERO).array();
		Control.create(this, nval, name, labels);
	    }
	    ode_which = (ChoiceControl) child("ode_which");
	    pde_which = (ChoiceControl) child("pde_which");
	    pde_which.addLegacyLabel("LSFEA3", "LSFEA");
	}

	// query
	public String diagInfo() { return "SolverSettings " + name(); }
	public String xmlLabel() { return "solver"; }

	// pretty names
	private String prettyName(String pfx, Control c) {
	    String cname = c.name();
	    if (! cname.startsWith(pfx)) return null;
	    return cname.substring(pfx.length());
	}
	public String prettyName(Control c, 
	ChoiceControl which, int i) {
	    String pfx = which.name().substring(0,4)
		+ which.stringVal(i) + "_";
	    return prettyName(pfx, c);
	}

	// import child, ignore old Toms690 control
	public PNamed importXMLChild(Element c) {
	    String cname = c.getAttribute("name");
	    if (cname.equals("pde_Toms690_order")) return null;
	    return super.importXMLChild(c);
	}

	// applicable controls
	public Control.List controls(ChoiceControl which, int k) {
	    Control.List cntls = new Control.List(8);
	    for (int i=0; i<nChild(); i++) {
		if (! (child(i) instanceof Control)) continue;
		Control cntl = (Control) child(i);
		if (prettyName(cntl, which, k) != null) 
		    cntls.add(cntl);
	    }
	    return cntls;
	}

	// controls starting with string
	public Control.List controls(String pfx) {
	    Control.List cntls = new Control.List(8);
	    for (int i=0; i<nChild(); i++) {
		if (! (child(i) instanceof Control)) continue;
		Control cntl = (Control) child(i);
	 	if (cntl.name().startsWith(pfx))
		    cntls.add(cntl);
	    }
	    return cntls;
	}   
}


