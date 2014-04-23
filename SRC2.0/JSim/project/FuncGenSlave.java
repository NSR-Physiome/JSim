/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// user controls for Func Gen slaves

package JSim.project; 

import JSim.aserver.*;
import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class FuncGenSlave extends PNamed {

	// discontinued Longtail names
	public static final String[] BADNAMES = new String[] {
	    "frArea", "rk1", "rk2", "rk3", "rk4", "scaleta", "nexp"
	};
	
	// constructor
	public FuncGenSlave(FuncGen p, String n, String t) 
	throws Xcept {
	    super(p, n);
	    makeControls();
	}
	
	// make user controls based on nvals
	protected void makeControls() throws Xcept {
	    NamedVal.NList nvals = server().getFuncGenDefaults();
	    for (int i=0; i<nvals.size(); i++) {
	    	NamedVal nval = nvals.nval(i);
		String nm = nval.name();
		if (! nm.startsWith(name() + ".")) continue;
		nm = nm.substring(name().length() + 1);
		String[] labels = null;
		if (nm.equals("upslope"))
		    labels = new String[] { "Regular", "Linear" };
		if (nm.equals("PDF"))
		    labels = new String[] { "LagNormal", "Gaussian", "Poisson", "RandomWalk", "GammaVariate" };
		if (nm.equals("tORfr"))
		    labels = new String[] { "tJoin", "frJoin" };
		if (nm.equals("expORpow"))
		    labels = new String[] { "Exponential", "PowerLaw" };
		Control.create(this, nval, nm, labels);
	    }
	}
	
	// simple query
	public String diagInfo() { return "FuncGenSlave " + name(); }
	public String xmlLabel() { return "function"; }
	public FuncGen master() { return (FuncGen) parent(); }

	// import XML element into matching PNamed child
	public PNamed importXMLChild(Element c) {
	    String cname = c.getAttribute("name");
	    if (cname.equals("rd")) cname = "RD";
	    if (name().equals("Longtail")) 
	    	for (int i=0; i<BADNAMES.length; i++) 
		    if (BADNAMES[i].equals(cname))
		        return null;
	    PNamed pnamed = nestedChild(cname); // nested for PModelVars
	    if (pnamed == null) 
		importXMLMessage("XML Element " + c.getNodeName() + " " + 
		   cname + " ignored.  No matching child in " + 
		   xmlLabel() + " " + this + ".");
	    else
	        pnamed.importXML(c);
	    return pnamed;
	}
}
  
