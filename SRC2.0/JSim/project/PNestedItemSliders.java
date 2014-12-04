/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// project stored sliders for a PNestedItem

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import java.util.*;
import org.w3c.dom.*;

public class PNestedItemSliders extends PNamed {


	// constructor needed?
	public PNestedItemSliders(PNestedItem item, String name) 
	throws Xcept {
	    super(item, name);
	}	

	// store slider value
	public void setSliderValue(String domainID, double value) 
	throws Xcept {
	    String cname = controlName(domainID);
	    if (! (child(cname) instanceof RealControl))
	        new RealControl(this, cname, Double.NaN);
	    RealControl c = (RealControl) child(cname);
	    c.setVal(value);
	}

	// get slider value
	public double getSliderValue(String domainID)  {
	    String cname = controlName(domainID);
	    if (! (child(cname) instanceof RealControl))
	    	return Double.NaN;
	    try { 
	    	RealControl c = (RealControl) child(cname);
	    	return c.val(); 
	    } catch (Exception e) {
	    	return Double.NaN;
	    }
	}

	// control name for domainID
	private static String controlName(String domainID) {
	    return domainID.replaceAll(" ", "__");
	}

	// import XML child, create control if needed
	public PNamed importXMLChild(Element c) {
	    String cname = c.getAttribute("name");
	    PNamed pnamed = nestedChild(cname); // nested for PModelVars
	    if (pnamed == null) try {
	        pnamed = new RealControl(this, cname, Double.NaN);
	    } catch (Xcept e) {
		importXMLMessage("XML Element " + c.getNodeName() + " " + 
		   cname + " ignored.  No matching child in " + 
		   xmlLabel() + " " + this + ".");
		return null;
	    }
	    pnamed.importXML(c);
	    return pnamed;
	}

	// simple query
	public String diagInfo() { return "NestedSliders " + name; }
	public String xmlLabel() { return "nestedsliders"; }
	
}

