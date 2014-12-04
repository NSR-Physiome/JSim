/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Memory allocation strategy

package JSim.project; 
import JSim.aserver.*;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class PModelMemory extends PNamed {

	// storeScheme constants
	public static final int STATIC = ASModel.MEMORY_STATIC;
	public static final int DYNAMIC = ASModel.MEMORY_DYNAMIC;
	public static final int SELECTED = ASModel.MEMORY_SELECTED;
	public static final int PROJECT = 3;

	// storeGrids constants
	public static final int GRID_ALL = ASModel.MEMORY_GRID_ALL;
	public static final int GRID_NTH = ASModel.MEMORY_GRID_NTH;

	// controls
	public ChoiceControl storeGrids; // GRID_ constants above
	public ChoiceControl storeScheme; // NOT YET IN USE
	public StringControl storeExprs; // if storeScheme=SELECTED

	// domain list
	private StringList domains;  // current compiled domain names
	
	// constructor
	public PModelMemory(PNamed p, String name) throws Xcept {
	    super(p, name);
	    storeGrids = new ChoiceControl(this, "storeGrids", 0,
	    	new String[] { "all", "nth" });
//	    storeScheme = new ChoiceControl(this, "storeScheme", 0,
//	    	new String[] { "static", "dynamic", "selected", "needed" });
//	    storeExprs = new StringControl(this, "storeExprs");
	}

	// import XML
	public PNamed importXMLChild(Element e) {
	    String type = e.getNodeName();
	    String name = e.getAttribute("name");
	    String value = e.getAttribute("value");
	    if (name.endsWith(".nth")) {
	    	String xname = name.substring(0, name.length()-4);
		makeControls(xname);
	    }
	    return super.importXMLChild(e);
	}

	// update domains after model build
	protected void updateDomains() throws Xcept {
	    domains = new StringList();
   	    if (! pmodel().rt().isBuilt()) return;
	    ASVar.List asvars = pmodel().rt().getASVars();
	    for (int i=0; i<asvars.size(); i++) {
	    	ASVar v = asvars.asvar(i);
		if (! v.isDomain()) continue;
		String xname = v.name();
		domains.add(xname);
		makeControls(xname);
	    }
	}
	
	// add domain, if necessary
	private void makeControls(String xname)  {
	    String cname = xname + ".nth";
	    if (child(cname) != null) return;
	    try {
		new IntControl(this, cname, 1);
	    } catch (Xcept e) {
	    	importXMLMessage("Error creating control " + cname);
	    }
	}
	
	// add job info
	public void addJobInfo(NamedVal.NList nvals) throws Xcept {
	    // Grid storage
	    nvals.setVal("memory.storeGrids", storeGrids.val());
	    for (int i=0; i<domains.size(); i++) {
		String cname = domains.get(i) + ".nth";
		IntControl xcntl = (IntControl) child(cname);
		if (xcntl == null) continue;
		nvals.setVal("memory." + cname, xcntl.val());
	    } 

	    // scheme NOT YET IN USE
/*	    int scheme = storeScheme.val();
	    String sexprs = null;
	    if (scheme == SELECTED) {
	    	sexprs = storeExprs.val();
	    } else if (scheme == PROJECT) {
	    	scheme = SELECTED;
		sexprs = getRequestedVars();
	    }
	    nvals.setVal("memory.storeScheme", scheme);
	    if (! Util.isBlank(sexprs))
	    	nvals.setVal("memory.storeExprs", sexprs);
*/
	}

	// add requested
	private String getRequestedVars() throws Xcept {
	    return null;
	}

	// simple query
	public String xmlLabel() { return "memory"; }
	public String diagInfo() { 
	    return "Model " + parent().name() + " memory";
	}
	public StringList domains() { return domains; }
	public IntControl nthControl(String x) {
	    return (IntControl) child(x + ".nth");
	}
}

	
