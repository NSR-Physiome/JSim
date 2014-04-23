/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// PModel variables 
//	contains run-time assigns, FuncGens and solver settings

package JSim.project; import JSim.aserver.*;

import java.io.PrintStream;
import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*;

public class PModelVars extends PNamed implements NameSpace {
	private PSolverSettings solver; // solver settings
	private PModelMemory memory;  // memory controls
	private PModelAssn assign;	// current PModel assignments
	protected boolean funcGenNamesChanged; // if add/rename/delete

	// constructor
	public PModelVars(PNamed p, String n) throws Xcept {
	    super(p, n);
	    funcGenNamesChanged = false;
	    solver = new PSolverSettings(this, "solver");
	    memory = new PModelMemory(this, "memory");
	    rollover(null);
	}

	// rollover rtmodel
	public void rollover(ASModel rt) throws Xcept {
	    assign = new PModelAssn(this, name, rt);
	}   

	// nested child
	public PNamed nestedChild(String n) {
	    String a = assign.name();
	    int alen = a.length();
	    if (n.equals(a)) return assign;
	    if (n.length()>alen+1 && 
		a.equals(n.substring(0,alen)) &&
		n.charAt(alen) == '.') 
		return assign.nestedChild(n.substring(alen+1));
	    return super.nestedChild(n);
	}

	// query
	public String diagInfo() { return "PModelVars " + name; }
	public String xmlLabel() { return "vars"; }
	public PSolverSettings solver() { return solver; }
	public PModelMemory memory() { return memory; }
	public PModelAssn assign() { return assign; }

	// PModel called XML export: only FuncGen stubs
	public Element exportXML(Document doc) throws Xcept { 
	    Element e = doc.createElement(xmlLabel());
	    e.setAttribute("name", name());
	    for (int i=0; i<nChild(); i++) {
		PNamed child = child(i);
		if (! (child instanceof FuncGen)) continue;
		Element e1 = doc.createElement("function");
		e1.setAttribute("name", child.name());
		e.appendChild(e1);
	    }
      	    return e;
	}

	// PModel called XML import: only FuncGen stubs
	public void importXML(Element e) {
	    if (! importXMLLabelMatch(e)) return;
	    NodeList nlist = e.getChildNodes();
	    for (int i=0; i<nlist.getLength(); i++) {
		if (! (nlist.item(i) instanceof Element)) continue;
		Element c = (Element) nlist.item(i);
	    	String ctype = c.getNodeName();
		if (! ctype.equals("function")) continue;
	    	String cname = c.getAttribute("name");
		if (child(cname) != null) continue;
	    	try {
		    new FuncGen(this, cname);
	    	} catch (Xcept x) {
		    importXMLMessage("Internal Xcept  in XML " + 
		    ctype + " constructor" +  x.cleanMessage());
		}
	    }
	}

	// get control
	public Control control(String n)  {
	    PNamed c = assign.nestedChild(n);
	    if (c == null) c = super.nestedChild(n);
	    if (c instanceof Control) return (Control) c;
	    return null;
	}

	// parse Controls (for RTML visibility Expr only)
	//   no units, unknowable at RTML parse 
	public Expr parseControlExpr(String s) throws Xcept {
	    if (funcGenNamesChanged)
	    	updateFuncGenNames();
	    return Expr.parse(this, s);
	}
 
	// NameSpace: find Expr for name
	public Expr compByName(String name) throws Xcept {

	    // variable must be AssignControl
	    PNamed c = assign.child(name);
	    if (c == null) c = nestedChild(name);
	    if (c == null) throw new Xcept(pmodel(),
		"No such variable \"" + name + "\"");
	    if (c instanceof Control) {
		ControlVar v = ((Control) c).cntlVar();
		if (v != null) return v;
	    }
	    throw new Xcept(pmodel(),
		"Unknown control variable \"" + name + "\"");
	}

	// NameSpace: trivial callbacks
	public Unit unitByName(String s) throws Xcept { 
	    return null; // units not knowable at RTML parse
	}
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
	    throw new Xcept(
	       "Derivatives not supported in RTML expressions");
	}
	public Expr funcCall(String name, Expr.List elist) throws Xcept { 
	    throw new Xcept(
	       "Function calls not supported in RTML expressions");
	}

	// set to default values
	public void setDefaults() throws Xcept {
	    super.setDefaults();
	    assign.setDefaults();
	}	    

	// send funcGenNames array to server
	public void updateFuncGenNames() throws Xcept {
	    StringList fgenNames = new StringList();
	    for (int i=0; i<nChild(); i++) {
	    	PNamed p = child(i);
		if (p instanceof FuncGen)
		    fgenNames.add(p.name());
	    }
	    pmodel().rt().setFuncGenNames(fgenNames.array());
	    funcGenNamesChanged = false;
	}

 	// solver & fgen parameters
	public void addJobInfo(NamedVal.NList nvals) 
	throws Xcept {
	    for (int i=0; i<nChild(); i++) {
	    	PNamed p = child(i);
		if (! (p instanceof PSolverSettings) &&
		    ! (p instanceof PModelMemory) &&
		    ! (p instanceof FuncGen)) continue;
		addNamedVals(p.name(), nvals, p);
	    }
	}
	
	// add named val controls to list
	private void addNamedVals(String pfx, NamedVal.NList nvals, 
	PNamed parent) throws Xcept {
	    for (int i=0; i<parent.nChild(); i++) {
	    	PNamed p = parent.child(i);
		String pname = pfx + "." + p.name();
		if (p instanceof Control) {
		    NamedVal nval = p.namedVal(pname);
		    if (nval != null)
		    	nvals.add(nval);
		}
		addNamedVals(pname, nvals, p);
	    }
	}

	// parset modified
	public void childChanged(PNamed c) throws Xcept {
//System.err.println("PModelVars.childChanged(" + c + ")");
	    pmodel().setParsModified(true);
	    super.childChanged(c);
	}
}

