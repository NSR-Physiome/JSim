/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// control parameter for project

package JSim.project;

import java.io.PrintStream;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.CharacterData;

import JSim.util.*;
import JSim.data.*;

public abstract class Control extends PNamed implements DiagInfo {

	// state
	protected ControlVar cntlVar;  // if any

	// constructor
	public Control(PNamed p, String n) throws Xcept {
	    super(p, n);

	}

	// NamedVal based pseudo-constructor
	public static Control create(PNamed parent, NamedVal nval, 
	String name, String[] labels) throws Xcept {
	    if (name == null) name = nval.name();
	    if (nval instanceof NamedVal.Bool) 
	    	return new BooleanControl(parent, name, nval.boolVal());
	    if (nval instanceof NamedVal.Real) 
	    	return new RealControl(parent, name, nval.realVal());
	    if (nval instanceof NamedVal.Str) {
	    	StringList slist = (labels == null) ? 
		    null : new StringList(labels);
	    	return new StringControl(parent, name, 
		    nval.stringVal(), slist);
	    }
	    if (! (nval instanceof NamedVal.Int)) throw new Xcept(nval,
	    	"Control.create(): Unsupported NamedVal subclass " + 
		nval.getClass());
	    if (labels == null)
	    	return new IntControl(parent, name, nval.intVal());
	    else
	    	return new ChoiceControl(parent, name, nval.intVal(), labels);	
	}

	// subclass specific methods
	abstract public String stringVal();
	abstract public String stringDef();
	abstract public void setVal(String s) throws Xcept;

	// consequences of value update to this Control
	public void update() throws Xcept {
	    parent.childChanged(this);
	    updateOther();
	}

	// set defaults
	public void setDefaults() throws Xcept { 
	    super.setDefaults();
	    setVal(stringDef());
	}

	// consequences of value update to other PNamed
	public void updateOther() throws Xcept { }

	// query
	public String diagInfo() { return "Control " + fullname(); }
	public String xmlLabel() { return "control"; }
	public double realVal() throws Xcept { 
	    throw new Xcept("Real-valued control expected"); }
	public boolean boolVal() throws Xcept { 
	    throw new Xcept("Boolean control expected"); }
	abstract int dataType();
	public ControlVar cntlVar() throws Xcept { 
	    if (cntlVar == null)
	    	cntlVar = new ControlVar(this);
	    return cntlVar; 
	}

	// XML export
	public void exportExtraXML(Element e) {
	    e.setAttribute("value", stringVal());
	}

	// XML input
	public void importXML(Element e) {
	    super.importXML(e);
	    String s = e.getAttribute("value");
		// Convert XML based SemSim annotations to string:
	    if( e.getAttribute("name").equals("semSimAnnotate")) {
			s = allTextAllChildNodes(e);
			// Get rid of extra xml control label:
			s = s.replace("<control name=\"semSimAnnotate\">","");
			s = s.trim();
			s = s.replace("</control>","");

			if (s.length()<1) s = SemSimControl.NO_SEMSIM_ANNOTATE;
				
	    }
	    if (s.length() == 0) s = allText(e); // Call checkChildNodes in PNamed.java and create string
	    try { 
		setVal(s); 
//		importXMLMessage(fullname() + " set to \"" + s + "\"");
	    } catch (Xcept x) {
		importXMLMessage(x.cleanMessage());
	    }
	}   



}

