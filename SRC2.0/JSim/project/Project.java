/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// JSim Project (top) object

package JSim.project; import JSim.aserver.*;

import java.io.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.data.*;

public class Project extends PNamed {

	public boolean changed; // project change flag
	private PApplication appl; // application controlling project

	// top-level project constructor
	public Project(String n, PApplication a) throws Xcept {
	    super(null, n);
	    appl = a;
	    appl.addProject(this);
	}

	// a child has been added or changed
	public void childChanged(PNamed child) {
	    changed = true;
	}

	// import content from file or URL
	public PNamed load(JSReadable readable) throws Xcept {
	    return load(readable, null);
	}
	public PNamed load(JSReadable readable, String options) 
	throws Xcept {
	    String n = loadName(readable);
	    if (readable.isModel()) {
	        String src = appl().readSource(readable, options);
	    	return new PModel(this, readable, src, false);
	    }
	    if (readable.isDataSet()) {
		Data.List dlist = appl().readData(readable);
	    	return new PDataSet(this, readable, dlist);
	    }
	    if (readable.isParSet()) 
	    	return new ParSet(this, readable);
	    throw new Xcept(readable,
	    	"Unsupported file suffix");
	}

	// name for child from readable
	protected String loadName(JSReadable readable) throws Xcept {
	    String n = readable.fileBaseName();
	    if (n.length() > 15) n = n.substring(0,15);
	    n = safeName(n);
	    return project().newChildName(n, false);
	} 

	// accumulate model save exprs
	public void addModelExprs(PModel pmodel, StringList exprs) {
	    for (int i=0; i<nChild(); i++) {
	    	PNamed child = child(i);
		if (child instanceof PlotPage 
		|| child instanceof PNested)
	    	    child.addModelExprs(pmodel, exprs);
	    }
 	}

	// query
	public String diagInfo() { return "Project " + name; }
	public String xmlLabel() { return "project"; }
	public ASServer server() { return appl().server(); }
	public PApplication appl() { return project().appl; }

	// import XML from file or URL
	public void importXML(JSReadable readable) throws Xcept {
	    String text = readable.readText();
	    Document doc = UtilXML.parse(text);
	    NodeList nodes = doc.getDocumentElement().getChildNodes();
	    Element e = null;
 	    for (int i=0; i<nodes.getLength(); i++) {
		if (nodes.item(i) instanceof Element) { 
		    e = (Element) nodes.item(i);
		    break;
		}
	    }
	    importXML(e);
	}	    

	// import XML Element
	public PNamed importXMLChild(Element c) {

	    // create blank child
	    String ctype = c.getNodeName();
	    String cname = c.getAttribute("name");
	    cname = newChildName(cname, false);
	    String ftype = c.getAttribute("type");
	    PNamed child = null;
	    try {
		if (ctype.equals("model")) 
		    child = new PModel(this, cname);
		else if (ctype.equals("parset")) 
		    child = new ParSet(this, cname);
		else if (ctype.equals("dataset")) 
		    child = new PDataSet(this, cname);
		else if (ctype.equals("plotpage")) 
		    child = new PlotPage(this, cname);
		else if (ctype.equals("nested")) 
		    child = new PNested(this, cname);
		else if (ctype.equals("graphic")) 
		    child = new PGraphic(this, cname);
		else if (ctype.equals("notes")) 
		    child = new PNotes(this, cname);
		else if (ctype.equals("imageset")) 
		    child = new PImageSet(this, cname);
	    } catch (Xcept x) {
		importXMLMessage("Internal Xcept  in XML " + 
		    ctype + " constructor" +  x.cleanMessage());
	    }
	    
	    if (child == null)
		importXMLMessage("Unrecognized XML tag during import:" + 
		ctype);
	    else 
		child.importXML(c);

	    return child;
	}
}

