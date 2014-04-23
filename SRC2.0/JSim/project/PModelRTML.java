/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// PModel RTML processing (also see JSim.gui.GModel)
//    

package JSim.project; import JSim.aserver.*;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class PModelRTML extends PNamed {
	private static boolean legacy = true; // save in legacy mode
	private Element rtml;

	// constructor
	public PModelRTML(PModel p, String n) throws Xcept {
	    super(p, n);
	}

	// simple query
	public String diagInfo() { return "PModelRTML " + name(); }
	public String xmlLabel() { return "rtml"; }

	// return RTML element
	public Element rtml() throws Xcept { 
	    if (rtml != null) return rtml;

	    // parse legacy text RTML
	    String text = pmodel().customRTMLText.val();
	    if (Util.isBlank(text)) return null;
	    Document doc = UtilXML.parse(text);
	    rtml = compactRoot(doc);
	    pmodel().customBuilt.setVal(true);
	    if (! legacy) 
		pmodel().customRTMLText.setVal("");
	    return rtml;
	}    

	// import from .rtml file
	public void importRTML(File f) throws Xcept {
	    // clear old values
	    rtml = null;
	    pmodel().customRTMLText.setVal("");
	    String text = UtilIO.readText(f);

	    // legacy/non-legacy updates
	    if (legacy) {
		pmodel().customRTMLText.setVal(text);
	    } else {
		Document doc = UtilXML.parse(text);
	    	rtml = compactRoot(doc);
	    }
	}

	// delete RTML
	public void deleteRTML() throws Xcept {
	    rtml = null;
	    pmodel().customRTMLText.setVal("");
	}

	// import project loaded XML
	public void importXML(Element e) {
	    rtml = null;
	    if (e == null) return;
	    if (e.getChildNodes().getLength() == 0) return;
	    rtml = compactRoot(e); 
	}

	// export to project file, or other existing Element
	public void exportExtraXML(Element e) throws Xcept {
	    Document doc = e.getOwnerDocument();
	    if (legacy || rtml == null) return;
	    NodeList nodes = rtml.getChildNodes();
	    int ct = nodes.getLength();
	    for (int i=0; i<ct; i++) {
		Node n = nodes.item(i);
		if (! (n instanceof Element)) continue;
	    	e.appendChild(doc.importNode(n, true));
	    }
	}

	// export to .rtml file
	public void exportRTML(File f) throws Xcept {
	    try {
	    	PrintStream out = new PrintStream(
		    new FileOutputStream(f));
	    	exportRTML(out);
	    	out.close();
	    } catch (FileNotFoundException e) {
		throw Xcept.wrap(e);
	    }
	}

	// export to PrintStream
	public void exportRTML(PrintStream out) throws Xcept {
	    if (rtml() == null) return;
	    XMLWriter xwrt = new XMLWriter();
	    xwrt.write(rtml(), out);
	}

	// compacted root of document
	//    can't do this in place for unknown reasons
	//    so make 2nd copy
	private Element compactRoot(Document doc) {
	    return compactRoot(doc.getDocumentElement());
	}
	private Element compactRoot(Element root1) {
	    Document doc2 = UtilXML.createDoc("JSim");
	    Element root2 = doc2.getDocumentElement();
	    NodeList nodes = root1.getChildNodes();
	    int ct = nodes.getLength();
	    for (int i=0; i<ct; i++) {
		Node n1 = nodes.item(i);
		if (! (n1 instanceof Element)) continue;
		Element n2 = (Element) doc2.importNode(n1, true);
		compact(n2);
	    	root2.appendChild(n2);
	    }
	    return root2;
	}

	// compact RTML spacing
	private void compact(Element elem) {
	    NodeList nodes = elem.getChildNodes();
	    int ct = nodes.getLength();
	    for (int i=0; i<ct; i++) {
		Node n = nodes.item(i);
		if (n == null) continue;
		switch(n.getNodeType()) {
		case Node.ELEMENT_NODE:
		    compact((Element) n);
		    break;
		case Node.ATTRIBUTE_NODE:
		    break;
		default:
		    elem.removeChild(n);
		    break;
		}
	    }
	}
}
