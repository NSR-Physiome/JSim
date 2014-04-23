/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plug-in graphic

package JSim.project;

import JSim.util.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class PGraphic extends PNamed {
	public static final String K_STATE = "graphicState";
	public StringControl variant;
	public Element state; // used by graphic plugin

	// constructor
	public PGraphic(PNamed p, String n) throws Xcept {
	    super(p, n);
	    addDescCntl();
	    variant = new StringControl(this, "variant");
	}

	// set state
	public void setGraphicState(Element e) {
	    state = e;
	}

	// import child
	public PNamed importXMLChild(Element c) {
	    if (! c.getNodeName().equals(K_STATE))
	    	return super.importXMLChild(c);
	    state = c;
	    return null;
	}

	// export state
	public void exportExtraXML(Element e) {
	    Document doc = e.getOwnerDocument();
	    if (state == null)
	    	state = doc.createElement(K_STATE);
	    Element e1 = (Element) doc.importNode(state, true);
	    e.appendChild(e1);
	}

	// query
	public String diagInfo() { return "PGraphic " + name; }
	public String xmlLabel() { return "graphic"; }
	public Element getGraphicState() { return state; }
}

