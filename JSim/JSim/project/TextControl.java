/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Multi-line text control parameter for project

package JSim.project;

import java.io.PrintStream;
import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.Text;
import org.w3c.dom.CDATASection;

public class TextControl extends StringControl {

	// constructor
	public TextControl(PNamed p, String n) throws Xcept {
	    super(p, n);
	    value = "";
	}

	// XML export
	public void exportExtraXML(Element e) {
	    Document doc = e.getOwnerDocument();
//	    CDATASection cdata = doc.createCDATASection(value);
	    Text text = doc.createTextNode(value);
	    e.appendChild(text);
	}

}

