/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test read attributes ability

package JSim.tests; import JSim.tests.*;

import java.io.*;
import JSim.util.*;
import org.w3c.dom.*;

public class XMLAttributesTest {

	// constructor
	public XMLAttributesTest(File f) throws Exception {
	    Document doc = UtilXML.parse(f);
	    Element e = doc.getDocumentElement();
	    dump(e);
	}
	
	// dump and element
	public void dump(Element e) throws Exception {
	    System.out.println("Element " + e.getNodeName());
	    NamedNodeMap attrs = e.getAttributes();
	    for (int i=0; i<attrs.getLength(); i++) {
	    	Attr attr = (Attr) attrs.item(i);
		System.out.println("  " + attr.getName() 
		    + "=" + attr.getValue());
	    }
	    NodeList nodes = e.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
	    	Node node = nodes.item(i);
		if (node instanceof Element)
		    dump((Element) node);
	    }
	}
	
	// mainline
	public static void main(String[] args) throws Exception {
	    File f = new File(args[0]);
	    new XMLAttributesTest(f);
	}
}

