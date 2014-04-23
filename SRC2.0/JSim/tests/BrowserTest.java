/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test model browser

package JSim.tests;

import java.io.*;
import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import org.w3c.dom.*; 

public class BrowserTest {

	// mainline
	public static final void main(String[] args) throws Exception {
//	    writeTest(args[0]);
//	    roundtripTest(args[0], args[1]);
	}


	// read Layout file, then write
	private static void roundtripTest(String inName, String outName) 
	throws Exception {

	    // read layout from disk
	    Document doc = UtilXML.parse(new File(inName));
	    Element root = doc.getDocumentElement();
	    PModelBrowserLayout l = new PModelBrowserLayout(null);
	    l.importXMLForce(root);
	    
	    // write to disk
	    l.writeXML(new File(outName));
	}

	// write dummy Layout file
	private static void writeTest(String fileName) throws Exception {

	    // create dummy Layout
	    PModelBrowserLayout l = new PModelBrowserLayout(null);
	    PModelBrowserLayout.Graph g = l.addNewGraph("vars");
	    g.putXY("c", 33.333f, 11.1f);
	    g.putXY("b", 22.333f, 11.1f);
	    g.putXY("a", 11.333f, 11.1f);
	    g = l.addNewGraph("seq");
	    g.putXY("bb", 22.333f, 22f);
	    g.putXY("aa", 11.333f, 22f);
	    g.putXY("dd", 44.333f, 22f);
	    g.putXY("cc", 33.333f, 22f);
	    
	    // write to disk
	    l.writeXML(new File(fileName));
	}
}

