/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Set of images in project

package JSim.project;

import java.io.*;
import JSim.util.*;
import org.w3c.dom.Element;


public class PImageSet extends PNamed {

	// controls

	// constructor
	public PImageSet(PNamed p, String n) throws Xcept {
	    super(p, n);
//	    new PImage(this, "exp", new File("exp.gif"));
	}

	// import XML Element
	public PNamed importXMLChild(Element c) {

	    // only PImages get special treatment
	    String ctype = c.getNodeName();
	    if (ctype != "image") {
		super.importXMLChild(c);
		return null;
	    }

	    // create new PImage
	    String cname = c.getAttribute("name");
	    try {
		PNamed child = new PImage(this, cname);
	    	child.importXML(c);
		return child;
	    } catch (Xcept x) {
		importXMLMessage("Error in PImage constructor" + 
		     x.cleanMessage());
	    }
	    return null;
	}

	// list of image names
	public String[] imageNames() {
	    PNamed.List list = children(PImage.class);
	    String[] names = new String[list.size()];
	    for (int i=0; i<list.size(); i++)
	    	names[i] = ((PImage) list.pnamed(i)).name();
	    return names;
	}
	
	// simple query
	public String diagInfo() { return "Image Set " + name; }
	public String xmlLabel() { return "imageset"; }
	public PImage pimage(String n) {
	    if (child(n) instanceof PImage)
		return (PImage) child(n);
	    return null;
	}
}

