/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// reader for XMLDataFormat

package JSim.data;

import java.io.*;
import java.util.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import JSim.util.*;

abstract public class XMLDataReader extends DataReader {

	// constructor
	public XMLDataReader(XMLDataFormat f, Reader r) 
	throws Xcept {
	    super(f, r);
	}

	// read data
	final public Data.List readData() throws Xcept {
	    String s = UtilIO.readText(reader);
	    Document doc = UtilXML.parse(s);
	    return importXML(doc);
	}

	// customizable import 
	public Data.List importXML(Document doc) throws Xcept {
	    Element e = doc.getDocumentElement();
	    if (! e.getNodeName().equals(rootName())) throw new Xcept(this,
		"root document element name must be " + rootName());
	    return importXML(e);
	}

	// customizable import 
	abstract public Data.List importXML(Element e)
	throws Xcept;

	// query
	public String rootName() {
	    return ((XMLDataFormat) format).rootName();
	}
}
