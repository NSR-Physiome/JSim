/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataWriter for XMLDataFormat

package JSim.data;

import JSim.util.*;

import java.io.*;
import java.net.URL;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

abstract public class XMLDataWriter extends DataWriter {

	// constructor
	public XMLDataWriter(XMLDataFormat f) { 
	    super(f);
	}

	// write data
	public void writeData(Writer wrt, Data.List data) throws Xcept {
	    Document doc = UtilXML.createDoc(rootName());
	    Element root = doc.getDocumentElement();
	    exportXML(doc, root, data);
	    XMLWriter xmlWriter = new XMLWriter();	    
	    xmlWriter.write(doc, wrt);
	}

	// customized export (write) support
	abstract public void exportXML(Document doc, Element e, 
	Data.List dlist) throws Xcept;

	// query
	public String rootName() {
	    return ((XMLDataFormat) format).rootName();
	}
}
