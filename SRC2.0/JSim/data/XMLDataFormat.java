/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XML based data format

package JSim.data;

import JSim.util.*;
import java.io.*;
import java.net.URL;
import org.w3c.dom.Document;

public abstract class XMLDataFormat extends DataFormat {

	// constructor
	public XMLDataFormat() { 
	    super();
	}

	// allowed file suffixes, 1st is default
	public String[] suffixes() { 
	    return new String[] { "xml" };
	}

	// is content representable by String?
	public boolean isText() { return true; }

	// name of root element
	abstract public String rootName();

}
