/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSML (JSim Markup Language) format

package JSim.data;

import java.io.*;
import java.util.*;
import java.net.URL;

import JSim.util.*;

public class JSMLDataFormat extends XMLDataFormat {

	// constructor
	public JSMLDataFormat() { super(); }

	// names
	public String shortName() { return "JSML"; }
	public String longName() { 
	    return "JSim Markup Language"; 
	}	

	// allowed file suffixes, 1st is default
	public String[] suffixes() { 
	    return new String[] { "jsml" };
	}

	// documentation URL
	public URL url() { return null; }

	// root element name
	public String rootName() { return "JSMLDataSet"; }

	// read support
	public boolean readSupported(boolean fsAccess) { 
	    return true; 
	}
	public DataReader createReader(Reader rdr) throws Xcept {
	    return new JSMLDataReader(this, rdr);
	}

	// write support
	public DataWriter createWriter() {
	    return new JSMLDataWriter(this);
	}
}
