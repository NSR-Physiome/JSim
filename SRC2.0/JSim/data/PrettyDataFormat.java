/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// write data in pretty table/cell format

package JSim.data;

import JSim.util.*;
import java.io.*;
import java.net.URL;

public class PrettyDataFormat extends DataFormat {

	// constructor 
	public PrettyDataFormat() { 
	    super();
	}

	// short name (1 word) for format
	public String shortName() { return "pretty"; }

	// long name (several words) for format
	public String longName() { 
	    return "pretty tables";
	}

	// allowed file suffixes, 1st is default
	public String[] suffixes() { 
	    return new String[] { "pdata" };
	}

	// URL for format specification (or null)
	public URL url() { return null; }

	// is content representable by String?
	public boolean isText() { return true; }

	// write support
	public DataWriter createWriter() {
	    return new PrettyDataWriter(this);
	}
}
