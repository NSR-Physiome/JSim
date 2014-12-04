/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// NSR .tac file

package JSim.data;

import java.io.*;
import java.util.*;
import java.net.URL;

import JSim.util.*;

public class TacDataFormat extends DataFormat {

	// constructor
	public TacDataFormat() { 
	    super();
	}

	// short name (1 word) for format
	public String shortName() { return "TAC"; }

	// long name (several words) for format
	public String longName() { 
	    return "NSR time-activity curve";
	}

	// allowed file suffixes, 1st is default
	public String[] suffixes() { 
	    return new String[] { "tac" };
	}

	// URL for format specification (or null)
	public URL url() { return null; }

	// is content representable by String?
	public boolean isText() { return true; }

	// read support
	public boolean readSupported(boolean hfAccess) { 
	    return true; 
	}
	public DataReader createReader(Reader rdr) throws Xcept {
	    return new TacDataReader(this, rdr);
	}

	// write support
	public DataWriter createWriter() {
	    return new TacDataWriter(this);
	}
}
