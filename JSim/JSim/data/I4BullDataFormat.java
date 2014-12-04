/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// I4 Bullseye text data

package JSim.data;

import java.io.*;
import java.util.*;
import java.net.URL;

import JSim.util.*;

public class I4BullDataFormat extends DataFormat {

	// constructor
	public I4BullDataFormat() { 
	    super();
	}

	// short name (1 word) for format
	public String shortName() { return "i4bull"; }

	// long name (several words) for format
	public String longName() { 
	    return "I4 bullseye";
	}

	// allowed file suffixes, 1st is default
	public String[] suffixes() { 
	    return new String[] { "bul" };
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
	    return new I4BullDataReader(this, rdr);
	}

	// write support
	public DataWriter createWriter() {
	    return new I4BullDataWriter(this);
	}
}
