/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// write data in Matlab .m file format

package JSim.data;

import JSim.util.*;
import java.io.*;
import java.net.URL;

public class MatlabDataFormat extends DataFormat {

	// constructor 
	public MatlabDataFormat() { 
	    super();
	}

	// short name (1 word) for format
	public String shortName() { return "matlab"; }

	// long name (several words) for format
	public String longName() { 
	    return "matlab";
	}

	// allowed file suffixes, 1st is default
	public String[] suffixes() { 
	    return new String[] { "m" };
	}

	// URL for format specification (or null)
	public URL url() { return null; }

	// is content representable by String?
	public boolean isText() { return true; }

	// write support
	public DataWriter createWriter() {
	    return new MatlabDataWriter(this);
	}
}
