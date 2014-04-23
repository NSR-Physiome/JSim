/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// read instance of a DataFormat

package JSim.data;

import JSim.util.*;
import java.io.*;

public abstract class DataReader implements DiagInfo {

	// state
	protected DataFormat format;
	protected Reader reader;
	private String fileName; // or other source, for diags

	// constructor
	public DataReader(DataFormat f, Reader r) {
	    format = f;
	    reader = r;
	    fileName = null;
	}

	// file name
	public String fileName() { 
	    return (fileName==null) ? "unknown" : fileName; 
	}
	public void setFileName(String s) { fileName = s; }

	// query
	public String diagInfo() {
	    return format.shortName() + " reader";
	}

	// read data
	abstract public Data.List readData() throws Xcept;
}

