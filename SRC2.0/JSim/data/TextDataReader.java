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

public abstract class TextDataReader extends DataReader {

	// constructor
	public TextDataReader(DataFormat f, Reader r) {
	    super(f, r);
	    if (! (reader instanceof LineNumberReader)) 
		reader = new LineNumberReader(reader);
	}

	// query
	public LineNumberReader rdr() {
	    return (LineNumberReader) reader;
	}
	public String diagInfo() {
	    return super.diagInfo() + " line #" + 
		rdr().getLineNumber();
	}
}

