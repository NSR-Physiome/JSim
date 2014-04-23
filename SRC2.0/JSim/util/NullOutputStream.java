/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// write nothing output

package JSim.util;

import java.io.*;

public class NullOutputStream extends OutputStream {

	// constructor
	public NullOutputStream() {
	    super();
	}

	// methods
	public void close() { }
	public void flush() { }
	public void write(byte[] b) { }
	public void write(byte[] b, int off, int len) { }
	public void write(byte b) { }
	public void write(int i) { }

	    
}