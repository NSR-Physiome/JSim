/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time access to the Matlab engine

package JSim.jruntime.matlab;

public class RTXMatlab {

	// constructor
	public RTXMatlab() throws Exception { 
	    connect();
	}

	// connect to engine
	public native void connect() throws Exception;

	// set variable in engine
	public native void setValue(String var, double[][] data)
	throws Exception;

	// run a single command
	public native void runCommand(String cmd) 
	throws Exception;

	// get variable from engine
	public native double[][] getValue(String var) throws Exception;
}


