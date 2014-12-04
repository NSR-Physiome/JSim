/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// error/warning/message processing for compiler

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 

import java.io.*;
import java.util.*;

public class Logger {
	public StringList msgs; // all messages
	public StringList alerts; // warnings & errors
	public StringList errors; // errors only
	protected ArrayList<SeqGraph> graphs; // saved graphs
	private int nwarnings;  // # warnings
	
	public static final int DEBUG = 0;
	public static final int WARN = 1;
	public static final int ERROR = 2;

	// logging control
	public PrintStream out; // concurrent output stream
	private int timeout;  // secs
	private long maxTime; // max time (in millis) to abort
	private String vtags;

	// constructor
	public Logger()  {
	    msgs = new StringList();
	    alerts = new StringList();
	    errors = new StringList();
	    // out = System.err;
	}

	// timeout
	public void setTimeout(int secs) {
	    timeout = secs;
	    maxTime = System.currentTimeMillis() + secs*1000L;
	}
	protected void checkTimeout() throws Xcept {
	    if (maxTime == 0
	    || System.currentTimeMillis() < maxTime) return;
	    throw new Xcept(
		"Compiler timed out at " + timeout + " sec");
	}
	public void setStream(PrintStream out) {
	    this.out = out;
	}
	protected void setSaveGraphs(boolean b) {
	    if (b)
	    	graphs = new ArrayList<SeqGraph>();
	    else
	    	graphs = null;
	}

	// verbose
	public void setVerbose(String vtags) { 
	    this.vtags = vtags; 
	}
	public boolean isVerbose() { 
	    return vtags != null && vtags.length() == 0; 
	}
	public boolean isVerbose(String s) {
	    if (isVerbose()) return true;
	    return vtags != null && vtags.indexOf(s) >= 0;
	}

	public void log(String msg) { log(DEBUG, msg); }
	public void warn(String msg) { log(WARN, msg); }
	public void error(String msg) { log(ERROR, msg); }

	public void log(String tag, String msg) {
	    if (out == null) return;
	    if (! isVerbose(tag)) return;
	    out.println(msg);
	}

	public void log(Exception e) {
	    String msg = e.getMessage();
	    if (! (e instanceof AbortXcept)) {
	        msg = " (internal?) " + msg;
		if (out != null)
	    	    e.printStackTrace(out);
	    }
	    error(msg);
	}    	

	public void log(int code, String msg) { 
	    switch (code) {
	    case ERROR:
		errors.add(msg);
	    	msg = "ERROR: " + msg;
		break;
	    case WARN:
		nwarnings++;
	    	msg = "WARNING: " + msg;
		break;
	    }
	    msgs.add(msg);
	    if (code != DEBUG) alerts.add(msg);
	    if (out != null && (isVerbose() || code != DEBUG)) 
	        out.println(msg);
	}

	// save a graph
	protected void saveGraph(String name, SeqGraph graph) 
	throws Xcept {
	    if (graphs == null) return;
	    log("Saving graph " + name);
	    graph = new SeqGraph(graph); // make copy
	    graph.setName(name);
	    graphs.add(graph);
	}

	// query
	public int nwarnings() { return nwarnings; }
	public int nerrors() { return errors.size(); }
}
