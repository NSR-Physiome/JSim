/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// timed process query 

package JSim.util;

import java.io.*;

public class ProcessQuery extends Thread {
	private Process proc;
	private long sleepTime;
	private boolean done;

	// constructor
	public ProcessQuery(Process p, int t) {
	    super("ProcessQuery thread");
	    proc = p;
	    sleepTime = t;
	    start();
	}

	// run method
	public void run() {
	    while (!done) {
		try {
	    	    if (dump(proc.getInputStream(), System.out)) continue;
	    	    if (dump(proc.getErrorStream(), System.err)) continue; 
		    int stat = proc.exitValue();
		    done = true;
		    continue;
		} catch (Exception e) { }
		try {
		    Thread.sleep(sleepTime);
		} catch (Exception e) { }
	    }
	}

	// dump a stream,  return if anything available
	public boolean dump(InputStream in, OutputStream out)
	throws IOException {
	    boolean working = false;
	    while (in.available() > 0) {
		working = true;
		out.write(in.read());
	    }
	    return working;
	}

	// test mainline
	public static void main(String[] args) throws Exception {
	    String cmd = args[0];
	    System.err.println("executing " + cmd);
	    String[] cmds = new String[] { cmd };
	    Process proc = Runtime.getRuntime().exec(cmds);
	    Thread query = new ProcessQuery(proc, 100);
	    query.join();
	    int stat = proc.exitValue();
	    System.err.println("exit status=" + stat);
	}
}

