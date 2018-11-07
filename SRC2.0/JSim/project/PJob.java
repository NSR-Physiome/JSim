/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// An batch job

package JSim.project; 

import JSim.aserver.*;
import JSim.util.*;
import JSim.data.*;

import java.io.*;

abstract public class PJob extends Thread {

	// stat values
	public static final int UNSAFE = -1; // term by kill
	public static final int RUNNING = 0;
	public static final int NORMAL = 1; // normal completion
	public static final int XCEPT = 2; // term by Xcept
	public static final int NOMEMORY = 3; // term by OutOfMemoryError
	public static final int OTHER = 4; // term by Exception

	// killStat values
	public static final int DISABLED = 0; // cannot kill 
	public static final int ENABLED = 1;  // may kill
	public static final int INPROGRESS = 2; // kill in progress

	// internal state
	private int jobCode;    // see ASModel
	private int stat;	// see above
	private int killStat;	// see above
	private String errMsg;
	private PNamed pnamed;
	private boolean stackTrace; // print stack trace on Exception?
	private long startTime;
	private long stopTime;
	protected NamedVal.NList baseVals;

	// constructor
	public PJob(PNamed p, int jcode) {
	    super();
	    pnamed = p;
 	    jobCode = jcode;
	    stat = UNSAFE;
	    errMsg = null;
	    stackTrace = false;
	    killStat = DISABLED;
	}

	// set stack dump
	public void setStackTrace(boolean b) { stackTrace = b; }

	// simple blocking run in this thread
	public void simpleRun() throws Xcept {
	    run();
	    postRun();
	    if (stat() != PJob.NORMAL) throw new Xcept(
		pnamed, termMessage());
	}

	// run
	final public void run() {
	    try {
		startTime = System.currentTimeMillis();
		stoppable();
		stat = NORMAL;
	    } catch (Xcept e) {
		errMsg = xceptMessage(e);
		stat = XCEPT;
		if (stackTrace) e.printStackTrace();
	    } catch (OutOfMemoryError e) {
		errMsg = "insufficient memory available,  see documentation";
		stat = NOMEMORY;
		if (stackTrace) e.printStackTrace();
	    } catch (Exception e) {
		errMsg = e.toString();
		stat = OTHER;
		if (stackTrace) e.printStackTrace();
	    }
	    stopTime = System.currentTimeMillis();
	}

	// call when user requests cancel
	public void cancel() { 
	    pmodel().rt().cancelJob();
	}

	// call when user requests next loop
	public void nextLoop() { 
	    pmodel().rt().nextLoop();
	}

	// stoppable (dangerous code)
	abstract public void stoppable() throws Xcept;

	// execute after run in monitor thread
	abstract public void postRun() throws Xcept;

	// process Xcept message (single line)
	public String xceptMessage(Xcept e) {
	    return e.cleanMessage();
	}

	//  query
	public String termMessage() {
	    switch(stat) {
	    case NORMAL: 
		return "terminated normally";
	    case UNSAFE: 
		return "killed by user";
	    default: 
		return "aborted: " + errMsg;
	    }	
	}

		// Write out to file in case next run crashes:
	public boolean backup() throws Xcept { 
			try {
			PModel currModel = this.pmodel();
			Project project = currModel.project();
			File file = new File(pnamed.name()+"_LastRun.proj");  // change name? ....
			// write project file
			OutputStream out = new FileOutputStream(file);
			PrintStream pout = new PrintStream(out);
			project.writeXML(pout);
			pout.close();
			}
			catch (FileNotFoundException e) {
				throw new Xcept("Unable to backup current project: "+ e.getMessage());
			} catch (SecurityException e) {
				throw new Xcept(
					"Backup file save operation not allowed:  " + e.getMessage()+ ".");
			}
		return true;			
	}

	public PModel pmodel() { return (PModel) pnamed; }
	public int stat() { return stat; }
	public int jobCode() { return jobCode; }
	abstract public String phaseDesc();
	abstract public double phaseFrac(); 
	public long runningTime() {  // in msec
	    long time = (stopTime == 0) ? 
		System.currentTimeMillis() : stopTime;
	    return time - startTime;
	}
	public NamedVal.NList baseVals() { return baseVals; }

	// estimate remaining time (msec), -1 if can't
	public long remainingTime() { 
	    if (startTime == 0) return -1;
	    if (stopTime != 0) return 0;
	    double frac = phaseFrac();
	    if (frac <= 0) return -1;
	    if (frac >= 1) return 0;
	    long currTime = System.currentTimeMillis();
	    return (long) 
	    	((currTime - startTime) * (1-frac) / frac);
	}

	// get job status
	public ASInfo.Status getJobStatus() {
	    try {
	    	return pmodel().rt().getJobStatus();
 	    } catch (Exception e) {
	    	return null;
  	    }
	}

	// PJob sets whether currently killable or not
	public void setKillable(boolean b) throws Xcept {
	    if (killStat == INPROGRESS) throw new Xcept(
		"user requested job kill in progress");
	    killStat = b ? ENABLED : DISABLED;
	}

	// PJob Monitor request permission to kill
	public boolean startKill() {
	    cancel();  // request nice cancel first
	    if (killStat == DISABLED) return false;
	    killStat = INPROGRESS;
	    return true;
	}

	// set run saveExprs
	public void setSaveExprs(StringList list) throws Xcept {
	    if (baseVals == null) throw new Xcept(
	    	"This job can't setSaveExprs(): " + getClass());
	    String s = list.toString(";", false);
	    baseVals.setVal("memory.saveExprs", s);
	    System.err.println("Setting memory.saveExprs=" + s);
	}

}	
