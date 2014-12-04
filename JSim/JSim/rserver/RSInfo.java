/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// static server side status info

package JSim.rserver;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.rclient.*;

public class RSInfo implements Serializable {

	// RSServer info
	public static class Server implements Serializable {
	    // user-specified configuration
	    public String host;		// server host
	    public int port;		// RMI registry port
	    public int userPort;	// exported object port base
	    public String name;		// server name
	    public String version;      // JSim version
	    public boolean sandbox;  	// enable client sandbox
	    public File workDir;	// work directory
	    public boolean sharedJVM;	// share server JVM with clients?
	    public long maxConnectTime;	// max client connect time (msec)
	    public long pingFreq;	// client ping frequency 
	    public long purgeFreq;	// timeout for purging pingless clients
	    public long clientMemory;	// max client memory (MB)
	    public long maxBuildTime; 	// max model build time (msec)
	    public long maxRunTime;	// max model run time (msec)
	    public boolean stackTrace;  // print stackTrace on exception

	    // dynamic state/query
	    public long startTime;	// time server started
	    public long queryTime;	// time server queried
	    public long requestTime; 	// last request
	    public String jvmID; 	// in this JVM
	    public String[] clientIDs;	// current clientIDs
	    public int clientMax;	// max # concurrent clients
	    public int clientSeq;	// # clients since launch
	    public long currMemory;	// current memory usage (bytes)
	    public long maxMemory;	// max memory usage (bytes)

	    // display
	    public String toString() {
		int nclients = (clientIDs == null) ? 0 : clientIDs.length;
		long cmem = currMemory >> 20;
		long mmem = maxMemory >> 20;
	    	StringBuffer s = new StringBuffer(
		    "server URL:\t\t" + url() +	
		    "\nversion:\t\t" + version +	
		    "\nuser port base:\t\t" + 
		    	((userPort>0) ? ("" + userPort) : "inactive") + 
		    "\nwork directory:\t\t" + workDir.getAbsolutePath() + 	
		    "\nmax model build time:\t" + Util.timeDiffString(maxBuildTime) +	
		    "\nmax model run time:\t" + Util.timeDiffString(maxRunTime) +	
		    "\nsandbox:\t\t" + sandbox + 	
		    "\nmax # clients:\t\t" + clientMax + 	
		    "\nmax client connect time:" + Util.timeDiffString(maxConnectTime) +	
		    "\nclient purge freq:\t" + Util.timeDiffString(purgeFreq) +	
		    "\nmax client memory:\t" + clientMemory + "MB" +	
		    "\nshared JVM:\t\t" + sharedJVM +
		    "\nstack trace:\t\t" + stackTrace +

		    "\nqueried:\t\t" + date(queryTime) +  	
		    "\nstarted:\t\t" + date(startTime) +  	
		    "\n  time since:\t\t" + timediff(queryTime, startTime) +
		    "\nlast request:\t\t" + date(requestTime) +  	
		    "\n  time since:\t\t" + timediff(requestTime, startTime) +
		    "\njvmID:\t\t\t" + jvmID +  	
		    "\ncurrent server memory:\t" + cmem + 
			"MB used of maximum " + mmem + "MB" +	
		    "\ncumulative # clients:\t" + clientSeq + 
		    "\n# current clients:\t" + nclients + 
		    "\n");
	        return s.toString(); 	
	    }

	    // server URL
	    public String url() {
		return "rmi://" + host + ":" + port + "/" + name;
	    }
	}

	// RSClient info
	public static class Client implements Serializable {
	    public String id;	// client id
	    public String host; // client connecting from this host
	    public int userPort; // non-default RMI port, or 0
	    public String jvmID;  // in this JVM
	    public File workDir; // work directory
	    public boolean sandbox;  // client in sandbox?
	    public File logFile; // log file
	    public long maxBuildTime; // max model build time (msec)
	    public long maxRunTime; // max model run time (msec)
	    public long startTime; // client start time 	    
	    public long requestTime; // last request 	    
	    public long currMemory; // current memory usage (bytes)
	    public long maxMemory; // maximum memory (bytes)
	    public RCInfo.JobStatus job; // active job, or null
	    public Model[] models;  // current models

	    // string
	    public String toString(long queryTime) {

		// models
		String mstat = "";
		if (models != null) 
		    for (int i=0; i<models.length; i++)
			mstat = mstat + 
			    models[i].toString(queryTime);

		long cmem = currMemory >> 20;
		long mmem = maxMemory >> 20;
		return 
		"\n\tclient ID:\t" + id + 
		"\n\tfrom host:\t" + host + 
		"\n\tuserPort:\t" + 
		    ((userPort > 0) ? ("" + userPort) : "default") +   
		"\n\twork dir:\t" + workDir + 	
		"\n\tjob status:\t" + 
		     ((job == null) ? "idle" : job.desc) + 
		"\n\tstarted:\t" + date(startTime) + 
		"\n\t  time since:\t" + timediff(queryTime, startTime) +
		"\n\tlast request:\t" + date(requestTime) +
		"\n\t  time since:\t" + timediff(queryTime, requestTime) +
		"\n\tjvmID:\t\t" + jvmID + 
		"\n\tcurrent memory:\t" + cmem + 
			"MB used of maximum " + mmem + "MB" +	
		"\n" +
		mstat; 
	    }
	}
	    
	// RSModel info
	public static class Model implements Serializable {
	    public String id;   // model ID   
	    public String name; // user's name for model
	    public long requestTime; // last request
	    public long buildTime; // last build completed	    
	    public long runTime; // last run finished	    

	    // string
	    public String toString(long queryTime) {
		
		return 
		"\n\t\tmodel ID:\t" + id + 
		"\n\t\tname:\t\t" + name + 
		"\n\t\tlast compiled:\t" + date(buildTime) + 
		"\n\t\t  time since:\t" + timediff(queryTime, buildTime) +
		"\n\t\tlast run:\t" + date(runTime) +
		"\n\t\t  time since:\t" + timediff(queryTime, runTime) +
		"\n\t\tlast request:\t" + date(requestTime) + 
		"\n\t\t  time since:\t" + timediff(queryTime, requestTime) +
		"\n";
	    }
	}

	//// time/date formatting

	// pretty time difference string
	public static String timediff(long t1, long t0) {
	    if (t1 == 0) return "never";
	    if (t0 == 0) return "never";
	    return Util.timeDiffString(t1-t0);
	}

	// pretty date
	public static String date(long l) {
	    if (l==0) return "never";
	    Calendar cal = Calendar.getInstance();
	    cal.setTime(new Date(l));
	    return "" + 
		cal.get(Calendar.YEAR) + "-" + 
		fmt2((cal.get(Calendar.MONTH)+1)) + "-" + 
		fmt2(cal.get(Calendar.DAY_OF_MONTH)) + " " + 
		fmt2(cal.get(Calendar.HOUR_OF_DAY)) + ":" + 
		fmt2(cal.get(Calendar.MINUTE)) + ":" + 
		fmt2(cal.get(Calendar.SECOND));
	}

	// 2 digit number
	private static String fmt2(int i) {
	    if (i<10) return "0" + i;
	    return "" + i;
	}

}

