/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// server administrator program for JSim RMI Server

package JSim.rserver;

import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;
import java.net.*;
import java.io.*;

import JSim.util.*;
import JSim.rclient.*;

public class RSAdmin  {

	// command constants
	public final static int NONE=0;
	public final static int START=1;
	public final static int STOP=2;
	public final static int STATUS=3;
	public final static int CONNECT=4;
	public final static int DISCONNECT=5;

	// administrator options
	private int port = RCInfo.DEFAULT_PORT;  // registry port
	private int userPort = 0;  // use RMI default object ports
	private int command = NONE;
	private String serverName = "JSimServer";
	private boolean stackTrace = false;
	private String clientID = null;
	private String clientHost = null;
	private File workDir = null;
	private boolean sharedJVM = false;
	private boolean sandbox = true;
	private int clientMax = 10; // max # concurrent clients
	private long maxConnectTime = 3600*12; // max client connect (sec)
	private long purgeFreq = 90; // for pingless clients (sec)
	private long clientMemory = 600; // max client memory (MB)
	private long maxBuildTime = 120; // max model build time (sec)
	private long maxRunTime = 3600;	// max model run time (sec)

	// state
	private Registry registry;

	// mainline
	public static void main(String[] args) {
	    new RSAdmin(args);
	}

	// constructor (mainline)
	public RSAdmin(String[] args) {
	    try {
		processArgs(args);		    		
		registry = LocateRegistry.getRegistry(port);
		switch (command) {
		case START: start(); break;
		case STOP: stop(); break;
		case STATUS: status(); break;
		case CONNECT: connect(clientID, clientHost); break;
		case DISCONNECT: disconnect(clientID); break;
		default: usage();
		}
		System.exit(0);
	    } catch (Exception e) {
		String msg =  (e instanceof Xcept) ?
		     e.getMessage() : e.toString();
	    	System.err.println(msg);
	    	if (stackTrace) e.printStackTrace();
	    	System.exit(1);
	    }
	}	    

	// process command line
	private void processArgs(String[] args) 
	throws Xcept {
	    for (int i=0; i<args.length; i++) {
		String s = args[i];
		if (s.equals("-start") && i+1<args.length) {
		    command = START;
		    workDir = new File(args[++i]);
		    workDir = workDir.getAbsoluteFile();
		} else if (s.equals("-stop")) {
		    command = STOP;
		} else if (s.equals("-status")) {
		    command = STATUS;
		} else if (s.equals("-connect") && i+2<args.length) {
		    command = CONNECT;
		    clientID = args[++i];
		    clientHost = args[++i];
		} else if (s.equals("-disconnect") && i+1<args.length) {
		    command = DISCONNECT;
		    clientID = args[++i];
		} else if (s.equals("-stack")) {
		    stackTrace = true;
		} else if (s.equals("-sandbox")) {
		    sandbox = true;
		} else if (s.equals("-nosandbox")) {
		    sandbox = false;
		} else if (s.equals("-sharedJVM")) {
		    sharedJVM = true;
		} else if (s.equals("-clientMax") && i+1<args.length) {
		    clientMax = Util.toInt(args[++i]);
		} else if (s.equals("-purgeFreq") && i+1<args.length) {
		    purgeFreq = Util.toInt(args[++i]);
		} else if (s.equals("-clientMemory") && i+1<args.length) {
		    clientMemory = Util.toInt(args[++i]);
		} else if (s.equals("-maxBuildTime") && i+1<args.length) {
		    maxBuildTime = Util.toInt(args[++i]);
		} else if (s.equals("-maxRunTime") && i+1<args.length) {
		    maxRunTime = Util.toInt(args[++i]);
		} else if (s.equals("-name") && i+1<args.length) {
		    serverName = args[++i];
		} else if (s.equals("-port") && i+1<args.length) {
		    port = Util.toInt(args[++i]);
		} else if (s.equals("-userPort") && i+1<args.length) {
		    userPort = Util.toInt(args[++i]);
		} else 
		    usage();
	    }
	    if (port <= 0) throw new Xcept(
		"jsserver port# must be positive");
	}

	// start server 
	private void start() throws Exception {

	    // integrity check
	    if (workDir == null) {
		System.err.println(
		   "work directory required when starting server");
		usage();
	    }
	    if (! workDir.isDirectory()) throw new Xcept(
		"No such work directory " + 
		workDir.getAbsolutePath());
	    if (purgeFreq < 60) throw new Xcept(
		"jsserver purgeFreq must be not less than 60 seconds "); 

	    // create server 
	    RSInfo.Server info = new RSInfo.Server();
	    info.host = InetAddress.getLocalHost().getHostName();
	    info.port = port;
	    info.userPort = userPort;
	    info.name = serverName;
	    info.version = Util.version();
	    info.sandbox = sandbox;
	    info.workDir = workDir;
	    info.clientMax = clientMax;
	    info.pingFreq = (purgeFreq*1000)/5; 
	    info.purgeFreq = purgeFreq*1000; 
	    info.maxConnectTime = maxConnectTime*1000;  
	    info.clientMemory = clientMemory; 
	    info.sharedJVM = sharedJVM;
	    info.maxBuildTime = maxBuildTime*1000; 
	    info.maxRunTime = maxRunTime*1000; 
	    info.stackTrace = stackTrace;
	    RSServer rserver = new RSServer(info);

	    // start registry, publish server
	    registry = LocateRegistry.createRegistry(port);
	    registry.rebind(serverName, rserver);
	    System.err.println("Starting " + 
		info.url() + " ...");
	    while (! rserver.shutdown()) {
		try {
		    Thread.sleep(1000);
		} catch (Exception e) {	}
	    }
	}

	// shutdown server
	private void stop() throws Exception {
	    RSServerIF server = server(serverName);
	    RSInfo.Server sinfo = server.getInfo();
	    if (sinfo.clientIDs != null)
	        for (int i=0; i<sinfo.clientIDs.length; i++) 
		    disconnect(sinfo.clientIDs[i]);
	    server.startShutdown();
	}

	// query server status
	private void status() throws Exception {
	    RSServerIF server = server(serverName);
	    RSInfo.Server info = server.getInfo();
	    System.out.print(info.toString());
	    if (info.clientIDs == null) return;
	    for (int i=0; i<info.clientIDs.length; i++) {
		String id = info.clientIDs[i];
		RSClientIF client = client(id);
		RSInfo.Client cinfo = client.getInfo();
		System.out.print(cinfo.toString(info.queryTime));
	    }
	}

	// new client connection
	private void connect(String clientID, String clientHost)
	throws Exception {
	    RSServerIF server = server(serverName);
	    RSInfo.Client cinfo = 
		server.newClientInfo(clientID, clientHost);
	    RSClient client = new RSClient(server, cinfo);
	    server.bindClient(clientID, client, cinfo.userPort);

	    // wait for client disconnect
	    while (! client.disconnected()) {
		try {
		    Thread.sleep(1000);
		} catch (Exception e) { }
	    }
	}

	// disconnect client
	private void disconnect(String clientID) throws Exception {
	    RSClientIF client = client(clientID);
	    client.disconnect();
	}	

	// find server for name
	private RSServerIF server(String name) throws Exception {
	    Object s = registry.lookup(name);
	    if (s == null) throw new Xcept(
		"No such JSim server \"" + name + "\"");
	    if (! (s instanceof RSServerIF)) throw new Xcept(
		"RMI registry lists \"" + name +
		"\" as a " + s.getClass().getName() + 
		" which does not implement RSServerIF");
	    return (RSServerIF) s;
	}

	// find client for name, id
	private RSClientIF client(String clientID)
	throws Exception {
	    String name = serverName + "." + clientID;
	    Object s = registry.lookup(name);
	    if (s == null) throw new Xcept(
		"No such JSim client connection \"" + name + "\"");
	    if (! (s instanceof RSClientIF)) throw new Xcept(
		"RMI registry lists \"" + name +
		"\" as a " + s.getClass().getName() + 
		" which does not implement RSClientIF");
	    return (RSClientIF) s;
	}

	// usage string
	private void usage() throws Xcept {
	    throw new Xcept(
		"Usage: jsserver command [options]\n" +
		"Commands:\n" +
		"\t-start dir\tstart server in given work directory\n" +
		"\t-stop\t\tstop server\n" +
		"\t-status\t\tquery server status\n" + 
		"\t-connect ID\tcreate client connection (debug only)\n" +
		"\t-disconnect ID\tdisconnect client\n" +
		"Startup Options:\n" +
		"\t-sandbox\t\trun clients in safe sandbox  (default)\n" +
		"\t-nosandbox\t\trun clients without safe sandbox\n" +
		"\t-purgeFreq number\tspecify uncommunicative client purge freq in secs (default 90)\n" +
		"\t-maxConnectTime number\tspecify max client connection time is secs (default 12*3600)\n" +
		"\t-clientMax number\tspecify max # concurrent clients (default 10)\n" +
		"\t-clientMemory number\tspecify max client memory in MB (default 600)\n" +
		"\t-maxBuildTime number\tspecify model build timeout in secs (default 120)\n" +
		"\t-maxRunTime number\tspecify mode run timeout in secs (default 3600))\n" +
		"\t-stack\t\t\tlog stack trace on exceptions\n" +
		"\t-sharedJVM\t\tclients share server's JVM (debug only)\n" +
		"\t-userPort number\tspecify user ports (default 0=RMI default ports)\n" +
		"General Options:\n" +
		"\t-name string\tspecify non-default server name\n" +
		"\t-port number\tspecify registry port (default " + RCInfo.DEFAULT_PORT + ")\n"
	    );
	}

	// return JVM ID, if any
	public static String jvmID() {
	    try {
		return System.getProperty("jsim.jvmID");
	    } catch (Exception e) {
		return null;
	    }
	}

}
