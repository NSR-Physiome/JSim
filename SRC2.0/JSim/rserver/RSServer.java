/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// remote JSim server

package JSim.rserver;

import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;
import java.net.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;
import JSim.rclient.*;

public class RSServer implements RCServerIF, RSServerIF  {
	private RSInfo.Server info; // exportable state info
	private boolean shutdown;  // shutting down?
	protected StringList clientIDs;  // current clients IDs
	private PrintWriter log;   // log output
	private Registry registry; // RMI registry for this server
	private File java;	// java executable
	private boolean clientReady; // client launch flag
	private String[] userPortIDs; // IDs per user port

	private static int TIMEOUT = 10000;
	private static int POLLFREQ = 500;

	// constructor
	public RSServer(RSInfo.Server infox) 
	throws RemoteException, Xcept, IOException {
	    info = infox;
	    if (info.userPort > 0) {
	    	UnicastRemoteObject.exportObject(this, info.userPort);
		userPortIDs = new String[info.clientMax];
	    } else 
	        UnicastRemoteObject.exportObject(this);
	    shutdown = false;
	    clientIDs = new StringList(4);
	    info.startTime = System.currentTimeMillis();
	    registry = LocateRegistry.getRegistry(info.port);

	    // find java
	    String javaHome = System.getProperty("java.home");
	    if (Util.isBlank(javaHome)) throw new Xcept(
		"java.home not defined");
	    java = new File(javaHome).getAbsoluteFile();
	    java = new File(java, "bin");
	    java = new File(java, "java");
	    if (! java.exists()) throw new Xcept(
		"Java binary " + java + " does not exist");

	    File f = new File(workDir(), "logfile");
	    FileWriter wrt = 
		new FileWriter(f.getAbsolutePath(), true);
	    log = new PrintWriter(wrt, true);
	    log("starting server " + info.url());
	    log("RSServer object registered on port=" +
	    	((info.userPort>0) ? ("" + info.userPort) : "default"));

	    // purge thread
	    RSPurge purge = new RSPurge(this);
	    purge.start();
	}

	// local query
	public File workDir() { return info.workDir; }
	public boolean shutdown() { return shutdown; }

	// establish connection to server,  return ID (RCServerIF)
	public RCInfo.Connect connect(RCInfo.Login login) 
	throws RemoteException {
	    String nhost = login.userName + "@" + login.hostName;
	    if (! Util.isBlank(login.login) && !login.login.equals(login.userName))
		nhost = login.login + " (" + nhost + ")";
	    log("login request: " + nhost +
		" " + login.osName + " JSim " + login.version);
	    try {
	    	if (clientIDs.size() >= info.clientMax) 
		    throw new Xcept("Maximum of " + info.clientMax + 
		    " simultaneous connections to server exceeded");
		String id = Util.safeName(login.login) + 
		    "." + newClientSeq();
		if (info.sharedJVM)
		    connectID(id, login.hostName);
	 	else
		    newJVM(id, login.hostName);

		// return info
		RCInfo.Connect cinfo = new RCInfo.Connect();
		cinfo.id = id;
		cinfo.optimAlgsInfo = client(id).optimAlgsInfo();
		cinfo.pingFreq = info.pingFreq;
	    	return cinfo;

	    } catch (Xcept e) {
		throw logXcept(e);
	    }
	}

	// new client sequence #
	synchronized private int newClientSeq() {
	    return info.clientSeq++;
	}

	// launch new JVM to call connectID() (via RSAdmin)
	public void newJVM(String id, String host) throws Xcept {
	    String cmd = java.getAbsolutePath();
	    StringList cmds = new StringList(16);
	    cmds.add(cmd);
	    cmds.add("-Xmx" + info.clientMemory + "m");
	    addProp("java.home", cmds);
	    addProp("java.class.path", cmds);
	    addProp("java.library.path", cmds);
	    addProp("jsim.home", cmds);
	    cmds.add("-Djsim.jvmID=" + id);
	    cmds.add("JSim.rserver.RSAdmin");
	    cmds.add("-name");
	    cmds.add(info.name);
	    cmds.add("-port");
	    cmds.add("" + info.port);
	    cmds.add("-connect");
	    cmds.add(id);
	    cmds.add(host);
//System.err.println("newJVM: " + cmds.toString(" ", false));

	    // launch JVM,  monitor successful start
	    clientReady = false;
	    try {
		Process proc = 
		    Runtime.getRuntime().exec(cmds.array());
		// unload sysout, err to prevent IO block
		new ProcessQuery(proc, 1000);
		int ct = TIMEOUT/POLLFREQ;
		int i=0;
	    	while (i<ct && !clientReady) {
		    Thread.sleep(POLLFREQ);
		    i++;
	    	}
		if (! clientReady) jvmLaunchError(proc);
		
		// wait for client(id) to return OK
		for (i=0; i<ct; i++) {
		    try {
		    	client(id);
		        return;
		    } catch (Exception e) {
		        Thread.sleep(POLLFREQ);
		    }
		}
		throw new Xcept("JVM launch client(" + id + 
		    ") did not respond within " + (TIMEOUT>>3) + " sec");
   
		
    	    } catch (Exception e) {
		String msg = "\t" + e;
		if (info.stackTrace) 
		    msg = msg + "\n" + Xcept.cleanMessage(e);
		log(logMessage("", e));
		throw Xcept.wrap(e);
	    }
	}

	// throw jvm launch error with sysadm info
	private void jvmLaunchError(Process proc) throws Exception {
	    try {
		int stat = proc.exitValue();
		String out = UtilIO.readText(
		    proc.getInputStream());
		String err = UtilIO.readText(
		    proc.getErrorStream());
		log("  client JVM exited unexpectedly: status=" + stat);
		if (!Util.isBlank(out)) log("    " + out); 
		if (!Util.isBlank(err)) log("    " + err); 
		throw new Xcept("Client JVM exited unexpectedly");
	    } catch (IllegalThreadStateException e) {
		proc.destroy();
		throw new Xcept(
		    "Client JVM creation timed out after "
		    + (TIMEOUT >> 3) + " seconds");
	    }
	}    

	// add java property to StringList
	public void addProp(String key, StringList cmds) 
	throws Xcept {
	    try {
	    	String val = System.getProperty(key);
	    	if (Util.isBlank(val)) throw new Xcept(
		    "RSServer: property <" + key + 
		    "> not defined");
		if (key.equals("java.class.path")) {
		    cmds.add("-classpath");
		    cmds.add(val);
		} else
	            cmds.add("-D" + key + "=" + val);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// new client info for id
	public RSInfo.Client newClientInfo(String id, String host)
	throws RemoteException {
	    RSInfo.Client cinfo = new RSInfo.Client();
	    cinfo.id = id;
	    cinfo.host = host;
	    cinfo.userPort = assignUserPort(id);
	    cinfo.workDir = new File(info.workDir, id);
	    cinfo.maxBuildTime = info.maxBuildTime; 
	    cinfo.maxRunTime = info.maxRunTime;
	    cinfo.sandbox = info.sandbox;
	    return cinfo;
	}

	// return assigned userPort, or 0 if inapplicable
	synchronized private int assignUserPort(String id) 
	throws RemoteException {
	    if (info.userPort <= 0) return 0;
	    for (int i=0; i<userPortIDs.length; i++) {
	        if (userPortIDs[i] != null) continue;
		userPortIDs[i] = id;
		return info.userPort+1+i; // RSServer uses userPort+0
	    }
	    throw new RemoteException("All userPort slots are filled");
	}
	
	// deassign a port
	synchronized private void releaseUserPort(String id) {
	    if (userPortIDs == null) return;
	    for (int i=0; i<userPortIDs.length; i++) {
	    	if (userPortIDs[i] == null) continue;
	        if (id.equals(userPortIDs[i])) userPortIDs[i] = null;
	    }
	}

	// connect a given client ID in server JVM
	public RSClientIF connectID(String id, String host)
	throws RemoteException {
	    RSInfo.Client cinfo = newClientInfo(id, host);
	    RSClient client = new RSClient(this, cinfo);
	    bindClient(id, client, cinfo.userPort);
	    return client;
	}

	// bind client to server
	public void bindClient(String id, RSClientIF client, int port)
	throws RemoteException {
	    clientIDs.add(id);
	    String name = info.name + "." + id;
	    registry.rebind(name, client);
	    log("  connected as " + id + " on port=" +
	    	((port>0) ? ("" + port) : "default"));
	    clientReady = true;
	}

	// disconnect clientID from server
	public void disconnectID(String id) throws RemoteException {
	    if (! clientIDs.containSame(id))
		throw new RemoteException("No such client: " + id);
	    clientIDs.remove(id);
	    String name = info.name + "." + id;
	    try {
	    	registry.unbind(name);
	    } catch (NotBoundException e) { 
		throw logXcept(e);
	    }
	    releaseUserPort(id);
	    log(id + ": disconnected");
	}

	// server status query (RSServerIF)
	public RSInfo.Server getInfo() throws RemoteException {
	    info.jvmID = RSAdmin.jvmID();
	    info.maxMemory = Runtime.getRuntime().maxMemory();
	    info.currMemory = Runtime.getRuntime().totalMemory();
	    info.queryTime = System.currentTimeMillis();
	    info.clientIDs = clientIDs.array();
	    return info;
	}

	// start "graceful" server shutdown
	public void startShutdown() throws RemoteException {
	    log("shutdown requested");
	    shutdown = true;
	    // RSAdmin.start() process will test this, then exit
	}

	// find client for name, id
	protected RSClientIF client(String clientID)
	throws Xcept {
	    String name = info.name + "." + clientID;
	    Object s = null;
	    try {
	    	s = registry.lookup(name);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	    if (s == null) throw new Xcept(
		"No such JSim client connection \"" + name + "\"");
	    if (! (s instanceof RSClientIF)) throw new Xcept(
		"RMI registry lists \"" + name +
		"\" as a " + s.getClass().getName() + 
		" which does not implement RSClientIF");
	    return (RSClientIF) s;
	}

	// log a message
	public void log(String msg) {
	    long d = System.currentTimeMillis();
	    String date = RSInfo.date(d);
	    log.println(date + ": " + msg);
	}
	public void log(String msg, StackTraceElement[] stack) {
	    log(msg);
	    if (stack == null || ! info.stackTrace) return;
	    for (int i=0; i<stack.length; i++) 
	    	log.println("\t" + stack[i]);
	}

	// log exception
	private final RemoteException logXcept(Exception e) {
	    return logXcept("", e);
	}
	private final RemoteException logXcept(String msg, Exception e) {
	    log("  failed: " + logMessage(msg, e));
	    return new RemoteException(msg + Xcept.cleanMessage(e));
	}
	private final String logMessage(String msg, Exception e) {
	    if (e != null) {
	    	msg = msg + " " + Xcept.cleanMessage(e);
	    	if (info.stackTrace) msg = msg + "\n" + 
	    	    Xcept.stackTraceString(e);
	    }
	    return msg;
	}

	// tag client request
	public void request(long l) {
	    info.requestTime = l;
	}

}

