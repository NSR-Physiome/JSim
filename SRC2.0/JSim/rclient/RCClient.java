/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// client of remote JSim RMI server

package JSim.rclient;

import java.util.*;
import java.net.*;
import java.rmi.*;
import java.rmi.registry.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 

public class RCClient extends ASServer implements DiagInfo {
	private String host;	// server host
	private int serverPort;	// on this port
	private String serverName; // jsim server name
	private RCServerIF server; // connected server
	private RCInfo.Connect connect; // connection info
	private RCClientIF rclient; // remote client connection
	private Hashtable<String,String> commonTexts;
	private NamedVal.NList solverDefaults; // solver defaults
	private OptimAlg.NList optimAlgs; // optimizer list
	private long lastRequest; // time of last remote request
	private Pinger pinger; // ping thread
	private ASServer.Messenger messenger; // or null
	private int nmodelIDs;

	// constructor
	public RCClient(NamedVal.NList options,
	ASServer.Messenger msgr, ASInfo.Sandbox sandbox)
	throws Xcept {

	    // process serverName, login
	    String serverName = options.stringVal("server", null);
	    String login = null;
	    if (serverName == null) throw new Xcept(
	    	"Server name missing");
	    int ainx = serverName.indexOf('@');
	    if (ainx>0) {
		login = serverName.substring(0,ainx);
		serverName = serverName.substring(ainx+1);
	    }
	    if (serverName.indexOf("://")<0) 
		serverName = "rmi://" + serverName;
	    commonTexts = new Hashtable<String,String>();

	    try {
	    	// create URI
	    	URI uri = new URI(serverName);
	    	if (! uri.getScheme().equals("rmi")) throw new Xcept(
		    "Unsupported protocol in JSim server: " +
		    uri.getScheme());
	    	if (sandbox != null) throw new Xcept(
	    	    "Sandbox option invalid for remote server");
	    	String password = options.stringVal("password", null);

	    	// decode uri
		if (! uri.getScheme().equals("rmi")) throw new Xcept(
		    "Illegal RCClient URI: " + uri + 
		    " (rmi: required)");
		host = uri.getHost();
		serverPort = uri.getPort();
		if (serverPort<=0) serverPort = RCInfo.DEFAULT_PORT;
		serverName = uri.getPath();
		if (Util.isBlank(serverName) || serverName.equals("/")) 
		    serverName = "/JSimServer";
		serverName = serverName.substring(1);

		// connect to registry,  find server
		Util.verbose("RCClient: getRegistry");
	    	Registry registry = 
		    LocateRegistry.getRegistry(host, serverPort);
		Util.verbose("RCClient: lookup serverName");
		Object s = registry.lookup(serverName);
		if (! (s instanceof RCServerIF)) throw new Xcept(this,
		    "Invalid JSim server class=" + 
		    s.getClass().getName());
		server = (RCServerIF) s;

		// format login request
		RCInfo.Login linfo = new RCInfo.Login();
		linfo.login = login;
		linfo.password = password;
		linfo.version = Util.version();
		linfo.hostAddr = InetAddress.getLocalHost().getAddress();
		linfo.hostName = InetAddress.getLocalHost().getHostName();
	 	try {
		    linfo.userName = System.getProperty("user.name");
		} catch (SecurityException e) {
		    linfo.userName = "anonymous";
		}
		linfo.osArch = System.getProperty("os.arch");
		linfo.osName = System.getProperty("os.name");
		linfo.osVersion = System.getProperty("os.version");
		if (Util.isBlank(login)) linfo.login = linfo.userName;

		// make client connection
		connect = server.connect(linfo);
		if (connect.port > 0 && connect.port != serverPort)
		    registry = 
			LocateRegistry.getRegistry(host, connect.port);
		String cname = serverName + "." + connect.id;
		Util.verbose("RCClient: lookup client");
		Object c = registry.lookup(cname);
		if (! (c instanceof RCClientIF)) throw new Xcept(this,
		    "Invalid JSim connection class=" + 
		    c.getClass().getName());
		rclient = (RCClientIF) c;
		

		// optimizers
		optimAlgs = new OptimAlg.NList(connect.optimAlgsInfo);

		// test connection, start pinger
		rclient.ping();
		request();
		if (connect.pingFreq > 0) {
		    pinger = new Pinger(this);
		    pinger.start();
		}

		// store messenger
		messenger = msgr;
		nmodelIDs = 0;

	    } catch (java.rmi.ConnectException e) {
		throw new Xcept(noConnectMsg(e));
	    } catch (java.rmi.UnknownHostException e) {
		throw new Xcept(unknownHostMsg());
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// complete connection message
	private String noConnectMsg(Exception e) {
	    StringBuffer buf = new StringBuffer();
	    buf.append(e.toString() + "\n");
	    buf.append("JSim server connection to host " + 
		host + " (port " + serverPort +
		") failed.\n");
	    buf.append("Possible reasons:\n");
	    buf.append("  1) the selected host (or port) is incorrect;\n");
	    buf.append("  2) the server is down at this time;\n");
	    buf.append("  3) a firewall on your computer or between your\n");
	    buf.append("    computer and " + host + " is blocking some or\n");
	    buf.append("    all traffic on port " + serverPort + ".");
	    return buf.toString();
	}

	// can't find host message
	private String unknownHostMsg() {
	    return "Requested JSim remote server host \"" + host + 
		"\"\n  was not found in internet name tables.";
	}

	// get/set dynamic server properties
	public void setProperties(NamedVal[] nvals) throws Xcept {
	    if (nvals == null) return;
	    for (int i=0; i<nvals.length; i++) 
	    	setProperty(nvals[i]);
	}
	public void setProperty(NamedVal nval) throws Xcept {
	    if (nval.name().equals("maxProc")
	    	&& nval.intVal() == 1) return;
	    throw new Xcept(nval, "Remote server property not supported");
	}
	public NamedVal getProperty(String n) throws Xcept {
	    return null;
	}

	// server-side plugins not implemented
	public void addPlugin(Plugin plugin) {
	    // no action, no complaint
	}

	// translate model text
	public String translateModelText(int srcType, 
	int destType, String srcText, String options) 
	throws Xcept {
	    try {
	    	return rclient.translateModelText(
		    srcType, destType, srcText, options);
	    } catch (RemoteException e) {
	    	throw Xcept.wrap(e);
	    }
	}	

	// get tranlator warnings
	public String[] getTranslatorWarnings() throws Xcept {
	    try {
	    	return rclient.getTranslatorWarnings();
	    } catch (RemoteException e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// create model
	public ASModel newModelRT() throws Xcept {
	    return new RCModel(this);
	}

	// next modelID
	protected String nextModelID() {
	    return "model." + nmodelIDs++;
	}

	// wait for job to complete
	protected void waitForJob(RCModel rt, RCInfo.JobStatus jstat) 
	throws Xcept {
	    while (! jstat.done) {
		RCInfo.JobStatus nstat = null;
	        try { 
		    Thread.sleep(500);
		    request();
		    nstat = rclient.getJobStatus();
		} catch (Exception e) {
		    nstat = jstat;
		    jstat.done = true;
		    jstat.termMessage = Xcept.wrap(e).getMessage();
		}
		if (nstat == null) throw new Xcept(
			"RCClientIF.getJobStatus() returned null");
		jstat = nstat;
		if (rt == null) continue;
		rt.updateJobStatus(jstat.jstat);
		if (jstat.jobInfo instanceof RCInfo.OptimOutput)
		    rt.updateOptimOutput(jstat.jobInfo);
		if (jstat.jobInfo instanceof RCInfo.MoptOutput)
		    rt.updateMoptOutput(jstat.jobInfo);
		if (jstat.jobInfo instanceof RCInfo.CacheDelta[])
		    rt.updateCacheDeltas(
			(RCInfo.CacheDelta[]) jstat.jobInfo);
	    }
	    if (jstat.termStat != RCInfo.NORMAL)
		throw new Xcept(jstat.termMessage);
	}

	// common texts
	public String getCommonText(String name) throws Xcept {
	    String text = commonTexts.get(name);
	    if (text != null) return text;
	    
	    try {
	    	request();
		text = rclient.getCommonText(name);
	        commonTexts.put(name, text);
		return text;
	    } catch (RemoteException e) {
	    	throw Xcept.wrap(e);
 	    }
	}

	// get solver/func gen default settings
	public NamedVal.NList getSolverDefaults() throws Xcept {
	    if (solverDefaults != null) return solverDefaults;
	    try {
		request();
		solverDefaults = new NamedVal.NList(rclient.getSolverDefaults());
	    } catch (RemoteException e) {
		throw new Xcept("Solver defaults not available from remote server"); 
	    }
	    return solverDefaults;
	}

	// disconnect from server
	public void disconnect() {
	    try {
		request();
	    	rclient.disconnect();
		pinger.stop(); // controlled exit too slow
	    } catch (Exception e) {
		System.err.println("" + e);
	    }
	}

	// simple query
	public String diagInfo() {
	    return "Server " + serverURI();
	}
	public String serverURI() {
	    return "rmi://" + host + ":" + serverPort + "/" + serverName;
	}
	public RCClientIF rclient() { return rclient; }
	public String id() { return connect.id; }
	public OptimAlg.NList optimAlgs() { return optimAlgs; }
	public OptimAlg.Info[] optimAlgsInfo() { 
	    return connect.optimAlgsInfo; 
	}

	// remote requests
	protected void request() {
	    lastRequest = System.currentTimeMillis();
	}

	// send ping() requests to keep remote connection alive
	public static class Pinger extends Thread {
	    private RCClient client;

	    // constructor
	    public Pinger(RCClient c) {
		super("Pinger thread");
		client = c;
	    }

	    // run thread
	    public void run() {
		while (true) {
		    try {
		    	Thread.sleep(client.connect.pingFreq);
		    	long currTime = System.currentTimeMillis();
		    	if (currTime - client.lastRequest < client.connect.pingFreq)
			    continue;
		    	client.request();
		    	client.rclient.ping();
		    } catch (Exception e) {
		    	System.err.println("RCClient.Pinger: " + e);
		    }
		}
	    }
	}
	    
	// memory usage message
	public String memoryMessage() throws Xcept {
	    String rmsg;
	    try {
	    	rmsg = rclient.getMemoryMessage();
	    } catch (RemoteException e) {
	    	rmsg = e.toString();
	    }
	    return "Client memory: " + Util.memoryMessage() 
	       + "\nServer memory: " + rmsg;  
	}
}

