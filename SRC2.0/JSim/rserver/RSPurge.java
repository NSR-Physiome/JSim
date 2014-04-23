/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// purge system of long-running models and client connections

package JSim.rserver;

import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;
import java.net.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;
import JSim.rclient.*;

public class RSPurge extends Thread {
	private RSServer server;
	private RSInfo.Server info;

	// constructor
	public RSPurge(RSServer s) throws RemoteException {
	    super("RSPurge thread");
	    server = s;
	    info = server.getInfo();
	}

	// run job
	public void run() {
	    while (true) {
	    	try {
		    Thread.sleep(info.purgeFreq);
		    for (int i=0; i<server.clientIDs.size(); i++) 
		        check(server.clientIDs.str(i));
		} catch (Exception e) { 
		    server.log("purge: " + e, e.getStackTrace());
		}
	    }
	}

	// check purge of client
	public void check(String id) 
	throws RemoteException, Xcept {
	    RSClientIF client = server.client(id);
	    RSInfo.Client cinfo = client.getInfo();
	    long currTime = System.currentTimeMillis();
	    String msg = null;
	    if (currTime - cinfo.startTime > info.maxConnectTime)
		msg = "maximum connect time exceeded";
	    if (currTime - cinfo.requestTime > info.purgeFreq)
		msg = "client appears dead (no communication)";
	    if (msg == null) return;

	    // do actual purge of client
	    server.log(id + ": " + msg);
	    client.disconnect();
	}
}
