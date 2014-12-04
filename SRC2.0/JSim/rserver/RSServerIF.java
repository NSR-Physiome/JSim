/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// admin calls to RSServer

package JSim.rserver;

import java.io.*;
import java.rmi.*;

import JSim.util.*;
import JSim.data.*;
import JSim.rclient.*;

public interface RSServerIF extends Remote {

	// log results
	void log(String msg, StackTraceElement[] stack) 
	throws RemoteException;

	// query status info
	RSInfo.Server getInfo() throws RemoteException;

	// shutdown server
	void startShutdown() throws RemoteException;

	// new client info
	RSInfo.Client newClientInfo(String clientID, String host)
	throws RemoteException;

	// bind client to server
	void bindClient(String id, RSClientIF client, int port)
	throws RemoteException;

	// disconnect clientID from server
	void disconnectID(String clientID) throws RemoteException;
}

