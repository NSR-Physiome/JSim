/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// interface to remote server (RSServer)

package JSim.rclient;

import java.io.*;
import java.rmi.*;

import JSim.util.*;
import JSim.data.*;

public interface RCServerIF extends Remote {

	// establish connection to server
	RCInfo.Connect connect(RCInfo.Login login) 
	throws RemoteException;
}

