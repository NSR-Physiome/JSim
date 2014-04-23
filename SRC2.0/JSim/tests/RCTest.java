/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// remote server test programs

package JSim.tests; import JSim.rclient.*;

import java.net.URI;
import java.rmi.*;
import java.io.*;
import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 

// create RCClient
public class RCTest {

	public static void main(String[] args) 
	throws Exception {
	    NamedVal.NList nvals = new NamedVal.NList();
	    nvals.setVal("server", "guest@rmi://localhost");
	    RCClient client = 
	    	(RCClient) ASServer.create(nvals, null, null);
	    System.out.println("client connection " +
		client.id() + " is alive.");
	    Thread.sleep(10000);
	    client.disconnect();
	    System.out.println("client disconnected");
	}
}


