/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// launch GUI application or applet 

package JSim.gui;

import javax.swing.*;
import java.util.*;
import JSim.util.*;
import JSim.aserver.*; import JSim.project.*;

public class GLaunch extends JApplet {	    

	// main line for freestanding application
	public static void main(String[] args) throws Xcept {
	    new GMain(null, args);
	}

	// show startup splash window
	public static void splash() {
	    System.err.println("JSim starting...");
	}

	//// instance vars/methods 
	private GMain gmain;  // if in applet

	// applet constructor - repress annoying warning message
	public GLaunch() {
	    getRootPane().putClientProperty(
		"defeatSystemEventQueueCheck", Boolean.TRUE);
	}

	// initialize applet
	public void init() {

	    // make args array
	    String[] arr = null;
	    String s = getParameter("args");
	    StringTokenizer stok = new StringTokenizer(
		(s!=null) ? s : "");
	    int ct = stok.countTokens() + 4;
	    arr = new String[ct];
	    int i=0;
	    arr[i++] = "-server";
	    arr[i++] = getDocumentBase().getHost();
	    arr[i++] = "-userurl";
	    arr[i++] = getDocumentBase().toString();
	    while (stok.hasMoreTokens()) 
		 arr[i++] = stok.nextToken();

	    // launch window
	    gmain = new GMain(this, arr);
	}

	// start applet
	public void start() {
	    SwingUtilities.invokeLater(new Runnable() {
 		public void run() { // run in event thread
		}
	    });
	}

	// stop applet
	public void stop() {
	    super.stop();
	    ASServer server = gmain.server();
	    if (server != null) server.disconnect();
	}
	    
}


