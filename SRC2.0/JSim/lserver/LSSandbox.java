/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// minimal sandbox for running untrusted java-code models

package JSim.lserver;

import java.io.*;
import java.net.*;
import java.util.*;
import java.security.*;
import JSim.util.*;
import JSim.aserver.*;

public class LSSandbox extends SecurityManager {
	private LSServer server; // for this server
	private StringList readDirs; // readable dirs
	private StringList writeDirs; // writeable dirs
	private StringList hosts; // connectable hosts
	private StringList runPerms; // disallowed RuntimePermissionS

	// constructor
	public LSSandbox(LSServer s, ASInfo.Sandbox info) throws Xcept {
	    super();
	    server = s;
	    Util.verbose("Creating local sandbox...");

	    // setup readDirs,  GUI requires entire F/S
	    readDirs = new StringList(8);
	    if (info.readGUI) {
		File[] roots = File.listRoots();
		for (int i=0; i<roots.length; i++) 
		    addDir(readDirs, roots[i]);
	    }
	    addDir(readDirs, Util.jsimHome());
	    addDir(readDirs, System.getProperty("java.home"));
	    addPath(readDirs, System.getProperty("java.class.path"));
	    addPath(readDirs, System.getProperty("java.ext.dirs"));
	    addPath(readDirs, System.getProperty("java.library.path"));
	    addPath(readDirs, System.getProperty("sun.boot.class.path"));
	    addPath(readDirs, server.jsimPath());
	    addDir(readDirs, server.buildDir);
	    addDir(readDirs, info.readPath);
	    
	    // setup writeDirs
	    writeDirs = new StringList(8);
	    addDir(writeDirs, server.buildDir);
	    addDir(writeDirs, info.writePath);

	    // runtime permissions
	    runPerms = new StringList(8);
	    runPerms.add("setSecurityManager");
	    // AWT & RMI clients need 
	    //    "createClassLoader" & "setContextClassLoader"

	    // hosts
	    hosts = new StringList(8);
	    addHost("localhost");
	    try {
	    	addHost(InetAddress.getLocalHost().getHostName());
	    } catch (UnknownHostException e) {
		// nothing to do but hope for the best
	    }
	    for (int i=0; i<info.hosts.size(); i++) {
		addHost(info.hosts.str(i));
	    }
	}

	// add a host to the accessible hosts list
	private void addHost(String h) {
	    hosts.add(h);
	    try {
		byte[] addr = InetAddress.getByName(h).getAddress();
		String h1 = "";
		for (int j=0; j<addr.length; j++) {
		    if (j>0) h1 = h1 + ".";
		    int k = addr[j];
		    if (k<0) k += 256;
		    h1 = h1 + k;
		}
		hosts.add(h1);
		Util.verbose("Adding host " + h + " (" + h1 + 
		    ") to sandbox");
	    } catch (UnknownHostException e) {
		// nothing for now
	    }
	}

	// add all dir/files in path to list
	private void addPath(StringList list, String path) throws Xcept {
	    StringList p = new StringList(path, File.pathSeparator);
	    for (int i=0; i<p.size(); i++) 
		addDir(list, p.str(i));
	}

	// add dir/file to list
	private void addDir(StringList list, StringList dirs) 
	throws Xcept {
	    for (int i=0; i<dirs.size(); i++) 
	        addDir(list, dirs.str(i));
	}
	private void addDir(StringList list, String s) throws Xcept {
	    addDir(list, new File(s));
	}
	private void addDir(StringList list, File f) throws Xcept {
	    if (f == null) return;
	    try {
	    	String n = f.getCanonicalPath();
	    	if (! hasFile(list, n)) list.add(n);
		if (! f.isAbsolute()) return;
		n = f.getPath();
	    	if (! hasFile(list, n)) list.add(n);		
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// is dir/file implied by list?
	private boolean hasFile(StringList list, String n) {
	    for (int i=0; i<list.size(); i++) 
		if (n.startsWith(list.str(i)))
		    return true;
	    File f = new File(n);
	    if (f.isAbsolute()) return false;
	    try {
	    	return hasFile(list, f.getCanonicalPath());
	    } catch (Exception e) {
		return false;
	    }
	}

	// generate SecurityException 
	private void error(String s) {
	    String msg = "Cannot " + s + " in the JSim sandbox";
	    throw new SecurityException(msg);
	}

	//// permission checks

	// check connection to host
	public void checkConnect(String host, int port) {
//	    System.err.println("checkConnect " + host + ":" + port);
	    if (hosts.containSame(host)) return;
	    error("connect to host " + host + " allowed hosts=" + hosts);
	}

	// file deletes check writeDirs
	public void checkDelete(String s) {
	    if (! hasFile(writeDirs, s))
	        error("delete file " + s);
	}

	// check exec
	public void checkExec(String cmd) {
	    error("run system command \"" + cmd + "\"");
	}

	// piped reads OK
	public void checkRead(FileDescriptor f) { }

	// file reads check readDirs
	public void checkRead(String s) { 
	    if (! hasFile(readDirs, s)) 
		error("read file " + s);
	}	
	public void checkRead(String s, Object o) { checkRead(s); }

	// package access OK
	public void checkPackageAccess(String s) { }

	// needs work!!!
	public void checkPermission(Permission p) {

	    // read any property, no write to java or jsim properties
	    if (p instanceof PropertyPermission) {
		if (p.getActions().equals("read")) return;
		String n = p.getName();
		boolean err = false;
		if (n.startsWith("java")) err = true;
		if (n.startsWith("jsim")) err = true;
		if (err) error("set property " + n);
		return;
	    }

	    // redundant with checkRead, checkWrite, ...
	    if (p instanceof FilePermission) return;		

	    // runtime permissions
	    if (p instanceof RuntimePermission) {
		if (runPerms.containSame(p.getName()))
		    error(p.getName());
		else
		    return;
	    }

	    // unimportant
	    if (p instanceof NetPermission) return;
	    if (p instanceof SecurityPermission) return;
	    if (p instanceof java.awt.AWTPermission) return;
	    if (p instanceof java.lang.reflect.ReflectPermission) return;
	    if (p instanceof java.util.logging.LoggingPermission) return;

//	    System.err.println("untested checkPermission: " + p);
	}

	// piped writes OK
	public void checkWrite(FileDescriptor f) { }

	// file writes check writeDirs
	public void checkWrite(String s) { 
	    if (! hasFile(writeDirs, s))
		error("write file " + s);
	}
}

