/*NSRCOPYRIGHT
	Copyright (C) 1999-2020 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// SBW functionality

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.project.*;
import JSim.aserver.*;
import JSim.gui.model.*;

import edu.caltech.sbw.*;

public class GSBW {
	protected GMain gmain;  // mainline
	protected ModuleImpl moduleImp;  // SBW module
	protected GModel target; // target for model text
	protected boolean isReady; // project window has init'ed
	protected boolean isRunning; // running as SBW module?

	// limit public methods to doAnalysis(), since SBW
	//   broker exposes all public methods
	
	// constructor
	public GSBW(GMain gmain) throws Exception {
	    this.gmain = gmain;
	    String com = gmain.gappl().sbwCommand;
	    if (Util.isBlank(com)) return;
	    if (! isAvailable()) return;
	    System.setProperty("java.net.preferIPv4Stack", "true");
	    System.setProperty(
	        "sbw.broker.allow-remote-modules", "true");
		
	    // initialization command
	    if (com.equals("register")) { 
		doRegister();
	    } else if (com.equals("module")) { 
		doModule();
		return;
	    } else if (com.equals("command")) {
		System.out.println(relaunchCommand());
	    } else {
		System.err.println("Unknown command: -sbw" + com);
	    }

	    // if still here, exit
	    Util.verbose("SBW command completed, exiting now...");
	    server().disconnect();
	    System.exit(0);
	}

	// simple query
	protected boolean isAvailable() { 
	    return ! gmain.isApplet(); 
	}
	protected GModel target() { return target; } 
	protected boolean isRunning() { return isRunning; }
	protected ASServer server() { return gmain.server(); }

	// set stuff
	protected void setReady() { isReady = true; }
	protected void setTarget(GModel gmodel) {
	    target = gmodel;
	}
	
	// register app with SBW
	protected void doRegister() throws Xcept {
	    Util.verbose("Performing -sbwregister ...");
	    try {
	    	initModule();
	    	moduleImp.registerModule();
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// run module with SBW
	protected void doModule() throws Xcept {
	    Util.verbose("Performing -sbwmodule ...");
	    try {
	        initModule();
	        moduleImp.enableModuleServices();
	        isRunning = true;
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// create module
	private void initModule() throws Exception {
	    if (moduleImp != null) return;
	    moduleImp = new ModuleImpl("JSim", "JSim Simulator", 
	        ModuleImpl.UNIQUE);
	    moduleImp.addService("JSim", "JSim Simulator",
	    	"Analysis", this);
	    moduleImp.setCommandLine(relaunchCommand());	    
	}

	// format java relaunch command
	protected String relaunchCommand() throws Exception {
	    String Q = "\""; // double quotes s/b OK on all OS
	    File fjava = new File(System.getProperty("java.home"));
	    fjava = new File(fjava, "bin");
	    // javaw on Win32 represses console
	    String njava = Util.isWin32() ? "javaw" : "java";
	    fjava = new File(fjava, njava);
	    String com = Q + fjava + Q;
	    String jshome = System.getProperty("jsim.home");
	    if (Util.isMacos()) { // MacOS specifics
		com = com + " -Xdock:name=JSim"; // dock name
		// icon
		com = com + " -Xdock:icon=" + Q + jshome + 
		    "/JSim.app/Contents/Resources/JSim.icns" + Q; 
		if (d32Available(fjava))
	            com = com + " -d32"; // 32-bit mode switch
	    } 
	    com = com + " -classpath " + Q +
	    	System.getProperty("java.class.path") + Q;
	    com = com + " -Djava.library.path=" + Q + 
	    	System.getProperty("java.library.path") + Q;
	    long maxmem = Runtime.getRuntime().maxMemory();
	    com = com + " -Xmx" + (maxmem/(1024*1024) + 3) + "m";
	    com = com + " -Djsim.home=" + Q + jshome + Q;
	    com = com + " -Djsim.userdir=" + Q + 
	        System.getProperty("jsim.userdir") + Q;
	    com = com + " JSim.gui.GLaunch";
	    com = com + " -sbwmodule";
	    return com;
	}

	// does java command have -d32 switch?
	private boolean d32Available(File fjava) {
	    try {
	    	String[] cmdarr = new String[] { "" + fjava, "-help" };
		Process proc = Runtime.getRuntime().exec(cmdarr);
		proc.waitFor(); // ignore return status
		String s = UtilIO.readText(proc.getInputStream());
	        if (s.indexOf(" -d32") >= 0) return true;
	    } catch (Exception e) {
	 	System.err.println("" + e);
	    }
	    return false;
	}		

	// do analysis
	public void doAnalysis(String sbmlText) {
	    Util.verbose("Performing SBW doAnalysis()");
	    try {
		GProject gproj = null;
	    	if (target == null) {
		    gproj = waitForProject();
	    	    target = createNewTarget(gproj);
		} else {
		    gproj = target.gproject();
		}
		sbmlText = convertSBML(sbmlText);
		String mml = server().translateModelText(
		    ASModel.TEXT_SBML, ASModel.TEXT_MML, sbmlText, null);
		PModel pmodel = target.pmodel();
		pmodel.modelSource.setVal(mml);
		pmodel.built.setVal(false);
		gproj.refresh();
		PushForward push = new PushForward(gproj, target);
		SwingUtilities.invokeLater(push);
	    } catch (Exception e) {
	    	msg(e);
	    }
	}

	// convert SBML text to level 2, version 1, assumes SBW connected
	private static String convertSBML(String text) throws Xcept {
	    try {
		// get an instance of the SBML support module
			edu.caltech.sbw.Module module = SBW.getModuleInstance(
		    "edu.caltech.NOMClipboard");
		// get hold of the NOM service
		Service service = module.findServiceByName("NOM");
		INOM inom = (INOM) service.getServiceObject(INOM.class);
		return inom.convertSBML(text, 2, 1);			
	    } catch (Exception e) {
	    	String txt = "SBML import error: ";
	        if (e instanceof SBWApplicationException) 
		    txt = txt + ((SBWApplicationException) e).getDetailedMessage();
		else
		    txt = txt + e.getMessage();
		throw new Xcept(txt);
	    }
	}

	// wait for project window to appear
	private GProject waitForProject() throws Xcept {
	    long timeOut = 10000; // 10 seconds 
	    long pollFreq = 200; // .2 second
	    long stopTime = System.currentTimeMillis() + timeOut; 
	    while (! isReady && System.currentTimeMillis() < stopTime) {	    
		try { Thread.sleep(pollFreq); }
		    catch (Exception e) { }
	    }
	    if (! isReady) throw new Xcept(
	    	"No project window for SBW model creation (" +
		(timeOut / 1000) + " second timeout)");
	    GNode.List gprojs = gmain.children(GProject.class);
	    if (gprojs.size() > 0) {
	    	return (GProject) gprojs.gnode(gprojs.size()-1);
	    }
	    throw new Xcept(
	    	"No project window for SBW model creation");
	}

	// create new target, within project
	protected GModel createNewTarget(GProject gproj) throws Xcept {
	    Project proj = gproj.project();
	    String pname = proj.newChildName("SBW", true);
	    PModel pmodel = new PModel(proj, pname);
	    return (GModel) gproj.add(pmodel);
	}

	// deliver message
	protected void msg(Exception e) {
	    if (target == null)
	    	gmain.warning("" + e);
	    else
	    	target.gproject().message(e);
	}

	// project is closing
	protected void projectClosing(GProject gproj) {
	    if (target == null) return;
	    if (target.gproject() == gproj)
	    	setTarget(null);
	}	

	// push forward
	protected class PushForward implements Runnable {
	    private GProject gproj;
	    private GModel gmodel;
	    protected PushForward(GProject gproj, GModel gmodel) {
	    	this.gproj = gproj;
		this.gmodel = gmodel;
	    }
	    public void run() { 
	        try {
		    gproj.pushFrameFront();
		    gproj.lefttabs().pushNode(gmodel);
		} catch (Exception e) {
		    System.err.println("" + e);
		}
	    }
	}

	// NOM interface
	public static interface INOM {
	    String convertSBML(String sbmlString, int level, int version) 
	    throws Exception;
	}

/* comment out in production versions, otherwise SBW exposes via reflection 
	// test SBML conversion
	public static void main(String[] args) throws Exception {
	    File f = new File(args[0]);
	    String text = UtilIO.readText(f);
	    SBW.connect();
	    text = convertSBML(text);
	    System.out.println(text);
	    SBW.disconnect();
	    System.exit(0); // otherwise hangs, AWT thread issue???
	}
*/
}


