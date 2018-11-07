/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// top level control of GUI application or applet

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.applet.*;
import javax.swing.plaf.metal.*;

import org.w3c.dom.Element;

import java.io.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public class GMain extends GNode {

	// internal state
	private GAppl gappl;	// application environment
	private JApplet applet;	// applet, if any
	private GMemory gmem; // memory monitor
	private GSBW gsbw; // SBW connection
	private int projCt; // # projects created (not exisiting!)
	private GLook glook; // LookAndFeel manager
	private GHelpLinks helpLinks; // WWW help links
	public Element[] xmlBuffer; // for project paste
	public String textBuffer; // for GEditors
	public PrintStream saveErr; // error stream
	public GJobAsyncMonitor monitor; // job monitor
	public JLabel appletLabel; // if applet and newFrame
	private boolean disconnected; // has been disconnected
	private Boolean bkupProj;  // bkup project file after every run.

	// no GActions here!  GAction requires GProject

	// constructor inside applet
	public GMain(JApplet a, String[] args) {
	    super(null, null);
	    applet = a;
	    // establish Project window,  exit on error
	    saveErr = System.err;
	    GProject gproj = null;
	    Project proj = null;
	    try {

		// create application environment from switches
		gappl = new GAppl(this, args);
		
		// memory monitor
		gmem = new GMemory(this);

		// SBW initialization
		gsbw = new GSBW(this);

		// look and feel manager
		glook = new GLook(this, gappl.lookName, gappl.rgb(),
		    gappl.fontSize(),  gappl.winSize());
		bkupProj = new Boolean(gappl.bkupProj) ;  // Default to no bkup?
		if (bkupProj == null) {
			bkupProj = new Boolean(false);
		}

		// help links init
		helpLinks = new GHelpLinks(this);

		// create initial project
		JRootPane root = isApplet() ? applet.getRootPane() : null;
		JRootPane proot = gappl.newFrame ? null : root;
		if (root != proot) {
		    appletLabel = new JLabel(glook.jsimIcon());
		    root.setContentPane(appletLabel);
		}
		gproj = new GProject(this, proot, true);
	    	proj = gproj.project();

	    } catch (Exception e) {
		String msg = (e instanceof Xcept) ?
		    e.getMessage() : e.toString();
		warning("==== JSIM Fatal Error\n" + msg);
		if (gappl == null || gappl.stackTrace) 
		    e.printStackTrace();
		if (server() != null) 
		    server().disconnect();
		errorPopup(null, msg);
		System.exit(1);
	    }

	    // initialize/load project,  errors to Project window
	    try {
		// load stuff into project
		gproj.setFile(gappl.projFile);
		if (gappl.projFile == null) 
		    gproj.addDefaultTabs();
		else {
		    gproj.message("Reading project file " + 
			gappl.projFile + "\n");
		    proj.importXML(gappl.projFile);
		}
		for (int i=0; i<gappl.projFileLoads.size(); i++) {
		    JSReadable r = gappl.projFileLoads.readable(i);
		    if (! gproj.skipDubious(r))
		        proj.load(r);
		}
	    } catch (Xcept e) {
		gproj.message(e);
		errorPopup(gproj.jcomp(), e.cleanMessage());
	    }

	    // load stuff into project,  errors to Project window
	    try {
	    	proj.revalidate();
	    	gproj.load(gappl().demoMode);
	    } catch (Xcept e) {
		gproj.message(e);
		errorPopup(gproj.jcomp(), e.cleanMessage());
	    }

	    if (! isApplet()) GMacosAdapter.connect(this);

	    gsbw().setReady();
	}

	// system-wide utilities
	public boolean isApplet() { return applet != null; }
	public AppletContext getAppletContext() {
	    if (applet == null) return null;
	    return applet.getAppletContext();
	}
	public GMemory gmem() { return gmem; }
	public GLook glook() { return glook; }
	public GAppl gappl() { return gappl; }
	public GSBW gsbw() { return gsbw; }
	public GHelpLinks helpLinks() { return helpLinks; }
	public void setBkupProj(Boolean b) {
		bkupProj = b;
	}
	public Boolean getBkupProj() {return bkupProj; }

	public void warning(String s) {
	    if (isApplet())
		applet.showStatus(s);
	    else
		System.err.println(s);
	}
	public String newProjectName() {
	    return "proj" + ++projCt;
	}
	public boolean editable() { return true; }

	// update JSim components UI for new LookAndFeel
	protected void updateUI() {
	    for (int i=0; i<nChild(); i++) {
		JComponent c = child(i).jcomp();
		if (c == null) continue;
	        SwingUtilities.updateComponentTreeUI(c);
	    }
	    lookUpdated();
	    refresh();
	}

	// redirect stderr
	public void nullErr() {
	    PrintStream nullErr = new PrintStream(
		new NullOutputStream());
	    try {
	    	System.setErr(nullErr);
	    } catch (SecurityException e) { 
		// nothing useful to do
	    }
	}
	public void origErr() {
	    try {
		System.setErr(saveErr);
	    } catch (SecurityException e)  { 
		// nothing useful to do
	    }
	}

	// set edit buffer for project2project paste
	public void setEditBuffer(PNamed.List list) throws Xcept {
	    if (list == null) {
		xmlBuffer = null;
		return;
	    }
	    xmlBuffer = new Element[list.size()];
	    for (int i=0; i<list.size(); i++) {
		PNamed pnamed = list.pnamed(i);
		xmlBuffer[i] = pnamed.exportXML();
	    }
	}

	// run batch job
	public void runJob(GJob gjob) throws Xcept {
	    runJob(gjob, false);
	}
	public void runJob(GJob gjob, boolean wait) throws Xcept {
	    boolean sync = wait;  // quick fix
	    if ((gappl().proto & 2) > 0) sync = true;
	    if ((gappl().proto & 1) > 0) sync = false;
	    sync = false; // ??? EB TESTING 15 Nov 2004
	    // ??? if seems OK, can get rid of GJobSyncMonitor
	    if (sync) 
		runJobSync(gjob);
	    else 
		runJobAsync(gjob, wait);
	}

	// works only with Async monitor
	public boolean jobRunning() {
	    if ((gappl().proto & 2) > 0) return false;
	    if (monitor == null) return false;
	    return monitor.jobQueued();
	}

	// run job asynch
	private void runJobAsync(GJob gjob, boolean wait) 
	throws Xcept {
	    if (monitor == null)
		monitor = new GJobAsyncMonitor(this);
	    monitor.enqueue(gjob, wait);
	}

	// run syncronously
	private void runJobSync(GJob gjob) throws Xcept {

	    // create and start monitor
	    GJobSyncMonitor jobMonitor = new GJobSyncMonitor(gjob);
	    jobMonitor.start();
	    
	    // popup dialog and wait till cancelled, killed
	    //     or terminated by jobMonitor
	    //     then let jobMonitor decide on action
	    jobMonitor.dialogShow();
	    jobMonitor.dialogDone();

	    // make sure jobMonitor completes	    
	    try {
		jobMonitor.join();
	    	jobMonitor = null;
	    } catch (Exception e) {
		warning(
		   "job monitor not terminated: " + e);
	    }

	    gjob.gpost();
	}
	    
	// load file called by OS  while application is running
	public void loadFile(File f) {
	    int ngproj = ngprojs();
	    if (ngproj < 1) return;

	    // can't load subfiles w/ multiple open projects 
	    GNode.List gprojs = gprojs();
	    if (ngproj > 1 && !UtilIO.fileSuffix(f).equals("proj")) {
		String msg = "When multiple projects are open, " +
		    "you must open file " + f.getName() + 
		    " via the project Add menu";
		for (int i=0; i<gprojs.size(); i++) {
		    GProject gproj = (GProject) gprojs.get(i);
		    gproj.warning(msg);
		}
		return;
	    }

	    // open file in context of single existing project
	    GProject gproj = (GProject) gprojs.get(0);
	    try {
		gproj.loadFile(f);
	    } catch (Xcept e) {
		gproj.warning(e.cleanMessage());
	    }
	}

	// exit requested, throw Xcept if cancelled
	public void exitRequest() throws Xcept {
	    GNode.List gprojs = gprojs();
	    for (int i=gprojs.size()-1; i>=0; i--) {
		GProject gproj = (GProject) gprojs.get(i);
		gproj.closeRequest();
	    }
	    exitNow(); // just to be safe, should never be reached
	}
	
	// project s/b down, exit now
	public void exitNow() throws Xcept {
	    if (appletLabel != null) appletLabel.setIcon(null);
	    System.err.println("JSim version " +  
	    	Util.version() + " exiting now");
	    if (! disconnected) { // disconnect only once
	    	server().disconnect();
	        disconnected = true; 
	    }
	    if (! isApplet()) System.exit(0);
	}

	// fatal error popup
	public void errorPopup(Component parent, String msg) {
	    JOptionPane.showMessageDialog(parent,
		msg, "JSim Error", 
		JOptionPane.INFORMATION_MESSAGE);
	}   

	// projects in app
	public GNode.List gprojs() { return children(GProject.class); }
	public int ngprojs() { return gprojs().size(); }
}

