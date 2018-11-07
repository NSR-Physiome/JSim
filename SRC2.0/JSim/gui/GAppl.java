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
import java.net.URL;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.*;
import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;
import JSim.gui.graph.*;
import JSim.gui.demograph.*;
import JSim.gui.gsgraph.*;
import JSim.aserver.*;

public class GAppl extends PApplication {

	// GUI specific args
	public int[] rgb;  // background color
	public int fontSize;  // font size
	public int[] winSize;  // project window size
	public String lookName; // user requested look&feel
	public boolean noprefs; // ignore user preferences
	public boolean liveUpdate; // debug: live plot update
	public int proto; // prototype flags (devel only)
	private int graph; // which GraphRender?
	public int tabType; // which project tab class?
	public boolean newFrame; // new frame even if applet
	public boolean demoMode; // compile/push fwd model & plot
	public Boolean bkupProj;  // Auto Backup proj file

	public StringList demoExprs; // plot exprs for demo plot
	public String sbwCommand; // -sbw... argument suffix

	// state
	private GMain gmain;

	// args constructor
	public GAppl(GMain m, String[] args) throws Xcept {
	    super(args);
	    gmain = m;

	    // load user preferences
	    if (noprefs) return;
	    try {
	    	File f = preferencesFile();
	    	if (f == null || ! f.exists()) return;
		GPrefs gprefs = new GPrefs(this, f);
		if (rgb == null) rgb = gprefs.rgb;
		if (fontSize == 0) fontSize = gprefs.fontSize;
		if (winSize == null) winSize = gprefs.winSize;
		if(gprefs.bkupProj !=null) {	
			bkupProj = new Boolean(gprefs.bkupProj);
		}
		else bkupProj = new Boolean(false);
	    } catch (Xcept e) {
		System.err.println(
		    "Error loading user preferences file: " +
		     e.cleanMessage());
	    }
	}

	// parse extra args
	protected void parse_xargs(String[] args) throws Xcept {
	    noprefs = false;
	    liveUpdate = true;
	    proto = 0;
	    rgb = null;
	    fontSize = 0;
	    winSize = null;
	    graph = 0;
		bkupProj = new Boolean(false);
	    // tabType = Util.isJava15() ? 0 : 1; // 1.6 => GCompactTabs menu bug
	    tabType = 2; // new for 1.6.91
	    newFrame = false;
	    lookName = "Metal";

	    // loop over args
	    if (args == null) args = new String[0];
	    for (int i=0; i<args.length; i++) {
		if (args[i].equals("-rgb") && i+1<args.length) {
		    rgb = GPrefs.intArrVal(args[++i]);
		} else if (args[i].equals("-fs") && i+1<args.length) {
		    fontSize = Util.toInt(args[++i]);
		} else if (args[i].equals("-ws") && i+1<args.length) {
		    winSize = GPrefs.intArrVal(args[++i]);
		} else if (args[i].equals("-look") && i+1<args.length) {
		    lookName = args[++i];
		} else if (args[i].equals("-noprefs")) {
		    noprefs = true;
		} else if (args[i].equals("-tabType") && i+1<args.length) {
		    tabType = Util.toInt(args[++i]);
		} else if (args[i].equals("-graph") && i+1<args.length) {
		    graph = Util.toInt(args[++i]);
		} else if (args[i].equals("-liveUpdate")) {
		    liveUpdate = true;
		} else if (args[i].equals("-noLiveUpdate")) {
		    liveUpdate = false;
		} else if (args[i].equals("-proto") && i+1<args.length) {
		    proto = Util.toInt(args[++i]);
		} else if (args[i].equals("-popout")) {
		    newFrame = true;
		} else if (args[i].startsWith("-sbw")) {
		    sbwCommand = args[i].substring(4);
		} else if (args[i].equals("-demo")) {
		    demoMode = true;
		    demoExprs = new StringList();
		    while (i+1<args.length && args[i+1].charAt(0) != '-') 
			demoExprs.add(args[++i]);		        
		} else {
		    throw new Xcept(
			"Illegal switch usage: " + args[i] + 
		        "\nJSim switches:\n" + usage());
		}
	    }
	}

	// preferences file
	public File preferencesFile() {
	    try {
	    	String home = System.getProperty("user.home");
	    	return new File(home + File.separator +
		    ".jsim" + File.separator + "local" + 
		    File.separator + "preferences.xml");
	    } catch (SecurityException e) {
		return null;
	    }
	} 

	// preferences query
	public int[] rgb() { return rgb; }
	public int fontSize() { return fontSize; }
	public int[] winSize() { return winSize; }
	public boolean isGUI() { return true; }

	// switch usage message
	public String usage() throws Xcept {
	    String res = "jsimUsage.txt";
	    URL url = getClass().getResource(res);
	    if (url == null) throw new Xcept(
		"Resource \"" + res + "\" not found");
	    return UtilIO.readText(url);
	}	    
	
	// import XML message
	public void importXMLMessage(Project proj, String s) {
	    GProject gproj = (GProject) gmain.child(proj);
	    if (gproj == null)
		Util.verbose(s);
	    else
		if (Util.verbose)
		    gproj.message("  " + s);
	}

  	// print message from server
	public void message(ASInfo.Message msg) {
	    String s = msg.warning ? "warning: " : "";
	    s = s + msg.text;
	    PModel pmodel = pmodelForID(msg.modelID);
	    GProject gproj = null;
	    if (pmodel != null) {
	        Project proj = pmodel.project();
	    	gproj = (GProject) gmain.child(proj);
	    }
	    if (gproj == null)
	    	System.err.println(s);
	    else
	    	gproj.message("Model " + pmodel.name() + " " + s);	
	}

	// create GraphRender
	public GraphRender newGraphRender(GraphContext ctxt) {
	    switch (graph) {
	    case 1: return new DemoGraph(ctxt);
	    }
	    return new GSGraph(ctxt);
	}
}

