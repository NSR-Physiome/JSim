/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plug-in graphic tab

package JSim.gui;

import java.io.File;
import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;
import java.lang.reflect.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import JSim.project.*;
import JSim.gui.plugin.*;



public class GGraphicTab extends GNode implements GGraphic.Callbacks {

	// state
	private PGraphic pgraphic;
	private JRootPane root;
	private JLabel inactive; // inactive label
	private String currVariant; // currently active variant
	private GGraphic ggraphic; // graphics plugin
	private PModel pmodel; // last run PModel
    	private boolean isLive; // currently running?

	// constructor 
	public GGraphicTab(GNode g, PGraphic p) throws Xcept {
	    super(g, p);
	    pgraphic = p;

	    // create widgets
	    root = new JRootPane();
	    inactive = new JLabel(
	    	"Graphic plugin is currently unavailable");
	    root.setContentPane(inactive);
	    setJComp(root);
	    gproject().addTab(this);
	}
	    
	// refresh
	public void refresh() {
	    super.refresh();

	    // current ggraphic OK?
	    String variant = pgraphic.variant.val();
	    if (Util.isBlank(variant)) return;
	    if (currVariant != null && variant.equals(currVariant))
	    	return;	
	    currVariant = variant;

	    // load new plugin
	    try {
		Plugin plugin = pgraphic.appl().plugins().plugin(
		    "Graphic", variant);
		if (plugin == null) throw new Xcept(
		    "Graphic plugin \"" + variant + "\" not found");
		Class[] clss = new Class[] { GGraphic.Callbacks.class };
		Object[] obj = new Object[] { this };
		ggraphic = (GGraphic) plugin.newInstance(clss, obj);
		root.setContentPane(ggraphic.getJComponent());
	    } catch (Exception e) {
	        String msg = e.toString();
		root.setContentPane(new JLabel(msg));
	    }

	    // attempt to load state
	    try {
		Element gstate = pgraphic.getGraphicState();
		ggraphic.importXML(gstate);
	    } catch (Exception e) {
		gproject().message(e);
	    }

	}

	// model job start/stop
	public void jobStartStop(PJob pjob, boolean start) {
	    if (pjob instanceof PModelBuildJob) 
	    	pmodel = start ? pjob.pmodel() : null;
	    else
	    	pmodel = pjob.pmodel();
	    if (ggraphic == null) return;
	    try {
	     	if (start)
		    ggraphic.jobStarted(pjob);
		else
		    ggraphic.jobStopped(pjob);
	    } catch (Exception e) {
	    	gproject().message(e);
	    }
	}

	// export state to PGraphic
 	public void exportGraphicState() throws Xcept {
	    if (ggraphic == null) return;
  	    Document doc = UtilXML.createDoc(PGraphic.K_STATE);
	    Element e = doc.getDocumentElement();
	    try {
	    	ggraphic.exportXML(e);
		pgraphic.setGraphicState(e);
	    } catch (Exception x) {
	    	throw Xcept.wrap(x);
	    }
	}
	
	//// callbacks

	// select model
	public PModel userModelSelect(Component parent) {
	    return (PModel) userSelect(parent, "model", PModel.class);
	}

	// select dataset
	public PDataSet userDataSetSelect(Component parent) {
	    return (PDataSet) userSelect(parent, "data set", PDataSet.class);
	}

	// select items of class
	public PNamed userSelect(Component parent, String label, Class clss) {
	    PNamed.List list = getProject().children(clss);
	    int n = list.size();
	    if (n == 0) return null;
	    if (n == 1) return list.pnamed(0);
	    String[] names = new String[n];
	    for (int i=0; i<n; i++) 
	    	names[i] = list.pnamed(i).name();
	    String name = (String) JOptionPane.showInputDialog(parent,
	    	"Select " + label, pgraphic.name() + " plugin",
		JOptionPane.QUESTION_MESSAGE, null, names, names[0]);
	    if (name == null) return null;
	    return getProject().child(name);
	}

	// get  project
	public Project getProject() { return gproject().project(); }

}
