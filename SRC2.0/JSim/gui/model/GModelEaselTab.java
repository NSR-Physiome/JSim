/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plug-in model easel tab

package JSim.gui.model;

import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;
import JSim.gui.plugin.*;

public class GModelEaselTab extends GNode 
implements GModelEasel.Callbacks {

	// state
	private JRootPane root;
	private JLabel inactive; // inactive label
	private String currVariant; // currently active variant
	private GModelEasel geasel; // easel plugin

	// constructor 
	public GModelEaselTab(GModel g) {
	    super(g, null);

	    // create widgets
	    root = new JRootPane();
	    inactive = new JLabel(
	    	"Easel plugin is currently unavailable");
	    root.setContentPane(inactive);
	    setJComp(root);
	}
	    
	// refresh
	public void refresh() {
	    super.refresh();

	    // current easel OK?
	    String variant = gmodel().pmodel().easelVariant.val();
	    if (Util.isBlank(variant)) return;
	    if (currVariant != null && variant.equals(currVariant))
	    	return;	

	    // load new plugin
	    try {
		currVariant = variant;
		Plugin plugin = gappl().plugins().plugin(
		    "ModelEasel", variant);
		if (plugin == null) throw new Xcept(
		    "Easel plugin \"" + variant + "\" not found");
		Class[] clss = new Class[] { GModelEasel.Callbacks.class };
		Object[] obj = new Object[] { this };
		geasel = (GModelEasel) plugin.newInstance(clss, obj);
		root.setContentPane(geasel.getJComponent());
	    } catch (Exception e) {
		root.setContentPane(new JLabel(e.getMessage()));
	    }

	}
		
	//// GModelEasel.Callbacks methods
	public void easelCompile() {
	    try {
	    	String text = geasel.getMML();
		gmodel().pmodel().modelSource.setVal(text);
		gmain().runJob(new GModelJob.Build(gmodel(), false));
	    } catch (Exception e) {
	    	gproject().message(e);
	    }
	}
	public void easelStateChanged(int flags) { } 
}
