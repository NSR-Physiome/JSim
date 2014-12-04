/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// abstract project control

package JSim.gui;

import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

abstract public class GControl extends GNode {

	// state
	private Control cntl;  // if null, reimplement cntl()
	protected GNode.List auxNodes; // aux refresh list

	// constructor
	public GControl(GNode p, Control c) {
	    super(p, c);
	    cntl = c;
	    auxNodes = null;
	}

	// setJComp registers help
	public void setJComp(JComponent c) {
	    super.setJComp(c);
	    setHelp(c, this);
	    if (! (c instanceof JTextArea)) 
		cursorTraversal(c);
	}

	// cursor-key focus traversal
	public static void cursorTraversal(JComponent j) {
	    int id = KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS;
	    HashSet<AWTKeyStroke> keys = 
	    	new HashSet<AWTKeyStroke>(j.getFocusTraversalKeys(id));
	    keys.add(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0));
 	    j.setFocusTraversalKeys(id, keys);
	    id = KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS;
	    keys = 
	    	new HashSet<AWTKeyStroke>(j.getFocusTraversalKeys(id));
	    keys.add(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0));
 	    j.setFocusTraversalKeys(id, keys);
	}

	// create default GControl for Control
	public static GControl create(GNode p, Control c) {
	    if (c instanceof BooleanControl) 
		return new GBooleanControl(p, (BooleanControl) c);
	    if (c instanceof ChoiceControl) 
		return new GMenuControl(p, (ChoiceControl) c);
	    if (c instanceof TextControl) 
		return new GTextControl(p, (TextControl) c);
	    if (c instanceof StringControl) 
		if (((StringControl) c).pickList() != null) 
		    return new GMenuControl(p, c);
	    return new GStringControl(p, c, 8);
	}   

	// return attached control
	//    override if initial control not useful
	public Control cntl() { return cntl; }

	// add aux node
	public void addAuxNode(GNode n) {
	    if (auxNodes == null)
		auxNodes = new GNode.List();
	    auxNodes.add(n);
	}

	// refresh / revalidate aux nodes
	public void refreshAux() {
	    if (auxNodes == null) return;
	    for (int i=0; i<auxNodes.size(); i++) {
		GNode gnode = auxNodes.gnode(i);
		if (gnode.pnamed() != null)
		    gnode.pnamed().revalidate();
		gnode.refresh();
	    }
	}

	// key to help database
	public String helpKey() { return helpKey(null); }
	public String helpKey(String val) {
	    if (cntl() == null) return null;
	    String kval = (val == null) ? "" : "value";
	    String ret = null;

	    // model specific variable?
	    PNamed p = cntl().parent();
	    if (p instanceof PModelAssn) {
		ret = "model" + kval + GHelp.sep +
		    cntl().pmodel().name() + 
		    GHelp.sep + cntl().name();


	    // general project variable
	    } else { 
		String cname = (p instanceof FuncGenSlave) ?
		    p.name() : GHelp.shortClassName(p);
		ret = "control"  + kval + GHelp.sep + 
		    cname + GHelp.sep + cntl().name();
	    }

	    // valid model DataControl?
	    //    disable this 27 Jan 2005 to enable Pwgt/Cwgt/etc.
//	    if (cntl() instanceof DataControl) {
//		DataControl dcntl = (DataControl) cntl();
//		if (dcntl.valid() && dcntl.base() instanceof PModel) 
//		    ret = "model" + kval + GHelp.sep +
//			dcntl.base().name() + GHelp.sep +
//			dcntl.stringVal();
//	    }		    
		
	    // sub-value
	    if (val == null) return ret;
	    return ret + GHelp.sep + val;
	}	    

	// special query
	public PlotPage plotpage() { 
	    return (PlotPage) 
		parent().pnamed().ancestor(PlotPage.class); 
	}
}
