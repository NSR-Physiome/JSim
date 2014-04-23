/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// GUI specific action for single GNode

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import JSim.util.*;
import JSim.project.*;

public class GAction extends AbstractAction {

	// state
	private String name;
	public GNode gnode;
	public ActionEvent event;
	public KeyStroke accel;

	// constructor
	public GAction(GNode n, String na) {
	    super(na); 
	    name = na;
	    gnode = n;
	    setEnabled();
	}

	// accelerator
	public void setAccel(int keyChar) {
	    setAccel(keyChar, false);
	}
	public void setAccel(int keyChar, boolean shift) {
	    // CTRL on Linux/Win32, Option on MacOS
	    int mask = gnode.toolkit().getMenuShortcutKeyMask();
	    if (shift) mask |= InputEvent.SHIFT_MASK;
	    accel = KeyStroke.getKeyStroke(keyChar, mask, false);
	}

	// action performed
	public void actionPerformed(ActionEvent ev) {
	    event = ev;
	    try {
	    	GProject gproj = gnode.gproject();
		if (gproj != null) // actions may be for GMain 
		    gproj.statlineClear();
		if (! sbEnabled()) throw new Xcept(
		    "action is illegal at this time");
		doit();
	    } catch (Exception e) {
	        String msg = (e instanceof Xcept) ?
		    ((Xcept) e).cleanMessage() : e.toString();
		gnode.warning("Error performing \"" + 
		    getValue(Action.NAME) + "\": " + msg);
		if (gnode.gappl().stackTrace)
		    e.printStackTrace();
	    }
	}

	// do something
	public void doit() throws Xcept {
	    throw new Xcept("action not implemented");
	}

	// should this action be enabled?
	public boolean sbEnabled() { return true; }

	// reset enabled
	public void setEnabled() {
	    setEnabled(sbEnabled());
	}

	// revised menu item text
	public String revisedText() { return null; }

	// create associated JMenuItem with accelerator 
	public JMenuItem item() {
	    JMenuItem item = new JMenuItem(this);  
	    if (accel != null) 
	    	item.setAccelerator(accel);
	    gnode.setHelp(item, this);
	    return item;
	}

	// create radio menu item
	public JRadioButtonMenuItem radio() {
	    JRadioButtonMenuItem radio = new
		JRadioButtonMenuItem(this);
	    gnode.setHelp(radio, this);
	    return radio;
	}

	// button with menu sized font
	public JButton button() { 
	    return button(null, null, true); 
	}
	public JButton button(String s) {
	    return button(s, null, true);
	}
	public JButton button(String s, boolean big) {
	    return button(s, null, big);
	}
	public JButton button(Icon icon) {
	    return button("", icon, true);
	}
	public JButton button(String s, Icon icon, boolean big) {
	    JButton b = new JButton(this);
	    if (s != null) b.setText(s);
	    if (icon != null) {
	    	b.setIcon(icon);
		b.setMargin(new Insets(0,0,0,0));
	    }
	    if (big) b.setFont(gnode.glook().bigFont());
	    gnode.setHelp(b, this);
	    return b;
	}

	// query
	public String name() { return name; }

	// key to help database
	public String helpKey() {
	    return "action" + GHelp.sep +
		GHelp.shortClassName(gnode) + 
		GHelp.sep + name();
	}

	// GAction.List
	public static class List extends ArrayList<GAction> {
	    public List() { super(); }
	    public GAction gaction(int i) { return (GAction) get(i); }
	}

}
