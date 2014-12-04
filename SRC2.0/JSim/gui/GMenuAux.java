/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// auxilliary button for JPopupMenu for GStringControl

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.metal.MetalComboBoxIcon;

import JSim.util.*;
import JSim.project.*;

public class GMenuAux extends GMenuControl {

	// state
	private GStringControl gscntl;
	private Icon buttonIcon;

	// constructor
	public GMenuAux(GNode p, GStringControl gs) {
	    super(p, null, true);
	    gscntl = gs;

	    // tiny button
	    buttonIcon = new MetalComboBoxIcon();
	    button.setIcon(buttonIcon);
	    button.setText(null);
	    Dimension sdim = gs.jcomp().getPreferredSize();
	    int hm = (sdim.height - buttonIcon.getIconHeight())/2;
	    button.setMargin(new Insets(hm, 1, hm, 1));
	}

	// same control as gscntl
	public Control cntl() { 
	    return (gscntl == null) ? null : gscntl.cntl();
	}

	// set button size
	protected void buttonSize(Dimension maxDim) { 
	}

	// menu item was selected
	public void actionPerformed(ActionEvent event) {
	    menuWaiting = false;
	    if (! (event.getSource() instanceof Item)) return;
	    int n = ((Item) event.getSource()).n;
	    String s = labels[n];
	    if (s.equals(cntl().stringVal())) return;

	    // update 
//	    gscntl.text().setText(s);
	    try {
	    	cntl().setVal(s);
	    	gscntl.refresh();
	    	gscntl.refreshAux();
	    } catch (Xcept e) {
		warning(e.cleanMessage());
	    }
	}

	// refresh
	public void refresh() {
	    if (! refreshing) {
		if (cntl() instanceof StringControl) {
		    StringControl scntl = (StringControl) cntl();
		    if (scntl.pickList() != null)
			resetLabels();
		}
		button.setEnabled(editable());
	    }
	    refreshing = false;
	}

}

