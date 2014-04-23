/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Left-hand project tabs

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GLeftProj extends GTabs {

	// constructor
	public GLeftProj(GNode g, PNamed p) {
	    super(g, p, true);
	}

	// refresh 
	public void refresh() {
	    super.refresh();
	    setParsetIcons();
	}

	// change listener
	public void stateChanged(ChangeEvent e) {
	    super.stateChanged(e);
	    setParsetIcons();
	}

	// set parset icons regular or select
	private void setParsetIcons() {

	    // determine highlighted pset,  if any
	    int i = tabs.getSelectedIndex();
	    if (i < 0 || i >= gnodes.size()) return;
	    GNode gnode = gnodes.gnode(i);
	    String pset = "";
//	    if (gnode instanceof GParSet)
//		pset = gnode.pnamed().name();
	    if (gnode instanceof GModel) {
		PModel pmodel = ((GModel) gnode).pmodel();
		pset = pmodel.parSetName.stringVal();
	    }

	    // modify GParSet icons for selected
	    for (i=0; i<gnodes.size(); i++) {
		gnode = gnodes.gnode(i);
		if (! (gnode instanceof GParSet)) continue;
		boolean select = gnode.pnamed().name().equals(pset);
		Icon icon = glook().parsetIcon(select);
		tabs.setIconAt(i, icon);
	    }
	}
}

