/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// custom menu

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GMenu extends JMenu implements MenuListener {

	// constructor
	public GMenu(String s, GNode gnode) {
	    super(s);
	    gnode.setHelp(this, gnode);
	    addMenuListener(this);
	}

	// menu going to pop up
	public void menuSelected(MenuEvent e) {
	    if (e.getSource() != this) return;
	    JMenu menu = (JMenu) e.getSource();
	    Component[] comps = getMenuComponents();
	    for (int i=0; i<comps.length; i++) {
		if (! (comps[i] instanceof AbstractButton))
		    continue;
		AbstractButton button = (AbstractButton) comps[i];
		if (!(button.getAction() instanceof GAction))
		    continue;
		GAction gact = (GAction) button.getAction();
		gact.setEnabled();
		String revText = gact.revisedText();
		if (revText != null) button.setText(revText);
	    }
	}

	// other menu listener methods
	public void menuCanceled(MenuEvent e) {	}
	public void menuDeselected(MenuEvent e) { }

}