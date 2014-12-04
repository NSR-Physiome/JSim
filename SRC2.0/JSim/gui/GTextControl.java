/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// multi-line text widget for any TextControl

package JSim.gui;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GTextControl extends GStringControl {

	// constructor
	public GTextControl(GNode p, TextControl c) {
	    super(p, c, 0);
	}

	// event processing
	public void keyTyped(KeyEvent e) {
	    changed = true;
	}

	// set text
	public void setText(String s) {
	    // MacOS Swing bug can't handle CRs in text
	    if (Util.isMacos()) s = Util.removeCR(s);

	    // conditional replacement improves editor undo
	    if (! s.equals(text.getText()))
	    	text.setText(s);
	}
}
