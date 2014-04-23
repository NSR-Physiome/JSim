/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// RadioButton for BooleanControl

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GBooleanRadioControl extends GControl {

	// state
	private JRadioButtonMenuItem radio;
	private GAction action;

	// constructors
	public GBooleanRadioControl(GNode p, BooleanControl c) {
	    this(p, c, c.name());
	}
	public GBooleanRadioControl(GNode p, BooleanControl c,
	String actionText) {
	    super(p, c);

	    action = new GAction(p, actionText) {
	    	public void doit() throws Xcept {
		    BooleanControl bcntl = (BooleanControl) cntl();
		    bcntl.setVal(radio.isSelected());
		    refreshAux();
		}
 	    };
	    radio = new JRadioButtonMenuItem(action);
	    setJComp(radio);
	}

	// refresh
	public void refresh() {
	    if (cntlVal() == jcompVal()) return;
	    if (! refreshing) {
	    	radio.setSelected(cntlVal());
	    }
	    super.refresh();
	}

	// internal control & jcomp values
	public boolean cntlVal() {
	    return ((BooleanControl) cntl()).val();
	}
	public boolean jcompVal() {
	    return radio.isSelected();
	}
}
