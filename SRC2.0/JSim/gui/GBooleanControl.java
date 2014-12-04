/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Checkbox for BooleanControl

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GBooleanControl extends GControl implements ActionListener {

	// state
	private JCheckBox check;
	private String verifyCheckText;
	private String verifyUncheckText;

	// constructors
	public GBooleanControl(GNode p, BooleanControl c) {
	    super(p, c);
	    check = new JCheckBox();
	    setJComp(check);
	    check.addActionListener(this);
	}
	public GBooleanControl(GNode p, BooleanControl c, String text) {
	    this(p, c);
	    ((JCheckBox) jcomp()).setText(text);
	}

	// set properties
	public void setVerifyCheckText(String s) { verifyCheckText = s; }
	public void setVerifyUncheckText(String s) { verifyUncheckText = s; }

	// item changed action
	public void actionPerformed(ActionEvent event) {
	    try {
	    	if (cntlVal() == jcompVal()) return;
	    	if (! verify()) 
		    check.setSelected(cntlVal()); 
		else 
		    ((BooleanControl) cntl()).setVal(jcompVal());
	    	refreshAux();
	    } catch (Xcept e) {
		warning("could not update control " + cntl().name() + 
		    ": " + e.cleanMessage());
	    }
	}

	// is GUI change verified?
	private boolean verify() {
	    String text = jcompVal() ? verifyCheckText : verifyUncheckText;
	    if (text == null) return true;
	    int stat = JOptionPane.showConfirmDialog(
	    	gproject().jcomp(), text, "Confirm change",
		JOptionPane.YES_NO_OPTION);
	    return stat == JOptionPane.YES_OPTION;
	}

	// refresh
	public void refresh() {
	    if (cntlVal() == jcompVal()) return;
	    if (! refreshing) {
	    	check.setSelected(cntlVal());
		check.setEnabled(editable());
	    }
	    super.refresh();
	}

	// internal control & jcomp values
	public boolean cntlVal() {
	    return ((BooleanControl) cntl()).val();
	}
	public boolean jcompVal() {
	    return check.isSelected();
	}
}
