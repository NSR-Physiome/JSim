/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// ComboBox editor for Control
//    This code is not currently being used.
//    JComboBox is a really flakey widget
//	come back and fix when have lots of time to waste

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GComboControl extends GControl implements ItemListener {

	// state
	private JComboBox combo;
	private String[] labels;

	// constructor
	public GComboControl(GNode p, Control c) {
	    super(p, c);
	    labels = makeLabels();
	    combo = new JComboBox(labels);
	    setJComp(combo);
	    refresh();
	    combo.addItemListener(this);
	    combo.setMaximumRowCount(20);
	}

	// make labels
	public String[] makeLabels() {
	    String[] labels;
	    if (cntl() instanceof ChoiceControl) {
	    	ChoiceControl c = (ChoiceControl) cntl();
	    	labels = new String[c.nLabels()];
	    	for (int i=0; i<labels.length; i++)
		    labels[i] = c.stringVal(i);
	    } else if (cntl() instanceof BooleanControl) {
	    	BooleanControl c = (BooleanControl) cntl();
	    	labels = new String[2];
	    	labels[0] = c.stringVal(true);
	    	labels[1] = c.stringVal(false);
	    } else if (cntl() instanceof StringControl) {
	    	StringControl c = (StringControl) cntl();
		StringList slist = c.pickList();
		int ct = (slist==null) ? 0 : slist.size(); 
		labels = new String[ct];
		for (int i=0; i<ct; i++)
		    labels[i] = slist.str(i);
	    } else 
		labels = new String[] { "???" };
	    return labels;
	}

	// reset labels,  must disable events
	public void resetLabels() {
	    combo.removeItemListener(this);
	    combo.setEditable(cntl() instanceof StringControl);
	    labels = makeLabels();
	    combo.removeAllItems();
	    for (int i=0; i<labels.length; i++) 
		combo.addItem(labels[i]);
	    combo.addItemListener(this);
	}

	// item changed action
	public void itemStateChanged(ItemEvent event) {
System.err.println("combo state changed: comp=" + jcompText() +
    " cntl=" + cntl().stringVal());
	    if (event.getStateChange() != ItemEvent.SELECTED) return;
	    int n = jcompVal();
	    if (cntlVal() == n) return;

	    try {
		Control c = cntl();
	    	if (c instanceof ChoiceControl) 
		    ((ChoiceControl) c).setVal(n);
	    	else if (c instanceof IntControl)
		    ((IntControl) c).setVal(n);
	    	else if (c instanceof BooleanControl) 
		    ((BooleanControl) c).setVal(n==0);
		else if (c instanceof StringControl) 
		    ((StringControl) c).setVal(jcompText());
		refreshAux();
	    } catch (Xcept e) {
		warning(e.cleanMessage());
	    }
	}

	// refresh
	public void refresh() {
System.err.println("combo refresh " + cntlVal() + " " + jcompVal());
	    if (cntlVal() == jcompVal()) return;
	    if (! refreshing) {
		if (cntl() instanceof StringControl)
		    setJCompText(cntl().stringVal());
		else 
	    	    combo.setSelectedIndex(cntlVal());
	    }
	    super.refresh();
	}

	// internal control & jcomp values
	public int cntlVal() {
	    int n=0;
	    Control c = cntl();
	    if (c instanceof ChoiceControl) 
		n = ((ChoiceControl) c).val();
	    else if (c instanceof IntControl) {
		n = ((IntControl) c).val();
		if (n<0) n=1;
	    } else if (c instanceof BooleanControl) 
		n = ((BooleanControl) c).val() ? 0 : 1;
	    else if (c instanceof StringControl) 
		n = -2;
	    return n;
	}
	public int jcompVal() {
	    return combo.getSelectedIndex();
	}
	public String jcompText() {
//	    Object item = combo.getEditor().getItem();
//	    Object item = combo.getSelectedItem();
	    JTextField f = (JTextField) 
		combo.getEditor().getEditorComponent();
	    String s = f.getText();
	    return s;
	}

	public void setJCompText(String s) {
	    JTextField f = (JTextField) 
		combo.getEditor().getEditorComponent();
	    f.setText(s);
	}

	public void setValidBlack(boolean b) {
	    System.err.println("GComboControl.validBlack() not implemented");
	}
}
