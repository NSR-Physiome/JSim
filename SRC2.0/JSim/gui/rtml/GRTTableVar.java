/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// one variable within a table

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;

public class GRTTableVar extends GRTVar {

	// state
	private GRTTable table; // in this table

	// constructor
	public GRTTableVar(GRTTable t, Element e) {
	    super(t, e);
	    table = t;
	}

	// make JComponent
	public void makeJComp() {

	    // make GRTVar stuff
	    makeStuff(table.unitWidth() > 0);

	    // tablevar panel / label component
	    JPanel panel = new JPanel(null);
	    setJComp(panel);
	    JLabel label = new JLabel(label());
	    setFont(label);
	    label.setLocation(0,0);
	    label.setSize(table.labelDim());
	    panel.add(label);
	    jvalue().setLocation(pix(table.labelWidth()), 0);
	    jvalue().setSize(table.valueDim());
	    panel.add(jvalue());
	    setHelp(label, jvalue());		

	    // unit component
	    if (junit() != null) {
		double x = table.labelWidth() + table.valueWidth();
		junit().setLocation(pix(x), 0);		
	    	junit().setSize(table.unitDim());
		panel.add(junit());
	    	setHelp(junit(), jvalue());		
	    }

	    // set panel dimensions
	    panel.setPreferredSize(
		new Dimension(pix(table.totalWidth()), pix(GRTTable.HMULT))); 
	}

}

