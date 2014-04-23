/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// table of variables (or others?)

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GRTTable extends GRTPageItem {

	// constants
	public static final double HMULT = 1.5;
	public static final double WLABEL = 7;
	public static final double WVALUE = 9;
	public static final double WUNIT = 0;
	public static final double WTOTAL = WLABEL+WVALUE+WUNIT;

	// state
	private double[] widths;
	private boolean useWidths;
	private GNode.List items;

	// constructor
	public GRTTable(GRTNode p, Element e) {
	    super(p, e);
	    widths = parseN(e.getAttribute("widths"), 3);
	    useWidths = ! Double.isNaN(widths[0]);
	    items = new GNode.List();
	    addXMLChildren(e);
	}

	// create child from XML Element
	public void addXMLChild(Element e) {
	    String n = e.getNodeName();
	    if (n.equals("var")) { 
		GRTTableVar v = new GRTTableVar(this, e);
		items.add(v);
	    } else 
		super.addXMLChild(e);
	}

	// make JComponent
	public void makeJComp() {
	    GridLayout layout = new GridLayout(items.size(), 1);
	    JPanel panel = new JPanel(layout);
	    setJComp(panel);
	    for (int i=0; i<items.size(); i++) {
		GRTTableVar var = (GRTTableVar) items.get(i);
		var.makeJComp();
		panel.add(var.jcomp());
	    }
	    setBounds();
	}

	// query
	public double labelWidth() { return useWidths ? widths[0] : WLABEL; }
	public double valueWidth() { return useWidths ? widths[1] : WVALUE; }
	public double unitWidth() { return useWidths ? widths[2] : WUNIT; }
	public double totalWidth() { 
	    return labelWidth() + valueWidth() + unitWidth(); 
	}
	public Dimension labelDim() {
	    return new Dimension(pix(labelWidth()), pix(HMULT));
	}
	public Dimension valueDim() {
	    return new Dimension(pix(valueWidth()), pix(HMULT));
	}
	public Dimension unitDim() {
	    return new Dimension(pix(unitWidth()), pix(HMULT));
	}
}

