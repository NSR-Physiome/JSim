/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// graph in Page

package JSim.gui.rtml;

import javax.swing.*;
import javax.swing.border.*;
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
import JSim.gui.graph.*;

public class GRTGraph extends GRTPageItem {
	private GraphRender render;
	private String expr;

	// constructor
	public GRTGraph(GRTNode p, Element e) {
	    super(p, e);
	    addXMLChildren(e);
	    expr = e.getAttribute("expr");
	}

	// make JComponent
	public void makeJComp() {
	    render = gappl().newGraphRender(null);
	    render.jcomp().setBorder(new EtchedBorder());
	    setJComp(render.jcomp());
	    setFont();
	    setBounds();
	}

	// refresh
	public void refresh() {
	    super.refresh();
	    PNamed pdata = gmodel().pmodel().vars().child(expr);
	    try {
	    	if (pdata == null || !(pdata instanceof FuncGen)) 
		    throw new Xcept ("FuncGen " + expr + " not found");
	    	FuncGen fgen = (FuncGen) pdata;
		RealNData data = fgen.previewData();
		if (data == null) 
		    throw new Xcept("no preview data");
		showData(data);
	    } catch (Xcept e) {
		showError(e.cleanMessage());
	    }
	}

	// show layout
	public void showLayout(GraphLayout layout) {
	    try {
		render.setGraphLayout(layout, null);
	    } catch (Exception e) {
		System.err.println("RTML refresh: " + e);
		e.printStackTrace();
	    }
	}

	// error
	public void showError(String msg) {
	    GraphLayout l = new GraphLayout();
	    l.fg = Color.black;
	    l.bg = Color.white;
	    l.fontSize = glook().fontSize();
	    l.title = "Preview";
	    l.errorMsg = msg;
	    l.style = Plot.XY_PLOT;
	    l.xaxis = new GraphLayout.Axis(); // useless
	    l.yaxis = new GraphLayout.Axis(); // useless
	    showLayout(l);
	}

	// show data
	public void showData(RealNData data) throws Xcept {
	    if (data.ndim() != 1) throw new Xcept(
		"GRTGraph data only supports 1D data");
	    data.calcExtrema();
	    GridData grid = data.grid(0);
	    GraphLayout l = new GraphLayout();
	    l.fg = Color.black;
	    l.bg = Color.white;
	    l.fontSize = glook().fontSize();
	    l.title = "Preview";
	    l.style = Plot.XY_PLOT;
	    l.xaxis = new GraphLayout.Axis(); 
	    l.xaxis.auto = true;
	    l.xaxis.min = grid.min();
	    l.xaxis.max = grid.max();
	    l.yaxis = new GraphLayout.Axis(); 
	    l.yaxis.auto = true;
	    l.yaxis.min = data.min();
	    l.yaxis.max = data.max();
	    GraphData gdata = new GraphData();
	    gdata.label = expr;
	    gdata.x = grid.samples();
	    gdata.y = data.samples();
	    gdata.color = Color.blue;
	    gdata.shape = GraphData.SHAPE_NONE;
	    gdata.thickness = GraphData.LINE_MEDIUM;
	    gdata.size = 4; 
	    gdata.line = GraphData.LINE_SOLID;
	    l.data = new GraphData[] { gdata };
	    showLayout(l);
	}
}

