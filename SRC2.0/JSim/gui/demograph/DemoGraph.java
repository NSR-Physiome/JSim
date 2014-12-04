/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// simple example of GraphRender for development/debugging

package JSim.gui.demograph;

import java.io.File;
import java.awt.*; 
import java.awt.event.*;
import java.awt.print.*;
import javax.swing.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class DemoGraph extends JPanel implements GraphRender,
ComponentListener, MouseListener {

	// instance info
	protected GraphContext ctxt; // context for callbacks
	protected GraphLayout layout; // layout to draw
	protected GraphData.Subset[] subsets; // live subsets?
	protected DemoGraphXY graphXY; // XY graph routine
	protected DemoGraphMap graphMap; // Colormap routine
	protected double[] zoom1;  // starting zoom coords

	// constructor
	public DemoGraph(GraphContext c) {
	    super();
	    ctxt = c;
	    addComponentListener(this);
	    addMouseListener(this);
	}

	// minimal paint routine,  just for debugging
	public void paint (Graphics g) {
	    long t0 = System.currentTimeMillis();
	    Dimension dim = getSize();
	    int w = dim.width;
	    int h = dim.height;

	    if (layout == null) {
	    	g.setColor(Color.white);
	    	g.fillRect(0,0,w,h);
		g.drawString("No data", w/2, h/10);
		return;
	    }
 
	    if (layout.style == GraphLayout.STYLE_2DLINE) {
		if (graphXY == null)
		    graphXY = new DemoGraphXY(this);
		graphXY.paint(g, w, h);
	    } else {
		if (graphMap == null)
		    graphMap = new DemoGraphMap(this);
		graphMap.paint(g, w, h);
	    }
	    System.err.println("paint time = " + 	
		(System.currentTimeMillis()-t0) + " msec");
	}

	// get/clear Subsets
	protected GraphData.Subset[] getClearSubsets() {
	    GraphData.Subset[] s = subsets;
	    subsets = null;
	    return s;
	}	    

	// listeners
	public void componentHidden(ComponentEvent e) { }
	public void componentMoved(ComponentEvent e) { }
	public void componentShown(ComponentEvent e) { }
	public void componentResized(ComponentEvent e) { }
	public void mouseClicked(MouseEvent e) { }
	public void mouseEntered(MouseEvent e) { }	
	public void mouseExited(MouseEvent e) { }	
	public void mousePressed(MouseEvent e) { 
	    if (layout == null || ctxt == null) return;
	    zoom1 = getXY(e);
	}	
	public void mouseReleased(MouseEvent e) { 
	    if (layout == null || ctxt == null)
		return;
	    double[] zoom2 = getXY(e);
	    if (zoom1 == null || zoom2 == null) return;
	    ctxt.updateAxisRanges(layout, 
		zoom1[0], zoom1[1], zoom2[0], zoom2[1]);
	}

	// translate
	public double[] getXY(MouseEvent e) {
	    if (layout.style != GraphLayout.STYLE_2DLINE)	
		return null;
	    return graphXY.getXY(e.getX(), e.getY());
	}

	//////////  methods for interface GraphRender

	// return Swing component
	public JComponent jcomp() { return this; }

	// load new layout
	public void setGraphLayout(GraphLayout l, GraphData.Subset[] s) 
	throws Exception {
	    subsets = (layout==l) ? s : null;
	    layout = l;
	    repaint();
	}

	// change all axis bounds
	public void changeAxisBounds(double[] xbounds,
	double[] ybounds,  double[] zbounds) {
	    if (layout == null) return;
	    changeAxisBounds(xbounds, layout.xaxis);
	    changeAxisBounds(ybounds, layout.yaxis);
	    changeAxisBounds(zbounds, layout.zaxis);
	    repaint();
	}

	// change one axis bounds
	private void changeAxisBounds(double[] b, 
	GraphLayout.Axis gaxis) {
	    if (b == null || gaxis == null) return;
	    gaxis.min = b[0];
	    gaxis.max = b[1];
	}

	// printng
	public int print(Graphics g, PageFormat pf, int inx)
	throws PrinterException {
	    throw new PrinterException(
		"DemoGraph printing not implemented");
	}

    	public int printComposite(Graphics g, PageFormat pf, int inx,
        int idx, int nrow, int ncol) throws PrinterException {
	    throw new PrinterException(
		"DemoGraph printing not implemented");
	}

	// export Encapsulated Post-Script
	public void exportEPS(File f) throws Exception {
	    throw new Exception(
		"DemoGraph.exportEPS() not implemented");
	}

}
