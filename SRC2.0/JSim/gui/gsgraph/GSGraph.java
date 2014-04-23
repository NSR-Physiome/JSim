/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// SGT based GraphRender (prototype)
 
package JSim.gui.gsgraph;
 
import java.io.*;
import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.ArrayList;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSGraph extends JPane implements GraphRender {

	// state
	private GraphContext ctxt; // calling context
	private JPanel jcomp;  // robust container for JPane
	private GraphLayout layout; // current layout
	private GraphData.Subset[] subsets; // current subsets
	private GSGeom geom; // layout geometry
	private GSBaseLayer baseLayer; // legend layer
	private ArrayList<GSDataLayer> dataLayers; // data layers
	private GSEvents events; // event listener
	private GSPrint print; // print handler
	private GSJobQueue jobQueue; // job queue

	// constructor
	public GSGraph(GraphContext c) {
	    super();
	    ctxt = c;
	    setLayout(new StackedLayout());
	    setDoubleBuffered(true);
	    setBatch(true); // explicit draw() calls only
	    setBackground(Color.white);
	    setVisible(false);
	    jcomp = new JPanel(new GridLayout(1,1));
	    jcomp.add(this);
	    geom = new GSGeom(this);
	    events = new GSEvents(this);
	    print = new GSPrint(this);
	    jobQueue = new GSJobQueue();
	}

	// GUI request for new or updated layout
	public void setGraphLayout(GraphLayout l, GraphData.Subset[] s) {
	    if (l != null) setVisible(true);
//	    if (isDrawable())
	        jobQueue.enqueue(new GSJob(this, l, s)); 
	}

	// GUI request to update axis bounds
	public void changeAxisBounds(double[] xbounds,
	double[] ybounds, double[] zbounds) {
	    if (isDrawable())
	    	jobQueue.enqueue(new GSJob(this, xbounds, ybounds, zbounds));
	}

	// graph resized,  adjust physical coords, transforms
	protected void resize() {
	    if (isDrawable())
	        jobQueue.enqueue(new GSJob(this, getSize()));
	}

	// setGraphLayout() work routine
	protected void jobWork(GraphLayout l, GraphData.Subset[] s) {
	    boolean all = 
		l != null ||  l != layout || s == null;
      	    layout = l;
	    subsets = s;
	    if (all) 
		reloadAll(null);
	    else 
		reloadSubsets();
	    doneJob();
	}

	// changeAxisBounds() work routine
	protected void jobWork(double[] xbounds,
	double[] ybounds, double[] zbounds) {
	    if (layout != null) {
	    	updateAxis(layout.xaxis, xbounds);
	    	updateAxis(layout.yaxis, ybounds);
	    	updateAxis(layout.zaxis, zbounds);
	    	reloadAll(null);
	    }
	    doneJob();
	}

	// resize work routine
	protected void jobWork(Dimension dim) {
	    reloadAll(dim);
	    doneJob();
	}

	// done current job
	private void doneJob() {

	    // update onscreen buffer
	    if (isShowing() && isDrawable()) {
		try {
	    	    validate(); // prevents flicker 
	    	    draw();
		} catch (Exception e) {
		    // NegArray known bug in SGT Contour plots
		    if (! (e instanceof NegativeArraySizeException))
		        System.err.println(e.toString());
		}
	    }

	    jobQueue.startNext();
	}

	// drawable
	private boolean isDrawable() {
	    if (! isVisible()) return false;
	    Dimension dim = getSize();
	    return dim.width > 0 && dim.height > 0;
	}

	// update one axis
	private void updateAxis(GraphLayout.Axis gaxis,
	double[] bounds) {
	    if (gaxis == null || bounds == null) return;
	    gaxis.min = bounds[0];
	    gaxis.max = bounds[1];
	}
	    
	// reload entire page
	private void reloadAll(Dimension dim) {
	    try {

	    // clear old state,  reset geometry
	    setOpaque(true);
	    removeAll();
	    setBackground(Color.white);
	    baseLayer = null;
	    dataLayers = new ArrayList<GSDataLayer>();
	    geom.recalc(dim);

	    // add data layers first
	    if (layout != null && layout.data != null) {
	    	for (int i=0; i<layout.data.length; i++) {
		    GraphData.Subset sset = null;
		    if (subsets != null && i < subsets.length)
			sset = subsets[i]; 
		    GSDataLayer layer = new GSDataLayer(
			geom, layout.data[i], sset);
		    dataLayers.add(layer);
		    add(layer);
	    	}
	    }

	    // add base layer last improves performance
	    baseLayer = new GSBaseLayer(this);
	    add(baseLayer);

	    } catch (Exception e) {
	    	e.printStackTrace();
	    }
	}    

	// reload data subsets
	private void reloadSubsets() {
	    // return if nonsense
	    if (layout == null || layout.data == null) return;
	    if (subsets == null) return;
	    if (layout.data.length != subsets.length
	    || dataLayers.size() != subsets.length) {
		System.err.println("mismatched layout, data and/or subsets");
		return;
	    }

	    // load data for each
	    for (int i=0; i<subsets.length; i++) {
		GSDataLayer layer = (GSDataLayer) dataLayers.get(i);
		dataLayer(i).appendData(layout.data[i], subsets[i]);
	    }
	}

	// simple query
	protected GraphContext ctxt() { return ctxt; }
	public JComponent jcomp() { return jcomp; }
	public GraphLayout graphLayout() { return layout; }
	public GSGeom geom() { return geom; }
	public GSBaseLayer baseLayer() { return baseLayer; }
	public GSDataLayer dataLayer(int i) {
	     return (GSDataLayer) dataLayers.get(i);
	}
	public GSEvents events() { return events; }

	// print component
	public void print(Graphics g) {
	    Graphics2D g2 = (Graphics2D) g;
	    draw(g2);
	}

	// print composite: this is an OLD HACK !!!
	//   s/b replaced by print(Graphics) above when have time
	public int printComposite(Graphics g, PageFormat pf, 
	int inx, int idx, int nrow, int ncol)
        throws PrinterException {
	    return print.printComposite(g, pf, inx, idx, nrow, ncol);
	}

	// export Encapsulated Post-Script
	public void exportEPS(File f) throws Exception {
	    print.exportEPS(f);
	}

	// process mouse events
	public void processMouseEvent(MouseEvent e) {
	    super.processMouseEvent(e);
	    events.processMouseEvent(e);
	}
	public void processMouseMotionEvent(MouseEvent e) {
	    super.processMouseMotionEvent(e);
	    events.processMouseMotionEvent(e);
	}

	// short exception messages in paint
	public void paint(Graphics g) {
	    try {
		super.paint(g);
	    } catch (Exception e) {
		// NegArray known bug in SGT Contour plots
		if (! (e instanceof NegativeArraySizeException))
		    System.err.println(e.toString());
	    }
	}
}

