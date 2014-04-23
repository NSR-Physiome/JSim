/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// one render/resize/update request 

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

public class GSJob extends Thread {
	private static int jobCt = 0;
	private GSGraph graph;
	private int code;
	private GraphLayout layout;
	private GraphData.Subset[] subsets;
	private double[] xbounds, ybounds, zbounds;
	private Dimension dim;

	// job codes
	private static final int SET_GRAPH_LAYOUT = 1;
	private static final int CHANGE_AXIS_BOUNDS = 2;
	private static final int RESIZE = 3;
	public static final int NCODES = 3;

	// setGraphLayout() constructor
	public GSJob(GSGraph g, GraphLayout l, GraphData.Subset[] s) {
	    super("GSJob_" + jobCt++);
	    code = SET_GRAPH_LAYOUT;
	    graph = g;
	    layout = l;
	    subsets = s;
	}

	// changeAxisBounds() constructor
	public GSJob(GSGraph g, double[] xb, double[] yb, double[] zb) {
	    super("GSJob_" + jobCt++);
	    code = CHANGE_AXIS_BOUNDS;
	    graph = g;
	    xbounds = xb;
	    ybounds = yb;
	    zbounds = zb;
	}

	// resize constructor
	public GSJob(GSGraph g, Dimension d) {
	    super("GSJob_" + jobCt++);
	    code = RESIZE;
	    graph = g;
	    dim = d;
	}

	// run the job
	public void run() {
	    try {
		runX();
	    } catch (Exception e) {
		System.err.println(e.toString());
	    }
	}

	// real work may generate undeclared exceptions
	public void runX() {
	    switch (code) {
	    case SET_GRAPH_LAYOUT:
	    	graph.jobWork(layout, subsets);
		break;
	    case CHANGE_AXIS_BOUNDS:
		graph.jobWork(xbounds, ybounds, zbounds);
		break;
	    case RESIZE:
		graph.jobWork(dim);
		break;
	    }
	}

	// query
	public int code() { return code; }
	public String toString() {
	    String s = getName().substring(6);
	    switch (code) {
	    case SET_GRAPH_LAYOUT: 
		return s + "_layout";
	    case CHANGE_AXIS_BOUNDS:
		return s + "_axes";
	    case RESIZE:
		return s + "_size_" + 
		    dim.width + "x" + dim.height;
	    default:
		return getName() + "_???";
	    }
	}
}



