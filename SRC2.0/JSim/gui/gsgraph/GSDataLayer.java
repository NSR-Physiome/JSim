/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// Layer for one data curve or coutour set
 
package JSim.gui.gsgraph;
 
import java.awt.*;
import javax.swing.*;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSDataLayer extends Layer {
	private static int layerCt = 0;
	private GSGeom geom; // geometry for rendering
	private CartesianGraph graph; // graph
	private GSLineData lineData; // line data
	private Attribute attr; // data rendering attributes

	// constructor
	public GSDataLayer(GSGeom g, GraphData gdata,
	GraphData.Subset sset) {
	    super("DataLayer_" + layerCt++, g.layerDimP);
	    geom = g;
	    graph = new CartesianGraph();
	    setGraph(graph);
	    geom.setTransforms(graph);

	    // lines/contours
	    switch (geom.style) {
	    case GraphLayout.STYLE_2DLINE:
		lineData = new GSLineData(geom, gdata.label);
		attr = new GSLineAttr(geom, gdata);
		break;
   	    }

	    // put data into graph
	    loadData(gdata, sset);
	}

	// load fresh data
	public void loadData(GraphData gdata,
	GraphData.Subset sset) {

	    // load XY data
	    if (geom.style == GraphLayout.STYLE_2DLINE) {
		lineData.loadData(gdata, sset);
	        graph.setData(lineData, attr);
	    }
	    if (geom.style != GraphLayout.STYLE_CONTOUR) return;
	    
	    // check contour data error conditions
	    if (gdata.x == null) return; // empty GraphData
	    if (geom.xaxis.tics.log() && 
	        gdata.x.length > 0 && gdata.x[0] <= 0) return;
	    if (geom.yaxis.tics.log() && 
	        gdata.y.length > 0 && gdata.y[0] <= 0) return;

	    // load contour data
	    attr = new GSContourAttr(geom, gdata);
	    SimpleGrid cdata = new SimpleGrid(
		gdata.zflip(), gdata.x, gdata.y, gdata.label);
	    graph.setData(cdata, attr);
	}

	// append data subset
	public void appendData(GraphData gdata,
	GraphData.Subset sset) {
	    switch (geom.style) {
	    case GraphLayout.STYLE_2DLINE:
		lineData.appendData(gdata, sset);
	        graph.setData(lineData, attr);
		break;
	    case GraphLayout.STYLE_CONTOUR:
		// so live update, for now
		break;
	    }
	}

	// query
	public CartesianRenderer renderer() {
	    return graph.getRenderer();
	}
}





