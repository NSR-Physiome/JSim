/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// base layer (legend, axes, title, footer) for GSGraph
 
package JSim.gui.gsgraph;
 
import java.awt.*;
import javax.swing.*;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSBaseLayer extends Layer {
	private GSGraph ggraph;
	private GSGeom geom;
	private CartesianGraph cgraph;
	private PlainAxis xaxis, yaxis;
	private LineKey legend;
	private SGLabel[] legendLabels;

	// constructor
	public GSBaseLayer(GSGraph g) {
	    super("BaseLayer", g.geom().layerDimP);
	    ggraph = g;
	    geom = ggraph.geom();
	    cgraph = new CartesianGraph();
	    setGraph(cgraph);

	    // error message
	    if (geom.errorPos != null) {
	    	SGLabel errMsg = new SGLabel(
		    "errMsg",  geom.errorText, geom.errorPos);
	    	errMsg.setHeightP(geom.fontHP);
	    	errMsg.setFont(geom.font);
		errMsg.setHAlign(SGLabel.LEFT);
		errMsg.setVAlign(SGLabel.BOTTOM);
	    	addChild(errMsg);
		return;  // no axes if error message
	    }	

	    // title
	    if (geom.titlePos != null) {	    
	    	SGLabel title = new SGLabel(
		    "title",  geom.titleText, geom.titlePos);
	    	title.setHeightP(geom.tfontHP);
	    	title.setFont(geom.font);
		title.setHAlign(SGLabel.CENTER);
		title.setVAlign(SGLabel.TOP);
		title.addPropertyChangeListener(ggraph.events());
	    	addChild(title);
	    }
	
	    // footer
	    if (geom.footerPos != null) {	    
	    	SGLabel footer = new SGLabel(
		    "footer",  geom.footerText, geom.footerPos);
	    	footer.setHeightP(geom.ffontHP);
	    	footer.setFont(geom.font);
		footer.setHAlign(SGLabel.LEFT);
		footer.setVAlign(SGLabel.BOTTOM);
		footer.addPropertyChangeListener(ggraph.events());
	    	addChild(footer);
	    }

	    // legend
	    legend = null;
	    if (geom.legendPos != null) makeLegend();

	    // axes
	    xaxis = makeAxis("X axis", geom.xaxis);
	    cgraph.addXAxis(xaxis);
	    yaxis = makeAxis("Y axis", geom.yaxis);
	    cgraph.addYAxis(yaxis);

	    // transforms
	    geom.setTransforms(cgraph);
//	    xaxis.setNSRLogScale(geom.xaxis.tics.log());
//	    yaxis.setNSRLogScale(geom.yaxis.tics.log());
	}


	// make legend
	private void makeLegend() {
	    GraphLayout layout = ggraph.graphLayout();
	    legend = new LineKey();
	    legend.setId("legend");
	    legend.setLocationP(geom.legendPos);
	    legend.setVAlign(LineKey.TOP);
	    legend.setHAlign(LineKey.RIGHT);
	    legend.setLayer(this);
	    legend.setLineLengthP(geom.fontHP*2);
	    addChild(legend);

	    // data items
	    legendLabels = new SGLabel[layout.data.length];
	    for (int i=0; i<layout.data.length; i++) {
		GraphData gdata = layout.data[i];
		if (gdata == null) continue;
		SGLabel lab = new SGLabel("line" + i,
		    gdata.label,
		    new Point2D.Double(0.0, 0.0));
		lab.setFont(geom.font);
		lab.setHeightP(geom.fontHP);
	 	if (i==0)
		    lab.addPropertyChangeListener(ggraph.events());
		LineCartesianRenderer r = 
		    (LineCartesianRenderer) ggraph.dataLayer(i).renderer();
		legend.addLineGraph(r, lab);
		legendLabels[i] = lab;
	    }

	}

	// make an axis
	private PlainAxis makeAxis(String id, GSGeom.Axis gaxis) {
	    PlainAxis axis = new PlainAxis(id);
	    axis.setTicArray(
		gaxis.tics.ticLabels(), gaxis.tics.ticValues());	
	    axis.setRangeU(gaxis.rangeU);
	    axis.setLabelFont(geom.font);
	    axis.setLabelHeightP(geom.fontHP);
	    axis.setLargeTicHeightP(geom.ticHP);
	    axis.setLocationU(gaxis.origin);
	    axis.setSelectable(false);

	    // NSR hack for log scale axis label position
	    axis.setNSRLogScale(gaxis.tics.log()); 

	    // axis label
	    if (! Util.isBlank(gaxis.label)) {
	    	SGLabel label = new SGLabel(id + " label", 
		    gaxis.label, new Point2D.Double(0,0));
	    	label.setFont(geom.font);
	    	label.setHeightP(geom.fontHP);
	    	axis.setTitle(label);
	    }

	    return axis;
    	}

	// query
	protected LineKey legend() { return legend; }
	protected SGLabel[] legendLabels() { return legendLabels; }
	protected PlainAxis xaxis() { return xaxis; }
	protected PlainAxis yaxis() { return yaxis; }
}



