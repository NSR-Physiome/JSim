/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// axis bounds for a GPlot

package JSim.gui;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.graph.*;

public class GPlotBounds {
	private int naxes; // #axes to render (XY=2, contour=3)
	private boolean[] auto;  // autoscale
	private boolean[] log; // log (for autoscale)
	private double[][] bounds; // [x,y,z][min,max]

	// constructor
	public GPlotBounds(Plot plot) throws Xcept{
	    naxes = plot.naxes();
	    auto = new boolean[3];
	    log = new boolean[3];
	    bounds = new double[3][2];
	    load(plot.xaxis, 0);
	    load(plot.yaxis, 1);
	    load(plot.zaxis, 2);
	}
	
	// load values from PlotAxis
	private void load(PlotAxis axis, int i) throws Xcept {
	    auto[i] = axis.autoscale.val();
	    log[i] = axis.log.val();
	    if (auto[i]) {
	    	bounds[i][0] = Double.NaN;
		bounds[i][1] = Double.NaN;
	    } else {
	    	bounds[i][0] = axis.min.realVal();
		bounds[i][1] = axis.max.realVal();
	    }
	}
	
	// expand axes to accomodate plot data
	public void expand(PlotData plotData)  {
	    if (plotData == null) return;
	    int ndim = plotData.ndim();

	    // special case 0D data
	    if (ndim == 0) {
	    	if (naxes == 2)
		    expand(1, plotData.data(0));
		else
		    expand(2, plotData.data(0));
		return;
	    }
	    
	    // dimensional agreement
	    if (ndim+1 == naxes)
	    	for (int i=0; i<naxes; i++)
		    expand(i, plotData.data(i));
	}
	
	// expand one axis (s/b NaN correct)
	private void expand(int i, Data data) {
	    if (! auto[i]) return;

	    // HACK!!! calcExtrema should be done in model/dataset query
	    // for best efficiency, especially in live update
	    data.calcExtrema(); 

	    double min = log[i] ? data.minpos() : data.min();
	    double max = data.max();
	    if (! (bounds[i][0] <= min)) bounds[i][0] = min;
	    if (! (bounds[i][1] >= max)) bounds[i][1] = max;
	}

	// do these bounds exceed arg's (s/b/ NaN correct)
	public boolean exceed(GPlotBounds pb, double fudgeFrac) {
	    if (hasNaNs()) return false;
	    if (pb.hasNaNs()) return true;
	    for (int i=0; i<naxes; i++) {
		double loLimit, hiLimit;
	        if (log[i]) {
		    loLimit = pb.bounds[i][0] * (1-fudgeFrac);
		    hiLimit = pb.bounds[i][1] * (1+fudgeFrac);
		} else {
	    	    double fudge = 
		        fudgeFrac * (pb.bounds[i][1] - pb.bounds[i][0]);
		    loLimit = pb.bounds[i][0] - fudge;
		    hiLimit = pb.bounds[i][1] + fudge;
		}
	    	if (bounds[i][0] < loLimit) return true;
	    	if (bounds[i][1] > hiLimit) return true;
	    }
	    return false;
	}

	// are there NaNs in current axes
	private boolean hasNaNs() {
	    for (int i=0; i<naxes; i++) 
	        for (int j=0; j<2; j++) 
		    if (Double.isNaN(bounds[i][j])) return true;
	    return false;
	}
	
	// load axes values into GraphLayout
	public void loadInto(GraphLayout layout) {
	    loadInto(layout.xaxis, 0);
	    loadInto(layout.yaxis, 1);
	    loadInto(layout.zaxis, 2);
	}
	
	// load single GraphLayout.Axis
	private void loadInto(GraphLayout.Axis axis, int i) {
	    if (axis == null) return;
	    axis.auto = auto[i];
	    axis.min = bounds[i][0];
	    axis.max = bounds[i][1];

	    // HACK!!! for stupid graph package
	    if (axis.min == axis.max) {
	    	double d = (axis.max == 0) ? 1 : Math.abs(axis.max/2);
		axis.min -= d;
	    	axis.max += d;
	    }
	    if (axis.min<axis.max) return;
	    if (axis.log) {
	    	axis.min = 1;
		axis.max = 10;
	    } else {
	    	axis.min = 0;
		axis.max = 1;
	    }
	}

	// simple query	
	public double xmin() { return bounds[0][0]; }
	public double xmax() { return bounds[0][1]; }
	public double ymin() { return bounds[1][0]; }
	public double ymax() { return bounds[1][1]; }
	public double zmin() { return bounds[2][0]; }
	public double zmax() { return bounds[2][1]; }
	public double[] xbounds() { return bounds[0]; }
	public double[] ybounds() { return bounds[1]; }
	public double[] zbounds() { return bounds[2]; }
	public double[] bounds(int i) { return bounds[i]; }
	public String toString() {
	    String s = "";
	    for (int i=0; i<naxes; i++) 
	    	s = s + "xyz".charAt(i) + "[" + 
		    bounds[i][0] + " " + bounds[i][1] + "] ";
	    return s;
	}
}

	
	
