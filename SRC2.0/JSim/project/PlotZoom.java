/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Plot zoom rectangles and a stack of them

package JSim.project;

import java.util.ArrayList;
import JSim.util.*;

public class PlotZoom {
	public boolean xauto;
	public boolean yauto;
	public boolean zauto;
	public double xmin, xmax;
	public double ymin, ymax;
	public double zmin, zmax;

	// constructors
	public PlotZoom(Plot plot) {
	    update(plot);
	}
	public PlotZoom(PlotZoom zoom) {
	    xauto = zoom.xauto;
	    xmin = zoom.xmin;
	    xmax = zoom.xmax;
	    yauto = zoom.yauto;
	    ymin = zoom.ymin;
	    ymax = zoom.ymax;
	    zauto = zoom.zauto;
	    zmin = zoom.zmin;
	    zmax = zoom.zmax;
	}

	// update current state
	public void update(Plot plot) {
	    xauto = plot.xaxis.autoscale.val();
	    xmin = plot.xaxis.min.val();
	    xmax = plot.xaxis.max.val();
	    yauto = plot.yaxis.autoscale.val();
	    ymin = plot.yaxis.min.val();
	    ymax = plot.yaxis.max.val();
	    zauto = plot.zaxis.autoscale.val();
	    zmin = plot.zaxis.min.val();
	    zmax = plot.zaxis.max.val();
	}

	// zoom in update 
	public void update(double x1, double x2,
	double y1, double y2) throws Xcept {
	    if (x1==x2 || y1==y2) throw new Xcept(
		"Invalid zoom rectangle (zero area)");

	    // safety bounds
	    if (x1 > x2) {
		double xx = x1;
		x1 = x2;
		x2 = xx;
	    }
	    if (y1 > y2) {
		double yy = y1;
		y1 = y2;
		y2 = yy;
	    }

	    // update
	    //   fancy preserving of [xy]auto fails because
	    //   min,max don't represent real data bounds
	    //   maybe address later
	    xauto=false;
	    xmin = x1;
	    xmax = x2;
	    yauto=false;
	    ymin = y1;
	    ymax = y2;
	}

	// apply zoom to plot
	public void apply(Plot plot) throws Xcept {
	    if (!xauto && plot.xaxis.autoscale.val())
		plot.confAxis.setVal("X");
	    plot.xaxis.autoscale.setVal(xauto);
	    plot.xaxis.min.setVal(xmin);
	    plot.xaxis.max.setVal(xmax);
	    if (!yauto && plot.yaxis.autoscale.val())
		plot.confAxis.setVal("Y");
	    plot.yaxis.autoscale.setVal(yauto);
	    plot.yaxis.min.setVal(ymin);
	    plot.yaxis.max.setVal(ymax);
	    plot.zaxis.autoscale.setVal(zauto);
	    plot.zaxis.min.setVal(zmin);
	    plot.zaxis.max.setVal(zmax);
	}

	// Zoom stack
	public static class Stack extends ArrayList<PlotZoom> {
	    private Plot plot; // for this plot
	    private int ptr; // current item on stack

	    // constructor
	    public Stack(Plot p) {
		super();
		plot = p;
		add(new PlotZoom(plot));
		ptr = 0;
	    }

	    // zoom in to specified rectange
	    public void zoomIn(double x1, double x2,
	    double y1, double y2) throws Xcept {
		curr().update(plot);
		PlotZoom z = new PlotZoom(curr());
		z.update(x1, x2, y1, y2);
		while (ptr >= size()) remove(ptr+1);
		add(z);
		ptr++;
		z.apply(plot);
	    }

	    // zoom in farther,  if possible
	    public void zoomIn() throws Xcept {
		if (! canZoomIn()) throw new Xcept(plot, 
		    "Plot is already zoomed in all the way");
		ptr++;
		curr().apply(plot);
	    }

	    // zoom out,  if possible
	    public void zoomOut() throws Xcept {
		if (ptr == 0) 
		    zoomAuto();
		else {
		    ptr--;
		    curr().apply(plot);
		}
	    }

	    // zoom auto
	    public void zoomAuto() throws Xcept {
		ptr = 0;
		curr().xauto = true;
		curr().yauto = true;
		curr().zauto = true;
		curr().apply(plot);
	    }

	    // simply query
	    public PlotZoom curr() {
		return (PlotZoom) get(ptr);
	    }
	    public boolean canZoomIn() { 
		return ptr < size()-1;
	    }
	    public boolean canZoomOut() {
		return ptr>0;
	    }
	    public boolean canZoomAuto() {
		if (!plot.xaxis.autoscale.val()) return true;
		if (!plot.yaxis.autoscale.val()) return true;
		if (!plot.zaxis.autoscale.val()) return true;
		return false;
	    }
	}
}

