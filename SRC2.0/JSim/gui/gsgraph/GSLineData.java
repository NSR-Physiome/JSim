/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// Line data
 
package JSim.gui.gsgraph;
 
import java.awt.*;
import javax.swing.*;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSLineData extends SimpleLine {
	static private final double[] range01 = new double[] { 0, 1 };
	private GSGeom geom; // geometry for rendering
	private double[] xdata, ydata; // data to draw
	private GraphData.Subset subset; // current subset
	
	// constructor
	public GSLineData(GSGeom g, String label) {
	    super(range01, range01, label);
	    setTitle(label);
	    geom = g;
	}

	// load fresh data
	public void loadData(GraphData gdata,
	GraphData.Subset sset) {
	    // replace storage
	    xdata = gdata.x;
	    ydata = gdata.y;
	    subset = sset;
	    if (xdata == null || ydata == null)
		xdata = ydata = new double[0];
            if (xdata.length != ydata.length) {
		xdata = ydata = new double[0];
		System.err.println("GSLineData.loadData(" + 
		    gdata.label + "): mismatched X/Y data arrays");
	    }

	    // constant data spans entire axis
	    if (gdata.isConstant && xdata.length == 2) {
		xdata[0] = geom.xaxis.tics.axismin();
		xdata[1] = geom.xaxis.tics.axismax();
	    }

	    // clip all data
	    int lox = 0;
	    int hix = (subset == null) ?
		xdata.length-1 : sset.hix;
	    if (hix >= xdata.length) hix = xdata.length-1;
	    clipData(lox, hix);

	    // store
	    setXArray(xdata);
	    setYArray(ydata);
	}

	// append data subset
	public void appendData(GraphData gdata,
	GraphData.Subset sset) {

	    // if no subset,  complete load
	    if (sset == null || subset == null) {
		loadData(gdata, sset);
		return;
	    }
	    if (gdata.x == null || gdata.y == null) return;

	    // rollover xy-data / subsets
	    double[] oldxdata = xdata;
	    double[] oldydata = ydata;
	    GraphData.Subset oldsset = subset;
	    xdata = gdata.x;
	    ydata = gdata.y;
	    subset = sset;

	    // clip new data
	    int lox = subset.lox;
	    int hix = subset.hix;
	    if (oldsset != null) 
		lox = oldsset.hix+1;
	    clipData(lox, hix);

	    // prepend old data
	    if (xdata.length == 0) {
		xdata = oldxdata;
		ydata = oldydata;
	    } else {
		double[] newxdata = xdata;
		double[] newydata = ydata;
		int oct = oldxdata.length;
		int nct = newxdata.length;
		xdata = new double[oct+nct];
		ydata = new double[oct+nct];
		for (int i=0; i<oct; i++) {
		    xdata[i] = oldxdata[i];
		    ydata[i] = oldydata[i];
		}
		for (int i=0; i<nct; i++) {
		    xdata[i + oct] = newxdata[i];
		    ydata[i + oct] = newydata[i];
		}
	    }

	    // store
	    setXArray(xdata);
	    setYArray(ydata);
	}
		

	// clip xdata/ydata, given starting ranges
	private void clipData(int lox, int hix) {

	    // local field access for speed
	    double xmin = geom.xaxis.clipRangeU.start;
	    double xmax = geom.xaxis.clipRangeU.end;
	    double xdelta = geom.xaxis.clipDeltaU;
	    double ymin = geom.yaxis.clipRangeU.start;
	    double ymax = geom.yaxis.clipRangeU.end;
	    double ydelta = geom.yaxis.clipDeltaU;

	    // clip lo/hi index at ends
	    while (lox <= hix) {
		double x = xdata[lox];
		double y = ydata[lox];
		if (y>ymin && y<ymax && x>xmin && x<xmax)
		    break;
		lox++;
	    }
	    while (lox <= hix) {
		double x = xdata[hix];
		double y = ydata[hix];
		if (y>ymin && y<ymax && x>xmin && x<xmax)
		    break;
		hix--;
	    }

	    // add one off-axis point on each end for continuation lines
	    if (lox>0 
	    && !Double.isNaN(xdata[lox-1]) 
	    && !Double.isNaN(ydata[lox-1]))
		lox--;
	    if (hix<xdata.length-1 
	    && !Double.isNaN(xdata[hix+1]) 
	    && !Double.isNaN(ydata[hix+1]))
		hix++;

	    // copy reduced data to xclip, yclip
	    int ct = hix-lox+1;
	    double[] xclip = new double[ct];
	    double[] yclip = new double[ct];
	    double xlast = Double.NaN;
	    double ylast = Double.NaN;
	    boolean xlog = geom.xaxis.tics.log();
	    boolean ylog = geom.yaxis.tics.log();
	    int nct = 0;
	    for (int i=0; i<ct; i++) {
		double x = xdata[lox+i];
		double y = ydata[lox+i];

		// bullet-proof problems not handled by SGT
		//  -Infinity causes semi-infinite loop in JPane.paint()
		if (xlog && x<=0) x = Double.NaN;
		if (ylog && y<=0) y = Double.NaN;
		if (Double.isInfinite(x)) x = Double.NaN; 
		if (Double.isInfinite(y)) y = Double.NaN;

		double dx = x - xlast;
		if (dx < 0) dx = -dx; // faster than Math.abs()?
		double dy = y - ylast;
		if (dy < 0) dy = -dy;
		if (dx < xdelta && dy < ydelta) continue;
		xclip[nct] = x;
		yclip[nct] = y;
		nct++;
		xlast = x;
		ylast = y;
	    }

	    // reduce for deltas
	    if (nct < ct || xlog || ylog) {
		double[] xclip2 = new double[nct];
		double[] yclip2 = new double[nct];
		for (int i=0; i<nct; i++) {
		    xclip2[i] = xclip[i];
		    yclip2[i] = yclip[i];
		}
		xclip = xclip2;
		yclip = yclip2;
	    }

	    // roll in new arrays
	    xdata = xclip;
	    ydata = yclip;
	}

}


