/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1 plot's context

package JSim.gui;

import java.util.Date;
import java.awt.Color;
import java.util.ArrayList;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.graph.*;

public class GPlotItem {
	private GPlot gplot;	// for this GPlot
	private PlotItem item;	// project item
	private int itemInx;	// item index (for legend)
	private int storeInx;	// query store for this item
	private PlotData plotData; // data to plot
	private Delta delta; // last draw graph delta
	private boolean isComplete; // data completely loaded
	
	// constructor loads all data
	protected GPlotItem(GPlot gp, PlotItem itm, int i, int j) {
	    gplot = gp;
	    item = itm;
	    itemInx = i;
	    storeInx = j;
	    try {
	    	plotData = item.getPlotData(storeInx);
	    } catch (Xcept e) {
	    	graph().gproject().message(e);
	    }
	    isComplete = !item.isLive(); 
 	}
	
	// refresh live data, return rendering subset
	protected GraphData.Subset reloadLive() throws Xcept {
	    if (isComplete) return null; // or zero-size subset???
	    plotData = item.getPlotData(storeInx);	
	    delta = new Delta(delta, plotData);
	    isComplete = delta.isComplete();
	    return delta.subset();
	}
	
	// make GraphData shell with rendering characteristics
	protected GraphData makeGraphData() {
	    GraphData gd = new GraphData();
	    gd.color = graph().glook().plotColor(item.color.val());
	    gd.shape = (storeInx == 0) ? 
		item.shape.val() : GraphData.SHAPE_NONE;
	    gd.size = graph().plotSize(item.size.val());
	    gd.thickness = item.thickness.val();
	    gd.colorMap = item.colorMap.val();
	    gd.palette = item.palette.val();
	    gd.line = item.line.val() + storeInx;
	    int max = GraphData.LINE_DOTSDASHDOTLDASH;
	    if (gd.line > max) 
		gd.line = 1 + (gd.line+max-1) % max;
	    gd.label = "" + (itemInx+1) + ": " + 
	    	item.expr.label(storeInx);
	    PNamed p = item.expr.base();
	    if (p instanceof PModel) 
		gd.help = "model" + GHelp.sep + 
		    p.name() + GHelp.sep + item.expr.stringVal();
	    return gd;
	}
	
	// update GraphData with latest
	protected void updateGraphData(GraphData gd) throws Xcept {
	    if (plotData == null) return;
	    int ndim = plotData.ndim();
	    int naxes = gplot.naxes();
	    double[] samp0 = plotData.data(0).samples();

	    // special case for 0D data
	    if (ndim == 0) {
		gd.isConstant = true;
		if (naxes == 2) 
		    gd.y = new double[] { samp0[0], samp0[0] };
		else
		    gd.z = new double[] { 
		    	samp0[0], samp0[0], samp0[0], samp0[0]};
		return;
	    }

	    // dimensional agreement
	    if (ndim+1 == naxes) {
	        gd.isConstant = false;
	    	gd.x = samp0;
	    	gd.y = plotData.data(1).samples();
	    	if (naxes > 2) 
		    gd.z = plotData.data(2).samples();
		return;
	    }

	    // dimensional conflicts issue warning/suggestion
	    String msg = "Plot style supports 0D or " +
		(naxes-1) + "D data,  but \"" + 
		plotData.desc() + "\" is " + ndim + "D.";
	    int ct = ndim - (naxes-1);
	    if (ct>1)
	        msg = msg + " Set " + ct + 
		    " variable domain(s) to constant value.";
	    graph().warning(msg);
	}	    

	// public query
	public GPlotGraph graph() { return gplot.graph(); }
	public PlotData plotData() { return plotData; }
	public long timeStamp() { return item.timeStamp(); }
	public String toString() { 
	    return item.expr.stringVal() + "#" + storeInx; 
	}

	// print legend for associated PModel or PDataSet
	public String[] makePrintLegend() {
	    PNamed p = item.expr.base();

	    // PModel legend
	    if (p instanceof PModel) {
	        PModel pmodel = (PModel) p;
	    	String[] s = new String[2];
		s[0] = "Model " + pmodel.name();
		long t = pmodel.timeStamp();
		if (t>0) s[0] = s[0] + " run " + new Date(t);
 	 	String pset = pmodel.parSetName.val();
		if (Util.isBlank(pset)) pset = "<default>";
		s[1] = "       using parameter set " + pset;
		return s;
	    }
	    
	    // PDataSet legend
	    if (p instanceof PDataSet) {
	    	PDataSet pdata = (PDataSet) p;
		String[] s = new String[1];
		s[0] = "Data set " + pdata.name();
		if (! pdata.origFile.isBlank())
		    s[0] = s[0] + " (" + pdata.origFile.val() + ")";
		return s;
	    }
	    
	    return null;
	}  	

	// graph delta 
	//   ONLY WORKS FOR XY PLOTS SO FAR !!!
	public static class Delta {
	    private int gridInx;
	    private int nx; // # points in subset
	    private int ndrawn; // # points drawn 
	    private int[] nloaded; // # points loaded for each axis
	    
	    // constructor
	    public Delta(Delta old, PlotData pd) throws Xcept {
		// initialize
	    	if (old == null) {
		    gridInx = -1;
		    nloaded = new int[2]; 
		} else {
		    gridInx = old.gridInx;
		    nx = old.nx;
		    ndrawn = old.ndrawable();
		    nloaded = old.nloaded;
		}

		// update nloaded[i] for each axis
		if (pd == null) return;
		if (pd.ndim() == 0) return;
		nx = pd.grid(0).ct();
		for (int i=0; i<2; i++) { 
		    Data.Subset ss = pd.data(i).subset;
		    if (ss == null) { 
		        nloaded[i] = nx;
		    } else {
		    	gridInx = ss.gridInx;
			nloaded[i] = ss.hix;
		    }
		}
	    }

	    // delta 
	    public GraphData.Subset subset() throws Xcept {
	        if (gridInx < 0) return null;
		GraphData.Subset ss = new GraphData.Subset();
		ss.lox = ndrawn;
		ss.hix = Math.max(ndrawn, ndrawable());
		ss.y = false; // OK till need live 2D data
		return ss;
	    }

	    // is complete
	    public boolean isComplete() {
	    	return nx > 0 && ndrawable() == nx;
	    }

	    // # points drawable
	    private int ndrawable() {
		return Math.min(nloaded[0], nloaded[1]);
 	    }
	}

	// GPlotItem.List
	public static class List extends ArrayList<GPlotItem> {
	    public List(int n) { super(n); }
	    public GPlotItem gitem(int i) { return (GPlotItem) get(i); }
	}
}
