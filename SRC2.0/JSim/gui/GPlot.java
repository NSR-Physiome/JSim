/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1 plot's context

package JSim.gui;

import java.util.*;
import java.awt.Color;
import java.awt.Rectangle;
import javax.swing.JComponent;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.graph.*;

public class GPlot implements GraphContext {
	private GPlotGraph graph; // parent
	private Plot plot;	// project plot info
	private GraphRender render;  // does graphic work
	private GPlotItem.List gitems;  // graph info per PlotItem
	private GPlotBounds gbounds; // last calc axis bounds, info
	private GraphLayout glayout; // last layout, if live

	// allowed overflow from current bounds
	private static final double FUDGEFRAC = 0.05; 

	//// PUBLIC METHODS for upper levels

	// constructor
	public GPlot(GPlotGraph g, Plot p) {
	    graph = g;
	    plot = p;
	    render = graph.gappl().newGraphRender(this);
	}

	// refresh all data
	public void refreshAll() throws Exception {
	    refreshAll(false);
	}
	private void refreshAll(boolean live) throws Exception {
	    glayout = null; // in case of Exception
	    
	    // reload gitems
	    gitems = new GPlotItem.List(plot.nItem());
	    for (int i=0; i<plot.nItem(); i++) {
		PlotItem item = plot.item(i);
		if (! item.isActive()) continue;
		int ct = item.nQueryStores();
		for (int j=0; j<ct; j++) 
        	    gitems.add(new GPlotItem(this, item, i, j));
	    }

	    // calculate axis bounds
	    gbounds = calcBounds();

	    // render complete layout
	    GraphLayout l = makeLayout();
	    updateLayout(l); 
	    render.setGraphLayout(l, null);
	    if (live) glayout = l;
	}    

	// try to refresh live data only,  but may refresh all
	public void refreshLive() throws Exception {
	    if (naxes() != 2) return; // only XY plots supported for now

	    // if no working layout, refreshAll
	    if (glayout == null) {
	    	refreshAll(true);
		return;
	    }

	    // reload live data into gitems, get subset deltas
	    GraphData.Subset[] subsets = 
	    	new GraphData.Subset[gitems.size()];
	    for (int i=0; i<gitems.size(); i++) 
	    	subsets[i] = gitems.gitem(i).reloadLive();

	    // is axis bounds exceeded, 
	    GPlotBounds newBounds = calcBounds();
	    if (newBounds.exceed(gbounds, FUDGEFRAC)) {
	        refreshAll(true);
		return;
	    }

	    // render subset layout
	    updateLayout(glayout);
	    render.setGraphLayout(glayout, subsets);
	}		

	// refresh axis bounds
	public void refreshAxisBounds() {
	    double[] xb = new double[] {
		plot.xaxis.min.val(), plot.xaxis.max.val() };
	    if (plot.xaxis.autoscale.val()) xb = null;
	    double[] yb = new double[] {
		plot.yaxis.min.val(), plot.yaxis.max.val() };
	    if (plot.yaxis.autoscale.val()) yb = null;
	    double[] zb = new double[] {
		plot.zaxis.min.val(), plot.zaxis.max.val() };
	    if (plot.zaxis.autoscale.val()) zb = null; 
	    try {
	    	render.changeAxisBounds(xb, yb, zb);
	    } catch (Exception e) {
		System.err.println("GPlot.refreshAxisBounds:" + e);
	    }
	}

	// simple query
	public GPlotGraph graph() { return graph; }
	public GraphRender render() { return render; }
	public int naxes() { return plot.naxes(); }
	public double[] dataBounds(int i) {
 	    if (gbounds == null) return null;
	    return gbounds.bounds(i);
	}
	
	//// INTERNAL CONTROL
	
	// calculate current bounds
	private GPlotBounds calcBounds() throws Xcept {
	    GPlotBounds bounds = new GPlotBounds(plot);
	    for (int i=0; i<gitems.size(); i++) 
	    	bounds.expand(gitems.gitem(i).plotData());
	    return bounds;
	}	
	
	// create GraphLayout
	private GraphLayout makeLayout() {
	    GraphLayout l = new GraphLayout();
	    l.fg = Color.black;
	    l.bg = Color.white;
	    l.fontSize = graph.glook().fontSize();
	    l.title = plot.title.val();
	    l.titleX = bound(plot.titleX.val(),.1,.9);
	    l.titleY = bound(plot.titleY.val(),.1,1);
	    l.footer = plot.footer.val();
	    l.footerX = bound(plot.footerX.val(),0,.9);
	    l.footerY = bound(plot.footerY.val(),0,.9);
	    l.legendX = bound(plot.legendX.val(),.1,1);
	    l.legendY = bound(plot.legendY.val(),.1,1);
	    l.style = plot.style.val();
	    l.showTitle = plot.showTitle.val();
	    l.showLegend = plot.showLegend.val();
	    l.showFooter = plot.showFooter.val();
	    l.timeStamp = 0;
	    for (int i=0; i<gitems.size(); i++) {
	        long timeStamp = gitems.gitem(i).timeStamp();
		if (timeStamp > l.timeStamp)
		    l.timeStamp = timeStamp;
	    }
	    l.xaxis = makeAxis(l, plot.xaxis);
	    l.yaxis = makeAxis(l, plot.yaxis);
	    l.zaxis = makeAxis(l, plot.zaxis);
	    l.data = new GraphData[gitems.size()];
	    for (int i=0; i<gitems.size(); i++) 
	    	l.data[i] = gitems.gitem(i).makeGraphData();
	    l.printLegend = makePrintLegend();
	    return l;
	}

	// make one GraphLayout.Axis 
	private GraphLayout.Axis makeAxis(GraphLayout l, PlotAxis paxis) {
	    GraphLayout.Axis a = new GraphLayout.Axis();
	    a.label = paxis.label.val();
	    a.showLabel = paxis.showLabel.val();
	    a.log = paxis.log.val();
	    return a;
	}

	// create print-only legend
	private String[] makePrintLegend() {
	    StringList list = new StringList(4);
	    list.add(graph.gproject().title());
	    for (int i=0; i<gitems.size(); i++) {
		String[] s = gitems.gitem(i).makePrintLegend();
		list.addUniq(s); // inefficient
	    }
	    return list.array();
	}

	// update layout with gbounds and gitems data
	private void updateLayout(GraphLayout l) throws Xcept {
	    gbounds.loadInto(l);
	    for (int i=0; i<gitems.size(); i++) 
	    	gitems.gitem(i).updateGraphData(l.data[i]);
	    
	    // HACK: constant data handling s/b in GraphRender
	    if (l.data == null) return;
	    for (int i=0; i<l.data.length; i++) {
	    	GraphData gd = l.data[i];
		if (!gd.isConstant) continue;
		if (gd.x != null) continue;
		gd.x = new double[] { l.xaxis.min, l.xaxis.max };
		if (naxes() == 2) continue;
		gd.y = new double[] { l.yaxis.min, l.yaxis.max };
	    }
	}    

	//// CALLBACKs for GraphContext interface (lower level)
	
	// GraphRender has edited graph labels 
	public void updateEditables(GraphLayout.Editables e) {
	    try {
		if (Util.isBlank(e.title)) {
		    e.title = Plot.TITLE_DEFAULT;
		    plot.showTitle.setVal(false);
		}
	 	plot.title.setVal(e.title);
	    	plot.titleX.setVal(bound(e.titleX,.1,.9));
	    	plot.titleY.setVal(bound(e.titleY,.1,1));
		if (Util.isBlank(e.footer)) {
		    e.footer = Plot.FOOTER_TIME;
		    plot.showFooter.setVal(false);
		}
	    	plot.footer.setVal(e.footer);
	    	plot.footerX.setVal(bound(e.footerX,0,.9));
	    	plot.footerY.setVal(bound(e.footerY,0,.9));
	    	plot.legendX.setVal(bound(e.legendX,.1,1));
	    	plot.legendY.setVal(bound(e.legendY,.1,1));
		if (Util.isBlank(e.xaxisLabel)) {
		    plot.xaxis.showLabel.setVal(false);
		    plot.xaxis.label.setVal(PlotAxis.LABEL_DEFAULT);
		} else 
		    plot.xaxis.label.setVal(e.xaxisLabel);
		if (Util.isBlank(e.yaxisLabel)) {
		    plot.yaxis.showLabel.setVal(false);
		    plot.yaxis.label.setVal(PlotAxis.LABEL_DEFAULT);
		} else 
		    plot.yaxis.label.setVal(e.yaxisLabel);
	    } catch (Xcept x) {
		System.err.println("updateGraphLayout(): " + x.cleanMessage());
	    }
	}

	// user has updated axis ranges (zoom)
	public void updateAxisRanges(GraphLayout layout,
	double x1, double y1, double x2, double y2) {
	    try {
		plot.zoomStack().zoomIn(x1, x2, y1, y2);
	    } catch (Xcept e) {
		graph.gproject().warning(e.cleanMessage());
	    }
	    graph.gplotpage().refresh();
	}

	// help system hooks
	public void setHelp(JComponent jcomp, String key) {
	    graph.setHelp(jcomp, key); // currently unused
	}
	public boolean helpActive() {
	    return graph.ghelp().helpActive();
	}
	public void showHelp(Rectangle r, String key) {
	    graph.ghelp().showHelp(render.jcomp(), r, key);
	}

	// bound double between lo and hi values
	public double bound(double d, double lo, double hi) {
	    if (d > hi) return hi;
	    if (d < lo) return lo;
	    return d;
	}

	//// GPlot.List class
	public static class List extends ArrayList<GPlot> {
	    public List(int n) { super(n); }
	    public GPlot gplot(int i) { return (GPlot) get(i); }
	}
}
