/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XY plot renderer for DemoGraph

package JSim.gui.demograph;

import java.awt.*;
import JSim.util.*;
import JSim.gui.graph.*;

public class DemoGraphXY {
	private DemoGraph graph;
	private int x1, x2, y1, y2; // bounds for data drawing

	// constructor
	public DemoGraphXY(DemoGraph t) {
	    graph = t;
	}

	// draw XY plot
	public void paint(Graphics g, int w, int h) {

	    // common initialization
	    GraphData.Subset[] subsets = graph.getClearSubsets();
	    int sp = 4;
	    x1 = w/10;
	    x2 = w-x1;
	    y1 = h/10;
	    y2 = h-y1;
	    int x3 = w/2;
	    int y3 = y1;
	    int hline = (int) (layout().fontSize * 1.5);
	    int ct = (layout().data == null) ? 0 : layout().data.length;

	    // background graphics
	    if (subsets == null) {

		// clear background
	    	g.setColor(Color.white);
	    	g.fillRect(0,0,w,h);

		// draw border
		g.setColor(Color.blue);
	    	g.drawRect(sp,sp,w-1-sp-sp,h-1-sp-sp);
	    	g.setColor(Color.black);

		// draw axes
	    	g.drawLine(x1,y1,x1,y2);
	    	g.drawLine(x1,y2,x2,y2);

		// title, error message
	    	Font font = g.getFont();
	    	font = font.deriveFont((float) layout().fontSize);
	   	g.setFont(font);
	    	if (layout().title != null) {
	    	    g.drawString(layout().title, x3, y3);
		}
	    	if (layout().errorMsg != null) {
		    int y4 = h/2;
		    g.drawString(layout().errorMsg, x1, y4);
	    	}

		// legend
	    	for (int i=0; i<ct; i++) {
		    GraphData d = layout().data[i];
		    if (d.x == null) continue;
		    g.setColor(d.color);
	            y3 += hline;
		    g.drawString(d.label + " #pts=" + d.y.length, 
		    	x3, y3);
		}
	    }

	    // figure extrema
	    if (layout().xaxis == null || 
		layout().yaxis == null) return;
	    double xmin = layout().xaxis.min;
	    double xmax = layout().xaxis.max;
	    double ymin = layout().yaxis.min;
	    double ymax = layout().yaxis.max;

	    // extrema message
	    if (subsets == null) {
		g.setColor(Color.black);
	    	String emsg = "X(" + pretty(xmin) + " " + pretty(xmax) +
		    ")  Y(" + pretty(ymin) + " " + pretty(ymax) + ")";
	    	g.drawString(emsg, x1, y2+hline);
	    }

	    // valid extrema?
	    if (Double.isNaN(xmin)) return;
	    if (Double.isNaN(xmax)) return;
	    if (Double.isNaN(ymin)) return;
	    if (Double.isNaN(ymax)) return;
	    if (xmin == xmax) return;
	    if (ymin == ymax) return;

	    // data points
	    for (int i=0; i<ct; i++) {
		GraphData d = layout().data[i];
		if (d.x == null) continue; // no data
		g.setColor(d.color);
		int hmark = d.size/2;

		// all or subset of data points
		int jmin = 0;
		int jmax = d.x.length;
		if (d.subset != null) {
		    jmin = d.subset.lox;
		    jmax = d.subset.hix;
		}
		if (subsets != null && subsets[i] != null) {
		    jmin = subsets[i].lox;
		    jmax = subsets[i].hix;
		}
		for (int j=jmin; j<jmax; j++) {
		    double x = d.x[j];
		    double y = d.y[j];
		    if (Double.isNaN(x)) continue;
		    if (Double.isNaN(y)) continue;
		    int xc = x1 + (int) ((x2-x1)*(x-xmin+0.0)/(xmax-xmin));
		    int yc = y2 + (int) ((y1-y2)*(y-ymin+0.0)/(ymax-ymin));
		    g.drawRect(xc-hmark, yc-hmark, hmark*2, hmark*2);
		}
	    }

	}

	// pretty number
	private String pretty(double d) {
	    String s = Util.pretty(d);
	    return s;
	}

	// query
	public GraphLayout layout() { 
	    return graph.layout;
	}
	// model coords for pixel
	public double[] getXY(int x, int y) {
	    if (layout() == null) return null;
	    if (x1 >= x2 || y2 <= y1) return null;
	    double[] xy = new double[2];
	    xy[0] = layout().xaxis.min + 
		((layout().xaxis.max-layout().xaxis.min)*
		(x-x1))/(x2-x1);
	    xy[1] = layout().yaxis.min + 
		((layout().yaxis.max-layout().yaxis.min)*
		(y-y2))/(y1-y2);
	    return xy;
	}
}

