/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Layout information for new graph

package JSim.gui.graph;

import java.awt.*;

public class GraphLayout {

	// values for style
	public static final int STYLE_2DLINE = 0;
	public static final int STYLE_CONTOUR = 1;
	public static final int STYLE_3DSURFACE = 2;
	public static final int STYLE_COLORMAP = 3; // obsolete, remove!

	// instance variables
	public Color fg;	// foreground color
	public Color bg;	// background color
	public int fontSize;	// base font size 
	public String title;	// graph title
	public double titleX;	//       X position (reqd if title null)
	public double titleY;	//       Y position (reqd if title null)
	public String footer;	// footer text
	public double footerX;	//        X position (reqd if footer null)
	public double footerY;	//        Y position (reqd if footer null)
	public int style;	// style
	public boolean showLegend; // show legend?
	public boolean showTitle; // show title?
	public boolean showFooter; // show footer?
	public double legendX;	//         X position (reqd if showLegend true)
	public double legendY;	//         Y position (reqd if showLegend true)
	public Axis xaxis;	// X axis setup
	public Axis yaxis; 	// Y axis setup
	public Axis zaxis; 	// Z axis setup (if required)
	public GraphData[] data;  // may be null or 0 length
	public String errorMsg; // if no data
	public String[] printLegend; // print-only legend
	public long timeStamp; // graph time-stamp, 0=current
	public boolean xscroll; // show horizonal scrollbar
	public boolean yscroll; // show veritical scrollbar

	// constructor sets some reasonable defaults
	public GraphLayout() {
	    fg = Color.black;
	    bg = Color.white;
	    titleX = 0.5;
	    titleY = 1;
	    footerX = 0;
	    footerY = 0;
	    legendX = 1;
	    legendY = 1;
	    style = STYLE_2DLINE;
	    fontSize = 12;
	}

	// Axis inner class
	public static class Axis {
	    public String label;	// axis label
	    public boolean showTics;    // show tics & numerics?
	    public boolean showLabel;	// show label?
	    public boolean auto; // is autoscaled?
	    public boolean log;	// is log-scale?
	    public double min;	// axis min value
	    public double max;	// axis max value

	    // constructor
	    public Axis() {
	        showTics = true;
		label = "";
		log = false;
		auto = true;
		min = 0;
		max = 1;
	    }
	}

	// GraphRender editable aspects of GraphLayout
	public static class Editables {
	    public String title;	
	    public double titleX;	
	    public double titleY;	
	    public String footer;	
	    public double footerX;	
	    public double footerY;	
	    public double legendX;
	    public double legendY;
	    public String xaxisLabel;
	    public String yaxisLabel;

	    // constructor
	    public Editables(GraphLayout l) {
		title = l.title;
		titleX = l.titleX;
		titleY = l.titleY;
		footer = l.footer;
		footerX = l.footerX;
		footerY = l.footerY;
		legendX = l.legendX;
		legendY = l.legendY;
		if (l.xaxis != null) xaxisLabel = l.xaxis.label;
		if (l.yaxis != null) yaxisLabel = l.yaxis.label;
	    }
	}		
}


