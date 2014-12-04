/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Layout information for histogram

package JSim.gui.graph;

import java.awt.*;

public class HistLayout {

	// instance variables
	public String title;	// histogram title

	// single-data item for now
	public int nbins;       // # bins to sort into
	public double[] samples; // sample data
	public double sampleMin;  // min sample to chart
	public double sampleMax;  // max sample to chart
	public Color barColor;  // bar color
	public int barSpacing;   // pixels between bars

	// constructor
	public HistLayout() {
	}	    
}



