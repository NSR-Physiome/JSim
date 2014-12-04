/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// graphing Attribute for XY line plot
 
package JSim.gui.gsgraph;
 
import java.awt.*;
import javax.swing.*;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSLineAttr extends LineAttribute {

	// constructor
	public GSLineAttr(GSGeom geom, GraphData gdata) {
	    super();
	    doStyle(gdata);
	    doStroke(this, gdata);
	    doDashArray(this, gdata);
	    doMark(geom, gdata.shape, gdata.size);
	    setColor(gdata.color);
	}
	    
	// set line style
	//   Note SGT does not support .line or .thickness options via MARK_LINE
	private void doStyle(GraphData gdata) {
	    int style;
	    if (gdata.shape == gdata.SHAPE_NONE) 
		style = STROKE;
	    else if (gdata.line == gdata.LINE_NONE)
		style = MARK;
	    else
		style = MARK_LINE;
	    setStyle(style);
	}

	// set line stroke
	protected static void doStroke(LineAttribute attr, GraphData gdata) {
	    float lw = 1;
            switch (gdata.thickness) {
            case GraphData.LINE_MEDIUM: lw = 2; break;
            case GraphData.LINE_THICK: lw = 3; break;
            }
	    attr.setWidth(lw);
            attr.setMiterLimit(1.0f);
            attr.setCapStyle(LineAttribute.CAP_BUTT);
	}

	// set dash array
	protected static void doDashArray(LineAttribute attr, GraphData gdata) {
	    // dash parms
            float is = 6; // interstitial space 1
            float sd = 6; // short dash length
            float ld = 12; // long dash length
            float dt = 2; // dot length

	    // dash array switch
	    float[] darr = null;
            switch (gdata.line) {
            case GraphData.LINE_SOLID: 
		darr = new float[] { 1 }; 
		break;
            case GraphData.LINE_SHORTDASH: 
		darr = new float[] { sd, is }; 
		break;
            case GraphData.LINE_LONGDASH: 
		darr = new float[] { ld, is }; 
		break;
            case GraphData.LINE_DOT: 
		darr = new float[] { dt, is }; 
		break;
            case GraphData.LINE_DOTDASH: 
		darr = new float[] { dt, is, sd, is }; 
		break;
            case GraphData.LINE_DOTDOTDASH: 
		darr = new float[] { dt, is, dt, is, sd, is }; 
		break;
            case GraphData.LINE_DOTDOTDOTDASH: 
		darr = new float[] { dt, is, dt, is, dt, is, sd, is }; 
		break;
            case GraphData.LINE_SDASHLDASH: 
		darr = new float[] { sd, is, ld, is };
		break;
            case GraphData.LINE_DOTSDASHLDASH: 
		darr = new float[] { dt, is, sd, is, ld, is }; 
		break;
            case GraphData.LINE_DOTSDASHDOTLDASH: 
		darr = new float[] { dt, is, sd, is, dt, is, ld, is }; 
		break;
            }
	    if (darr != null) attr.setDashArray(darr);
	}

	// set mark and mark height
	private void doMark(GSGeom geom, int shape, int size) {
            double fpxsincr = 4.0/3.0/geom.PIXPU;
            double Psize = 2*size*fpxsincr;
	    int mark = 0;
	    double markh = 0;
            switch (shape) { 
            case GraphData.SHAPE_TRIANGLE: 
                mark = 10;
                markh = Psize;
                break;
            case GraphData.SHAPE_CIRCLE: 
                mark = 50;
                markh = Psize + 2*fpxsincr;
                break;
            case GraphData.SHAPE_STAR: 
                mark = 44;
                markh = Psize;
                break;
            case GraphData.SHAPE_SQUARE: 
                mark = 9;
                markh = Psize;
                break;
            case GraphData.SHAPE_DIAMOND: 
                mark = 15;
                markh = Psize + 3*fpxsincr;
                break;
            case GraphData.SHAPE_ASTERISK:
                mark = 11;
                markh = Psize;
                break;
           }
	   if (mark != 0) {
	   	setMark(mark);
	   	setMarkHeightP(markh);
	   }
	}
}
 
  
