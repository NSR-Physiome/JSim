/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plot icons and gui controls

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.jcomp.*;
import JSim.gui.graph.*;
import JSim.gui.gsgraph.*;

public class GPlotIconControl extends GMenuControl {

	// attribute codes
	public static final int LINE = 0;
	public static final int THICK = 1;
	public static final int COLOR = 2;
	public static final int SHAPE = 3;
	public static final int SIZE = 4;
	private static int[] ATTR_CTS = new int[] {
	    11, 3, 13, 6, 4 };

	// static icons
	private static Icon[][] icons;   

	// constructor
	public GPlotIconControl(GNode g, Control c, int attr) {
	    super(g, c, attr); 
	}

	// make labels (fake)
	public String[] makeLabels() { return new String[0]; }
	
	// make Icons
	public Icon[] makeIcons() {
	    if (icons == null) makeStaticIcons();	
	    return icons[attr];
	}
	
	// color
	public Color bg(int i) {
	    if (attr == COLOR) return glook().plotColor(i);
	    return Color.white;
	}
	
	// create icons
	public void makeStaticIcons() {
	    int wh = glook().fontSize() * 3 / 2;
	    Color fg = Color.black;
	    Color bg = Color.white;
	    int sz = glyphSize(1 /* normal */);

	    // line styles
	    Icon[] lineIcons = new Icon[ATTR_CTS[LINE]];
	    for (int i=0; i<lineIcons.length; i++) {
		lineIcons[i] = new GLineIcon(
		   4*wh, wh, fg, bg, i, GraphData.LINE_THIN);
 	    }	

	    // line thickness
	    Icon[] thickIcons = new Icon[ATTR_CTS[THICK]];
	    for (int i=0; i<thickIcons.length; i++) {
		thickIcons[i] = new GLineIcon(
		   wh, wh, fg, bg, GraphData.LINE_SOLID, i);
 	    }	

	    // colors
	    Icon[] colorIcons = new Icon[ATTR_CTS[COLOR]];
	    for (int i=0; i<colorIcons.length; i++) {
		fg = bg = glook().plotColor(i);
		colorIcons[i] = new GSymbolIcon(
		   wh, wh, fg, bg, sz, GraphData.SHAPE_NONE);
	    }

	    // shapes
	    fg = Color.black;
	    bg = Color.white;
	    Icon[] shapeIcons = new Icon[ATTR_CTS[SHAPE]];
	    for (int i=0; i<shapeIcons.length; i++) {
		shapeIcons[i] = new GSymbolIcon(
		   wh, wh, fg, bg, sz, i);
	    }

	    // sizes 
	    Icon[] sizeIcons = new Icon[ATTR_CTS[SIZE]];
	    for (int i=0; i<sizeIcons.length; i++) {
		sizeIcons[i] = new GSymbolIcon(
		    wh, wh, fg, bg, glyphSize(i), 
		    GraphData.SHAPE_ASTERISK);
	    }

	    icons = new Icon[][] { lineIcons, thickIcons,
	    	colorIcons, shapeIcons, sizeIcons };
	}

	// preferred plot glyph size
	public int glyphSize(int i) {
	    int sz = (int) (plotSize(i) * 2.2 + 0.8);
	    if ((sz & 1) == 1) sz++; // even sz is prettier
	    return sz;
	}
	     
	// symbol size for plots / icons
	public int plotSize(int i) {
	    double f = 0.4;
	    switch(i) {
	    case 0:  f = 0.275; break;
	    case 1:  f = 0.4; break;
	    case 2:  f = 0.55; break;
	    case 3:  f = 0.7; break;
	    }
	    return (int) (glook().fontSize() * f);
	} 
}
