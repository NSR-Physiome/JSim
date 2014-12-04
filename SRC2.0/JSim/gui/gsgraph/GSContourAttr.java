/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// graphing Attribute for contour/colormap plot
 
package JSim.gui.gsgraph;
 
import java.awt.*;
import javax.swing.*;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSContourAttr extends GridAttribute {
	private GSGeom geom;

	// constructor
	public GSContourAttr(GSGeom g, GraphData gdata) {
	    super();
	    geom = g;

	    // set style
	    int style = CONTOUR;
	    switch (gdata.colorMap) {
	    case GraphData.COLORMAP_AREA_FILL:
		style = (gdata.line == GraphData.LINE_NONE) ?
		    AREA_FILL : AREA_FILL_CONTOUR;
		break;
	    case GraphData.COLORMAP_RASTER:
		style = (gdata.line == GraphData.LINE_NONE) ?
		    RASTER : RASTER_CONTOUR;
		break;
	    }
	    setStyle(style);

	    // contour levels / colormap
	    if (style != RASTER) 
		setContours(gdata);
	    if (gdata.colorMap != GraphData.COLORMAP_NONE) 
		setColors(gdata);
	}

	// set contour levels more individually
	private void setContours(GraphData gdata) {
	    double[] zvals = geom.ztics.ticValues();
	    String[] zlabs = geom.ztics.ticLabels();
	    ContourLevels clevels = new ContourLevels();
	    for (int i=0; i<zvals.length; i++) {
		ContourLineAttribute attr = 
		    new ContourLineAttribute();
		attr.setCapStyleOverridden(true);
		attr.setStyleOverridden(true);
		attr.setDashArrayOverridden(true);
		attr.setDashPhaseOverridden(true);
		attr.setMiterLimitOverridden(true);
		attr.setWidthOverridden(true);
		attr.setStyle(LineAttribute.STROKE);
		attr.setColor(gdata.color);	
		attr.setLabelColor(gdata.color);
		attr.setLabelFont(geom.font);
		attr.setLabelHeightP(geom.fontHP);
		attr.setLabelHeightPOverridden(false);
		attr.setAutoLabel(false);
		attr.setLabelText(zlabs[i]);
		GSLineAttr.doStroke(attr, gdata);
		GSLineAttr.doDashArray(attr, gdata);
		clevels.addLevel(zvals[i], attr);
	    }
	    setContourLevels(clevels);
	}

	// set color map
	private void setColors(GraphData gdata) {

	    // color & fractions table
	    Color[] ctbl = null;
	    double[] ftbl = null;
	    switch (gdata.palette) {
	    case GraphData.PALETTE_GRAYSCALE: 
		ctbl = new Color[] { Color.black, Color.white };
		break;
	    case GraphData.PALETTE_RED: 
		ctbl = new Color[] { Color.black, Color.red };
		break;
	    case GraphData.PALETTE_GREEN: 
		ctbl = new Color[] { Color.black, Color.green };
		break;
	    case GraphData.PALETTE_BLUE: 
		ctbl = new Color[] { Color.black, Color.blue };
		break;
	    case GraphData.PALETTE_HEAT: 
		ctbl = new Color[] { 
		    Color.black, Color.red, Color.orange,
		    Color.yellow, Color.white };
		break;
	    case GraphData.PALETTE_RAINBOW: 
		ctbl = new Color[] { 
		    Color.red, Color.orange, Color.yellow,
		    Color.green, Color.blue, new Color(200,0,200) };
		break;
	    case GraphData.PALETTE_PET: 
		ctbl = new Color[] { 
		    Color.black, new Color(200,0,200), Color.blue,
		    Color.green, Color.yellow, Color.orange,
		    Color.red, Color.white };
		break;
	    default:
		ctbl = new Color[] { Color.black, Color.white };
		break;
	    }

	    // equally spaced fractions table if null
	    if (ftbl == null) {
		ftbl = new double[ctbl.length];
		for (int i=0; i<ftbl.length; i++) 
		    ftbl[i] = i/(ftbl.length-1f);
	    }
	    	    
	    // interp color array
	    int n = (gdata.colorMap == GraphData.COLORMAP_RASTER) ?
		256 : (geom.zlevels+1);  // pad 1 for occasional SGT access to color[n]
	    Color[] colors = new Color[n];   
	    for (int i=0; i<n; i++) {
		double f = i/(n-1f);
		int j=0;
		while (j<ctbl.length-1 && f>ftbl[j+1]) j++;
		double f1 = (f-ftbl[j]) / (ftbl[j+1]-ftbl[j]);
		int r = interp(f1, ctbl[j].getRed(), ctbl[j+1].getRed());
		int g = interp(f1, ctbl[j].getGreen(), ctbl[j+1].getGreen());
		int b = interp(f1, ctbl[j].getBlue(), ctbl[j+1].getBlue());
		colors[i] = new Color(r,g,b);
	    }

	    // set SGT color map
	    IndexedColorMap cmap = new IndexedColorMap(colors);
	    setColorMap(cmap);
	}

	// interp int color intensities
	private int interp(double f, int v1, int v2) {
	    return (int) ((1-f)*v1 + f*v2);
	}
}


