/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// Geometry calculations for GSGraph
 
package JSim.gui.gsgraph;
 
import java.text.*;
import java.util.*;
import java.awt.*;
import javax.swing.*;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSGeom {
	// SGT bug require about 100 pixels/PU, 
	//   otherwise contour plot lines have way too many labels
	//   if this bug were fixed,  could scale pixels=PU
	//   any remove all PIXPU references 
	public static final double PIXPU = 100; // pixels per PU

	// private state
	private GSGraph graph; // for this graph

	// layer dimensions
	protected Dimension layerDim; // graph/layer pixel dims
	protected double layerWP; // layer width PU
	protected double layerHP; // layer height PU
	protected Dimension2D layerDimP; // layer dims (PU)

	// fonts
	protected Font font;
	protected double fontHP; // font height in PU
	protected double ticHP; // tic height in PU
	protected double fontW;  // font width in PU
	protected double tfontHP; // title font height in PU
	protected double ffontHP; // footer font height in PU

	// messages/locations
	protected String titleText, footerText, errorText;
	protected Point2D.Double titlePos, footerPos, errorPos,
	    legendPos;
	protected Rectangle legendBounds; // (pixels) for help query

	// style, axes
	protected int style; // GraphLayout.STYLE_?
	protected GSGeom.Axis xaxis, yaxis; // axes

	// z axis configuration
	protected GSTics ztics; // Z tics
	protected int zlevels; // # contour/colormap levels

	// constructor
	public GSGeom(GSGraph g) {
	    graph = g;
	    recalc(new Dimension(300, 300));
	}

	// recalculate at new size
	public void recalc(Dimension dim) {
	    if (dim != null) layerDim = dim;

	    // window dimension ~= layer phys dim
	    if (layerDim.width < 50) layerDim.width=50;
	    if (layerDim.height < 50) layerDim.height=50;
	    layerWP = layerDim.width / PIXPU;
	    layerHP = layerDim.height / PIXPU;
	    layerDimP = new Dimension2D(layerWP, layerHP);
	    errorText = null;
	    errorPos = null;


	    // fonts
	    GraphLayout glayout = graph.graphLayout();
	    if (glayout == null) glayout = new GraphLayout();
	    int fsize = glayout.fontSize;
	    fsize = fsize - 4;
	    if (fsize < 4) fsize = 4;
	    font = new Font("Lucida Sans", Font.PLAIN, fsize);
	    fontHP = fsize * 1.5 / PIXPU;
	    ticHP = fontHP * 0.4;
	    tfontHP = fontHP * 1.2;
	    ffontHP = fontHP * 0.9;
	    // tried getMaxAdvance() below, but was too big
	    fontW = graph.getFontMetrics(font).stringWidth("E") / PIXPU;

	    // style
	    style = glayout.style;

	    // adjust axis bounds depending on what's drawn
	    double leftP = 0.02*layerWP;
	    double rightP = 0.94*layerWP;
rightP = 0.98*layerWP; // ???
	    double bottomP = 0.02*layerHP;
	    double topP = 0.98*layerHP;

	    // error message
	    errorText = null;
	    if (glayout.xaxis == null ||
		glayout.yaxis == null ||
		glayout.data == null ||
		glayout.data.length == 0)
		errorText = "No data available...";
	    if (! Util.isBlank(glayout.errorMsg))
		errorText = glayout.errorMsg;
	    if (! Util.isBlank(errorText)) {
		errorPos = new Point2D.Double(leftP, bottomP);
		bottomP += fontHP;
	    }
	    
	    // title
	    titleText = 
		(glayout.showTitle) ? glayout.title : null;
	    if (Util.isBlank(titleText)) {
		titlePos = null;
	    } else {
		titlePos = new Point2D.Double(
		    glayout.titleX*layerWP, 
		    glayout.titleY*layerHP);
		topP -= tfontHP;
	    }

	    // footer
	    footerText = 
		(glayout.showFooter) ? glayout.footer : null;
	    if (footerText != null) {
		DateFormat fmt = new SimpleDateFormat(
		    "ddMMMyy, HH:mm");
		String sdate = fmt.format(
		    new Date(glayout.timeStamp));
		footerText = footerText.replaceAll("%TIME", sdate);
	    }
	    if (Util.isBlank(footerText)) {
		footerPos = null;
	    } else {
		footerPos = new Point2D.Double(
		    glayout.footerX*layerWP,
		    glayout.footerY*layerHP);
		bottomP += ffontHP;
	    }

	    // legend position
	    legendPos = null;
	    legendBounds = null;
	    if (style == GraphLayout.STYLE_2DLINE 
	    && glayout.showLegend
	    && glayout.data != null) {
		legendPos = new Point2D.Double(
		    glayout.legendX*layerWP,  
		    glayout.legendY*layerHP);
	    }

	    // space for axis label, tics, ticlabels
	    boolean showXlab = (glayout.xaxis == null) ? false : 
		(glayout.xaxis.showLabel && !Util.isBlank(glayout.xaxis.label));
	    if (showXlab) 
	    	bottomP += fontHP;
	    boolean showXtics =  (glayout.xaxis == null) ? 
	    	true : glayout.xaxis.showTics;
	    if (showXtics) 
	    	bottomP += fontHP + ticHP; // tics and numeric labels
	    boolean showYlab = (glayout.yaxis == null) ? false : 
		(glayout.yaxis.showLabel && !Util.isBlank(glayout.yaxis.label));
	    if (showYlab) 
	    	leftP += fontHP;
	    boolean showYtics =  (glayout.yaxis == null) ? 
	    	true : glayout.yaxis.showTics; 
	    if (showYtics) 
	    	leftP += fontHP + ticHP; // tics and numeric labels

	    // create XY axes and origins
	    xaxis = new GSGeom.Axis(
		leftP, rightP, glayout.xaxis);
	    yaxis = new GSGeom.Axis(
		bottomP, topP, glayout.yaxis);
	    xaxis.origin = new Point2D.Double(
		xaxis.tics.axismin(), yaxis.tics.axismin());
 	    yaxis.origin = xaxis.origin;
	    // axes through middle of plot data hard to read

	    // Z axis
	    GraphLayout.Axis zaxis = glayout.zaxis;
	    if (zaxis == null) zaxis = new GraphLayout.Axis();
	    ztics = new GSTics(
		zaxis.min, zaxis.max, zaxis.log, zaxis.auto, 0);
	    zlevels = 2 + ztics.ticValues().length;

	    // positions
	    
	}	

	// size of a range
	protected double size(Range2D range) {
	    return range.end - range.start;
	}   

	// apply transforms to graph
	protected void setTransforms(CartesianGraph cgraph) {
	    cgraph.setXTransform(xaxis.tran);
	    cgraph.setYTransform(yaxis.tran);
	}

	//// one axis 
	public class Axis {
	    protected Range2D rangeP; // range in phys coords
	    protected GSTics tics;  // tics parms
	    protected Range2D rangeU; // range in user coords
	    protected AxisTransform tran; // PU transform
	    protected Range2D clipRangeU; // clip range 
	    protected double clipDeltaU; // clip delta
	    protected Point2D.Double origin; // axis origin
	    protected String label; // label to show,  if any

	    // constructor
	    public Axis(double startP, double endP, GraphLayout.Axis gaxis) {
		if (gaxis == null) gaxis = new GraphLayout.Axis();

		// physical range		
		rangeP = new Range2D(startP, endP);

	    	// tic positions/labels
	    	int nchars = (int) (size(rangeP)/fontW);
		boolean auto = (style == GraphLayout.STYLE_CONTOUR) ? 
		    false : gaxis.auto; // don't expanded axes for contours
	    	tics = new GSTics(gaxis.min, gaxis.max, 
		    gaxis.log, auto, nchars);

		// user range for axes 
	    	rangeU = new Range2D(
		    tics.axismin(), tics.axismax(), tics.minDelta()/2);

		// axis transform
	    	if (tics.log()) 
	    	    tran = new LogTransform(rangeP, rangeU);
	    	else
	    	    tran = new gov.noaa.pmel.sgt.LinearTransform(
		        rangeP, rangeU);

	    	// data clipping 
		double fudge = tics.axisrange() * 0.05;
	    	clipRangeU = new Range2D(
		    tics.axismin() - fudge,
		    tics.axismax() + fudge);
	    	clipDeltaU = 0.5 * size(rangeU) / (size(rangeP)*PIXPU);

		// label
		label = gaxis.showLabel ? gaxis.label : null;
	    }
	}

}



