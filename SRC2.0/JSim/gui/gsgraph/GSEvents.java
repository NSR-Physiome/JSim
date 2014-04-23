/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// listener for relevant GSGraph events
 
package JSim.gui.gsgraph;
 
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import java.beans.*;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;
import JSim.gui.GUtil;

public class GSEvents implements ComponentListener, 
PropertyChangeListener {

	private GSGraph graph;  // for this graph
	private GSGeom geom;  // geometry
	private GraphLayout glayout; // layout for 1 event's processing

	private boolean queryVisible; // coordinate query popup visible?
	private Window queryPopup; // coord query popup window
	private JLabel queryLabel; // coord query label

	private int legendHelpInx; // current help index shown

	// constructor
	public GSEvents(GSGraph g) {
	    graph = g;
	    geom = graph.geom();
	    graph.addComponentListener(this);
	    graph.addPropertyChangeListener(this);
	    legendHelpInx = -1;
	}

	// additional process for SGT mouse events
	public void processMouseEvent(MouseEvent e) {
	    glayout = graph.graphLayout();
	    if (glayout == null) return;
	    if (isQuery(e))
		showQuery(e.getPoint());
	    else
		hideQuery();
	}

	// additional process for SGT mouse motion events
	public void processMouseMotionEvent(MouseEvent e) {
	    glayout = graph.graphLayout();
	    if (glayout == null) return;
	    if (queryVisible)
		queryLoad(e.getPoint());
	    helpMotion(e.getPoint());
	}

	//// coordinate query processing

	// is MouseEvent coordinate query request?
	private boolean isQuery(MouseEvent e) {
	    if (e.getID() != e.MOUSE_PRESSED) return false;
	    int mods = e.getModifiersEx();
	    int down = MouseEvent.BUTTON3_DOWN_MASK;
	    int up = MouseEvent.SHIFT_DOWN_MASK
		| MouseEvent.CTRL_DOWN_MASK 
		| MouseEvent.META_DOWN_MASK
		| MouseEvent.ALT_DOWN_MASK;
	    if ((mods & (down|up)) == down) return true;
	    down = MouseEvent.BUTTON1_DOWN_MASK
		| MouseEvent.SHIFT_DOWN_MASK;
	    up = MouseEvent.CTRL_DOWN_MASK 
		| MouseEvent.META_DOWN_MASK
		| MouseEvent.ALT_DOWN_MASK;
	    if ((mods & (down|up)) == down) return true;
	    return false;
	}

	// show query
	private void showQuery(Point p) {

	    // create new queryPopup?
	    if (queryPopup == null) {
		queryPopup = new Window( 
		    SwingUtilities.getWindowAncestor(graph));
		queryLabel = new JLabel();
	    	queryLabel.setBorder(new EtchedBorder());
		queryPopup.add(queryLabel);
	    }		

	    // position/load/show queryPopup
	    Point qpos = GUtil.getLocationOnScreen(graph);
	    qpos.x += p.x + geom.fontHP*geom.PIXPU;
	    qpos.y += p.y + geom.fontHP*geom.PIXPU;
	    queryPopup.setLocation(qpos);
	    queryLoad(p);
	    queryVisible = true;
	}
	
	// load/reload query text
	private void queryLoad(Point p) {
	    double x = geom.xaxis.tran.getTransU(p.x/geom.PIXPU);
	    double y = geom.yaxis.tran.getTransU(geom.layerHP - p.y/geom.PIXPU);
	    queryLabel.setText(geom.xaxis.tics.prettyQuery(x)
		+ " " + geom.yaxis.tics.prettyQuery(y));
	    Dimension dim = queryLabel.getPreferredSize();
	    dim.height += GUtil.windowWarningBannerHeight(queryPopup);  
	    queryPopup.setSize(dim);
	    queryPopup.setVisible(true); // must be here, or motion inconsistent
	}

	// hide query
	private void hideQuery() {
	    if (! queryVisible) return;
	    queryPopup.setVisible(false);
	    queryVisible = false;
	}

	//// help system processing

	// motion affects help popup?
	private void helpMotion(Point p) {

	    // help system active?
	    if (graph.ctxt() == null) return;
//	    if (! graph.ctxt().helpActive()) return;

	    // need new legend bounds?
	    if (geom.legendBounds == null) {
		updateLegendBounds();
		if (geom.legendBounds == null) return;
	    }

	    // calc new help index,  see if changed
	    int inx = -1;
	    if (geom.legendBounds.contains(p)) 
		inx = (int) ((p.y - geom.legendBounds.y) 
		    / (geom.fontHP*geom.PIXPU));
	    if (inx == legendHelpInx) return;
	    legendHelpInx = inx;

	    // popdown help?
	    if (inx < 0) {
		graph.ctxt().showHelp(null, null);
		return;
	    }
	    
	    // get help key from layout data
	    String hkey = null;
	    if (glayout.data != null 
	    && inx < glayout.data.length) 
		hkey = glayout.data[inx].help;

	    // get rectangle bounds
	    Rectangle rect = null;
	    SGLabel[] labels = graph.baseLayer().legendLabels();
	    if (labels != null 
	    && inx < labels.length
	    && labels[inx] != null) 
		rect = labels[inx].getBounds();

	    // show help
	    graph.ctxt().showHelp(rect, hkey);
	}

	// update legend bounds
	private void updateLegendBounds() {
	    if (graph.baseLayer() == null) return;
	    LineKey legend = graph.baseLayer().legend();
	    if (legend == null) return;
	    geom.legendBounds = legend.getBounds();
	}

	//// ComponentListener methods

	public void componentHidden(ComponentEvent e) { }
	public void componentMoved(ComponentEvent e) { }
	public void componentShown(ComponentEvent e) { }
	public void componentResized(ComponentEvent e) {
	    graph.resize();
	}

	//// property change listener
	public void propertyChange(PropertyChangeEvent e) {

	    // skip if no layout or context
	    glayout = graph.graphLayout();
	    if (glayout == null) return;
	    if (graph.ctxt() == null) return;
	    if (graph.baseLayer() == null) return;

	    // local fields
	    Object source = e.getSource();
	    String name = e.getPropertyName();
	    Object value = e.getNewValue();

	    // farm out to appropriate handler
	    if (name == "zoomRectangle" 
	    && source == graph) 
		zoom((Rectangle) value);

	    else if (name == "objectSelected"
	    && source == graph 
	    && (value instanceof SGLabel)) 
		editLabel((SGLabel) value);

	    else if (name.endsWith("location")
	    && (source instanceof SGLabel)
	    && (value instanceof Point)) 
		moveLabel((SGLabel) source);

	    else if (name.endsWith("location")
	    && (source instanceof SGLabel)
	    && (value instanceof Point2D.Double)) 
		moveLegend();

	    // event tracing for debug only
	    // else System.err.println("" + source + " " + name + "=" + value);
	}

	// zoom rectangle
	private void zoom(Rectangle r) {
	    // ignore if tiny
	    double tiny = geom.PIXPU*graph.geom().fontHP/2;
	    if (r.width < tiny || r.height < tiny) return;

	    // get user coords,  appropriately rounded
	    double xmin = geom.xaxis.tran.getTransU(
		r.x/geom.PIXPU);
	    double xmax = geom.xaxis.tran.getTransU(
		(r.x+r.width)/geom.PIXPU);
	    double ymin = geom.yaxis.tran.getTransU(
		geom.layerHP-(r.y+r.height)/geom.PIXPU);
	    double ymax = geom.yaxis.tran.getTransU(
		geom.layerHP-r.y/geom.PIXPU);
	    xmin = Util.round(xmin, geom.xaxis.tics.minDelta()/100);
	    xmax = Util.round(xmax, geom.xaxis.tics.minDelta()/100);
	    ymin = Util.round(ymin, geom.yaxis.tics.minDelta()/100);
	    ymax = Util.round(ymax, geom.yaxis.tics.minDelta()/100);
	    graph.ctxt().updateAxisRanges(glayout,
		xmin, ymin, xmax, ymax);
	}

	// edit label
	private void editLabel(SGLabel sglabel) {
	    String id = sglabel.getId();
	    String text = id.equals("footer") ?
		glayout.footer : sglabel.getText();
	    text = JOptionPane.showInputDialog(graph,
		"Modify plot " + id, text);
	    if (text == null) return;    
	    if (id.equals("title"))
		glayout.title = text;
	    else if (id.equals("footer"))
		glayout.footer = text;
	    else if (id.equals("X axis label"))
		glayout.xaxis.label = text;
	    else if (id.equals("Y axis label"))
		glayout.yaxis.label = text;
	    else
		return;
	    graph.ctxt().updateEditables(
		new GraphLayout.Editables(glayout));
	    graph.setGraphLayout(glayout, null); // TOO CLUMSY!!!
	}

	// move label
	private void moveLabel(SGLabel sglabel) {
	    String id = sglabel.getId();
	    Point2D.Double p = sglabel.getLocationP();
	    double x = p.x / geom.layerWP;
	    double y = p.y / geom.layerHP;
	    if (id.equals("title")) {
		glayout.titleX = x;
		glayout.titleY = y;
	    } else if (id.equals("footer")) {
		glayout.footerX = x;
		glayout.footerY = y;
	    } else
		return;
	    graph.ctxt().updateEditables(
		new GraphLayout.Editables(glayout));
	    graph.setGraphLayout(glayout, null); // TOO CLUMSY!!!
	}

	// move legend
	private void moveLegend() {
	    LineKey legend = graph.baseLayer().legend();
	    if (legend == null) return;
	    Point2D.Double p = legend.getLocationP();
	    double x = p.x / geom.layerWP;
	    double y = p.y / geom.layerHP;
	    if (x == glayout.legendX || y == glayout.legendY)
		return;
	    glayout.legendX = x;
	    glayout.legendY = y;
	    graph.ctxt().updateEditables(
		new GraphLayout.Editables(glayout));
	    updateLegendBounds();
	}

}

