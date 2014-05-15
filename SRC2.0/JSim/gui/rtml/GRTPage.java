/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// GRT Page (displayable pane)

package JSim.gui.rtml;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GRTPage extends GRTNode implements Named {

	// constants
	private static final Integer BACKDROP = new Integer(1);
	private static final Integer BOTTOM = new Integer(2);
	private static final Integer MIDDLE = new Integer(3);
	private static final Integer TOP = new Integer(4);

	// state
	Element loadElement; // non-null if load needed
	protected String name;
	private JLayeredPane layers;

	// empty constructor
	protected GRTPage(GRTNode p) {
	    super(p);
	}

	// constructor
	public GRTPage(GRTNode p, Element e) {
	    this(p);
	    name = e.getAttribute("name");
	    loadElement = e;
	}

	// get jcomp() allows delayed creation
	public JComponent jcomp() {
	    if (loadElement != null) {
		addXMLChildren(loadElement);
		loadElement = null;
	    }
	    if (super.jcomp() == null)
		makeJComp();
	    return super.jcomp();
	}

	// make JComponents
	public void makeJComp() {
	    layers = new JLayeredPane() {
	    	public void updateUI() {
		    super.updateUI();
		    auxUpdateUI();
		}
	    };
	    JPanel panel = new JPanel();
	    panel.setBackground(Color.white);
	    layers.add(panel, BACKDROP);
	    JScrollPane scroll = new JScrollPane(layers);
	    scroll.getViewport().setBackground(Color.white);
	    scroll.getVerticalScrollBar().setUnitIncrement(
		glook().fontSize());
	    scroll.getHorizontalScrollBar().setUnitIncrement(
		glook().fontSize());
	    GControl.cursorTraversal(scroll);
	    setJComp(scroll);

	    // add page items
	    int xmax = 100;
	    int ymax = 100;
	    for (int i=0; i<nChild(); i++) {
		GRTPageItem c = (GRTPageItem) child(i);
		c.makeJComp();
		if (c.jcomp() == null) continue;
		Integer layer = (c instanceof GRTImage) ?
		    BOTTOM : MIDDLE;
		layers.add(c.jcomp(), layer);
		c.jcomp().setVisible(c.visible());
		if (i==0 || c.xMax()>xmax) xmax = c.xMax();
		if (i==0 || c.yMax()>ymax) ymax = c.yMax();
	    }

	    layers.setPreferredSize(new Dimension(xmax, ymax));
	}

	// aux updateUI(): GRTVarButton aux gets confused, clear
	private void auxUpdateUI() {
	    for (int i=0; i<nChild(); i++) {
	    	if (! (child(i) instanceof GRTVarButton)) continue;
		((GRTVarButton) child(i)).clearAux();
	    }
	}

	// add components at different levels
	public void addTop(JComponent c) { 
	    layers.add(c, TOP);
layers.setComponentZOrder(c, 0);
	    layers.revalidate(); // needed for 1st time popup 
	}

	// key to help database
	public String helpKey() {
	    return "GRTPage" + GHelp.sep + name();
	}

	// create child from XML Element
	public void addXMLChild(Element e) {
	    String n = e.getNodeName();
	    if (n.equals("table")) 
		new GRTTable(this, e);
	    else if (n.equals("image")) 
		new GRTImage(this, e);
	    else if (n.equals("text")) 
		new GRTText(this, e);
	    else if (n.equals("pageButton")) 
		new GRTPageButton(this, e);
	    else if (n.equals("varButton")) 
		new GRTVarButton(this, e);
	    else if (n.equals("removeButton")) 
		new GRTRemoveButton(this, e);
	    else if (n.equals("graph")) 
		new GRTGraph(this, e);
	    else if (n.equals("svg")) 
		new GRTSvg(this, e);
	    else 
		super.addXMLChild(e);
	}

	// query
	public String name() { return name; }
	public String diagInfo() { return "Page " + name; }

	// action to load page
	public static class PageAction extends GAction {
	    String page;
	    public PageAction(GNode n, String p) {
		super(n, p);
		page = p;
	    }
	    public PageAction(GNode n, String l, String p) {
		super(n, l);
		page = p;
	    }
	    public void doit() {
		gnode.gmodel().gpars().setPage(page);
	    }
	}

	// when theme updated,  clear content
	public void lookUpdated() {
	    setJComp(null);
	    needsContent = true;
	}

	// refresh page
	public void refresh() {
	    super.refresh();

	    // revise child visibility
	    for (int i=0; i<nChild(); i++) {
		GRTPageItem c = (GRTPageItem) child(i);
		if (c.jcomp() == null) continue;
		c.jcomp().setVisible(c.visible());
	    }
	}		

	// set focus to top of page
	//   required for menubar accels to activate after compile
	public void setFocusTop() {
	    for (int i=0; i<nChild(); i++) {
		GRTPageItem c = (GRTPageItem) child(i);
	        if (c.jcomp() == null) continue;
		c.jcomp().requestFocus();
		break;
	    }
	}
}

 
