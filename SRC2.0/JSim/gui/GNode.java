/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// 1 Node in tree describing GUI component heirarchy

package JSim.gui;

import java.io.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.*;
import java.util.ArrayList;
import java.awt.print.*;

import org.jibble.epsgraphics.EpsGraphics2D;

import JSim.util.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.gui.model.*;

public class GNode implements Printable {

	// state
	private GNode parent;  // top node is null
	private GNode.List children; // may be null
	private PNamed pnamed;  // correspondence to project PNamed
	private JComponent jcomp; // parent Swing component
	protected boolean refreshing; // refresh() in progress
	protected boolean needsRefresh;// for GTabs children
	protected boolean needsContent;// for simple constructors

	// constructor
	public GNode(GNode p, PNamed pn) {
	    parent = p;
	    pnamed = pn;
	    children = null;
	    if (parent != null) parent.addChild(this);
	    needsContent = true;
	    needsRefresh = true;
	}

	// reset pnamed, use EXTREME caution
	protected void resetPNamed(PNamed pn) {
	    pnamed = pn;
	}

	// query
	public GNode parent() { return parent; }
	public int nChild() {
	    return (children==null) ? 0 : children.size();
	}
	public GNode child(int i) {
	    if (children==null) return null;
	    return children.gnode(i);
	}
	public GNode child(PNamed p) {
	    for (int i=0; i<nChild(); i++) 
		if (child(i).pnamed == p) 
		    return child(i);
	    return null;
	}
	public GNode.List children(Class clss) {
	    GNode.List list = new GNode.List();
	    for (int i=0; i<nChild(); i++) {
		GNode c = child(i);
		if (clss.isInstance(c))	
		    list.add(c);
	    }
	    return list;
	}
	public GNode firstChild(Class clss) {
	    for (int i=0; i<nChild(); i++) 
	    	  if (clss.isInstance(child(i)))
		    return child(i);
	    return null;
	}
	public PNamed pnamed() { return pnamed; }
	public JComponent jcomp() { return jcomp; }
	public GNode ancestor(Class clss) {
	    GNode n = this;
	    while (n != null && !clss.isInstance(n)) 
		n=n.parent;
	    return n;
	}
	public ASServer server() {
	    if (gappl() == null) return null;
	    return gappl().server();
	}
	public GMain gmain() { 
	    return (GMain) ancestor(GMain.class);
	}
	public GProject gproject() {
	    return (GProject) ancestor(GProject.class);
	}
	public GModel gmodel() {
	    return (GModel) ancestor(GModel.class);
	}
	public GPlotPage gplotpage() {
	    return (GPlotPage) ancestor(GPlotPage.class);
	}
	public GAppl gappl() { return gmain().gappl(); }
	public GLook glook() { return gmain().glook(); }
	public GHelp ghelp() { return gproject().ghelp(); }
	public GSBW  gsbw() { return gmain().gsbw(); }
	public GHelpLinks helpLinks() { return gmain().helpLinks(); }
	public boolean isApplet() { return gmain().isApplet(); }
	public boolean isRemote() { return gmain().isRemote(); }
//	public GNode gnode(PNamed p) { return GNamed.gnode(p); }
	public boolean needsRefresh() { return needsRefresh; }
	public Toolkit toolkit() {
	    return Toolkit.getDefaultToolkit();
	}
	// are controls in this node currently editable
	public boolean editable() { return parent.editable(); }
	// key to help database
	public String helpKey() { return null; }

	// update
	public void addChild(GNode c) {
	    if (children == null) 
		children = new GNode.List();
	    children.add(c);
	}
	public void setJComp(JComponent c) { jcomp = c; }
	public void warning(String s) { 
	    GProject gproj = gproject();
	    if (gproj == null)
	    	System.err.println(s);
	    else 
	        gproj.warning(s); 
	}
	public void setHelp(JComponent jcomp, Object obj) {
	    jcomp.addMouseListener(ghelp());
	    jcomp.addMouseMotionListener(ghelp());
	    jcomp.putClientProperty("help", obj);
	}
	public JMenu newMenu(String s) { 
	    return new GMenu(s, this);
	}

	// hide associated popups (if any)
	public void hidePopups() {
	    for (int i=0; i<nChild(); i++) 
		child(i).hidePopups();
	}

	// destroy this GNode, remove instantiations of it
	public void destroy() {
//	    if (gnode(pnamed) == this) 
//		GNamed.setGNode(pnamed, null);
	    int inx = parent.children.indexOf(this);
	    if (inx < 0) 
		warning("GNode destroy could not find " + 
		    pnamed.name() + " in GNode parent");
	    else 
		parent.children.remove(inx);
	}

	// model vars utils
	public String parenName(ASVar v) {
	    String s = v.name();
	    int ct = v.ndim();
	    if (ct == 0 || v.isDomain()) return s;
	    s = s + "(";
	    for (int i=0; i<v.ndim(); i++) {
		if (i>0) s = s + ","; 
		s = s + v.domain(i).name();
	    }
	    return s + ")";
	}

	// set preferred width in proportion to fontSize 
	public void setPreferredWidth(double w0) {
	    JComponent j = jcomp();
	    Dimension dim0 = j.getPreferredSize();
	    int w = (int) (w0 * glook().fontSize());
	    Dimension dim1 = new Dimension(w, dim0.height);
	    j.setPreferredSize(dim1);
	}

	// set JTextArea margin off left boundary
	public void setIndentedBorder(JComponent c) {
	    c.setBorder(new EmptyBorder(0,4,0,0));
	}

	// dimension utilities
	public Dimension max(Dimension d1, Dimension d2) {
	    int w = d1.width;
	    if (d2.width > w) w = d2.width;
	    int h = d1.height;
	    if (d2.height > h) h = d2.height;
	    return new Dimension(w,h);
	}
	public Dimension dim(Dimension d, double f1, double f2) {
	    int w = (int) (d.width*f1);
	    int h = (int) (d.height*f2);
	    return new Dimension(w,h);
	}

	// GLook has been updated
	public void lookUpdated() {
	    for (int i=0; i<nChild(); i++)
		child(i).lookUpdated();
	}

	// set content tab label
	public void setTabLabel(JLabel label) {
	    PNamed p = pnamed;
	    while (p != null && ! (p.parent() instanceof Project))
	    	p = p.parent();
	    if (p == null) return;
	    Icon icon = glook().tabIcon(p.getClass());
	    if (icon == null) return;
	    label.setIcon(icon);
	    label.setText(Util.midcrop(p.name(), 20) + ":  ");
	    label.setSize(label.getPreferredSize());
	}

	// refresh window content
	public void refresh() {
	    if (refreshing) return;
	    refreshing = true;

	    // build content 1st time
	    if (needsContent) makeContent();

	    // debugging message
/*	    String msg = "Refreshing " + this.getClass().getName() + " ";
	    if (pnamed instanceof Control) 
		msg = msg + ((Control) pnamed).dotname();
	    else if (pnamed != null) 
		msg = msg + pnamed.dotname();
	    System.err.println(msg);
*/
	    for (int i=0; i<nChild(); i++) 
		child(i).refresh();
	    needsRefresh = false;
 	    refreshing = false;	
	}

	// make content, relieve constructor burden
	public void makeContent() {
	    needsContent = false;
	}

	// print text dialog
	public void printText(String title, String content) throws Xcept {
	    PrinterJob pjob = PrinterJob.getPrinterJob();
	    pjob.setJobName(title);
	    if (! pjob.printDialog()) throw new Xcept(
		"Printing canceled"); 
	    Printable gtext = new GTextPrintable(title, content);
	    pjob.setPrintable(gtext);
	    gproject().message("Sending text to printer..."); 
	    try {
		pjob.print();
	    } catch (PrinterException e) {
		throw new Xcept(e.toString());
	    }
	}

	// associated frame, if any
	public Frame frame() {
	    Window w = SwingUtilities.getWindowAncestor(
		jcomp());
	    if (w instanceof Frame) return (Frame) w;
	    return null;
	}

	// center component, set & return pref size
	public Dimension centerJComp(int x0, int x1, int y) {
	    return centerJComp(jcomp(), x0, x1, y);
	}
	public static Dimension centerJComp(JComponent jcomp, 
	int x0, int x1, int y) {
	    Dimension dim = jcomp.getPreferredSize();
	    jcomp.setSize(dim);
	    int x = x0 + (x1-x0-dim.width)/2;
	    if (x<x0) x = x0;
	    jcomp.setLocation(x, y);
	    return dim;
	}

	// start print job for this  node
	public void startPrintJob() throws Xcept {
	    PrinterJob pjob = PrinterJob.getPrinterJob();
	    pjob.setJobName("JSim print");
	    pjob.setPrintable(this);
	    if (! pjob.printDialog()) 
		throw new Xcept("Printing canceled");
	    gproject().message("Sending page to printer...");
	    try {
		pjob.print();
	    } catch (PrinterException e) {
		throw Xcept.wrap(e);
	    }
	}
	    
	// print
	public int print(Graphics g, PageFormat pf, int inx) 
	throws PrinterException {
	    if (inx>0) return(Printable.NO_SUCH_PAGE);
	    jcomp().print(g);
	    return(Printable.PAGE_EXISTS);
	}

	// start an EPS export job for this node
	public void exportEPS(File f) throws Xcept {
	    // render node -> EPS -> String
	    Graphics2D g2d = new EpsGraphics2D();
	    jcomp.print(g2d);
	    String epsout = g2d.toString();
	    g2d.dispose();

	    // write file
	    if (f.getName().indexOf('.') < 0)
		f = new File(f.toString() + ".eps");
	    try {
		BufferedWriter out = new BufferedWriter(
	    	    new FileWriter(f));
		out.write(epsout);
	        out.newLine();
	        out.close();
	    } catch (IOException e) { 
		throw Xcept.wrap(e);
	    }
	}

	// export graphic-editable state before project save
 	public void exportGraphicState() throws Xcept {
	    for (int i=0; i<nChild(); i++) 
	    	child(i).exportGraphicState();
	}

	// List
	public static class List extends ArrayList<GNode> {
	    public List() { super(); }
	    public GNode gnode(int i) { return (GNode) get(i); }
	    public GControl gcntl(int i) { return (GControl) get(i); }
	}

	//// DEBUG UTILS

	public void dumpMaps(String name) {
	    dumpMaps(name, jcomp);
	}
	public static void dumpMaps(String name, JComponent jcomp) {
	    dumpInputMap(name, jcomp, 
	        JComponent.WHEN_IN_FOCUSED_WINDOW);
	    dumpInputMap(name, jcomp, 
	        JComponent.WHEN_FOCUSED);
	    dumpInputMap(name, jcomp,
	    	JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
	    dumpActionMap(name, jcomp);
	}
        public static void dumpInputMap(String name, JComponent
	jcomp, int condition) {
	    System.err.println("InputMap_" + condition + ": " + name);
	    if (jcomp == null) {
	        System.err.println("  null component");
		return;
	    }
	    System.err.println("  class " + jcomp.getClass());
	    InputMap imap = jcomp.getInputMap();
	    KeyStroke[] keys = imap.allKeys();
	    if (keys == null) {
	        System.err.print("  empty");
	        return;
 	    }
	    for (int i=0; i<keys.length; i++) {
	    	Object o = imap.get(keys[i]);
		System.err.println("  " + keys[i] + " => " + o);
	    }
	}
        public static void dumpActionMap(String name, JComponent jcomp) {
	    System.err.println("ActionMap from " + jcomp.getClass());
	    if (jcomp == null) {
	        System.err.println("  null component");
		return;
	    }
	    ActionMap amap = jcomp.getActionMap();
	    Object[] akeys = amap.allKeys();
	    if (akeys == null) {
	        System.err.print("  empty");
		return;
	    }
	    for (int i=0; i<akeys.length; i++) {
	        Object o = amap.get(akeys[i]);
		System.err.println("    " + akeys[i] + " => " + o);
	    }
	}

}

 
