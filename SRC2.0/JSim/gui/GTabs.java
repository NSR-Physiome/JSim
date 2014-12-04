/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// JTabbedPane with GNode children for efficient refresh

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;

public class GTabs extends GAbstractTabs implements ChangeListener,
MouseMotionListener {

	// state
	protected JTabbedPane tabs;
	protected GNode.List gnodes; // child nodes with tabs
	private boolean themeIcons; // add theme icons?
	private int stack; // last forward pane
	private int mouseTab; // tab under mouse, or -1
	private GNode lastNode; // last node selected,  for hidePopups()

	// constructor
	public GTabs(GNode g, PNamed p) {
	    this(g, p, false);
	}
	public GTabs(GNode g, PNamed p, boolean icons) {
	    super(g, p);
	    gnodes = new GNode.List();
	    tabs = new JTabbedPane();
	    setJComp(tabs);
	    tabs.addChangeListener(this);
	    tabs.addMouseMotionListener(this);
	    stack = -1;
	    mouseTab = -1;
	    themeIcons = icons;
	}

	// add node
	public void addTab(String s, GNode gnode) {	    
	    gnodes.add(gnode);
	    Icon icon = themeIcons ? glook().tabIcon(gnode) : null;
	    tabs.addTab(s, icon, gnode.jcomp());
	}

	// refresh
	public void refresh() {
	    if (needsContent) makeContent();
	    needsRefresh = false;
	    for (int i=0; i<gnodes.size(); i++)
		gnodes.gnode(i).needsRefresh = true;
	    GNode gnode = selectedNode();
	    if (gnode == null) return;
//	    System.err.println("Refreshing tab " + currName());
	    gnode.refresh();
	}

	// hide popups
	public void hidePopups() {
	    if (needsContent) return;
	    GNode gnode = selectedNode();
	    if (gnode == null) return;
	    gnode.hidePopups();
	}

	// remove tab
	public void removeTab(GNode gnode) {
	    int inx = indexOf(gnode);
	    if (inx<0) return;
	    tabs.removeTabAt(inx);
	    gnodes.remove(gnode);
	}

	// rename tab
	public void renameTab(GNode gnode, String n) {
	    int inx = indexOf(gnode);
	    if (inx<0) return;
	    tabs.setTitleAt(inx, n);
	}

	// index for node
	public int indexOf(GNode gnode) {
	    int i = gnodes.indexOf(gnode);
//	    if (i<0) System.err.println(
//		"GTabs cannot index " + gnode.pnamed().name());
	    return i;
	}

	// selected node
	public GNode selectedNode() {
	    if (tabs.getTabCount() == 0) return null;
	    int i = tabs.getSelectedIndex();
	    if (i<0 || i>=gnodes.size()) {
		String n = (pnamed() == null) ?
		    "null" : pnamed().name();
//		System.err.println("GTabs " + n + 
//		    " cannot find currrent tab");
		return null;
	    }
	    return gnodes.gnode(i);
	}
	    
	// change listener
	public void stateChanged(ChangeEvent e) {
	    if (lastNode != null) lastNode.hidePopups();
	    if (tabs.getTabCount() == 0) return;
	    GNode gnode = selectedNode();
	    lastNode = gnode;
	    if (gnode == null) return;
	    if (gnode.needsRefresh()) {
//	    	System.err.println("Refreshing tab " + currName());
		gnode.refresh();
	    }
	}

	// mouse motion to track help tab
	public void mouseDragged(MouseEvent e) { }
	public void mouseMoved(MouseEvent e) { 
	    int x = e.getX();
	    int y = e.getY();
	    int tab = tabs.getUI().tabForCoordinate(
		tabs, x, y);
	    if (tab != mouseTab) {
		mouseTab = tab;
		ghelp().showHelp(tabs, this);
	    }
	}

	// help key points to mouseTab
	public String helpKey() {
	    if (mouseTab < 0 || mouseTab >= gnodes.size()) 
		return null;
	    GNode gnode = gnodes.gnode(mouseTab);
	    return "tab" + GHelp.sep + 
		GHelp.shortClassName(gnode);	    
	}	    

	// tab bounds
	public Rectangle tabBounds() {
	    if (mouseTab < 0) return null;
	    return tabs.getUI().getTabBounds(
		tabs, mouseTab);
	}

	// name of current tab
	public String currName() { 
	    int inx = tabs.getSelectedIndex();
	    if (inx <0) return "???";
	    return tabs.getTitleAt(inx);
	}

	// set tab placement
	public void setTabPlacement(int i) {
	    tabs.setTabPlacement(i);
	}

	// set selection
	public void setSelectedIndex(int i) {
	    GNode node = selectedNode();
	    if (node != null) node.hidePopups();
	    tabs.setSelectedIndex(i);
	}

	// push pop 
	public void pushFirst() {
	    if (tabs.getTabCount()<1) return;
	    stack = tabs.getSelectedIndex();
	    setSelectedIndex(0);
	}
	public void pushLast() {
	    if (tabs.getTabCount()<1) return;
	    stack = tabs.getSelectedIndex();
	    setSelectedIndex(tabs.getTabCount()-1);
	}
	public void pushNode(GNode gnode) {
	    int i = indexOf(gnode);
	    if (i < 0) return;
	    stack = tabs.getSelectedIndex();
	    setSelectedIndex(i);
	}
	public void pop() {
	    if (stack<0 || stack>=tabs.getTabCount())
		return;
	    setSelectedIndex(stack);
	}

	// query
	public JTabbedPane tabs() { return tabs; } 
}
