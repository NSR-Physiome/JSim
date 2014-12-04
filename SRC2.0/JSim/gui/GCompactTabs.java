/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// compact 1-line tabs

package JSim.gui;

import java.util.ArrayList;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;
import JSim.gui.jcomp.*;

public class GCompactTabs extends GAbstractTabs 
implements JSCompactTabs.Listener {

	// state
	private Class[] classes; // gnode classes
	private ArrayList<Item> items; // tab items
	private Item stack; // last forward pane
	private Item currItem; // current displayed item
	private boolean isLeft; // is project leftside tabs?
	private ArrayList<ChangeListener> changeListeners; // change listeners

	// widgets
	private JPanel panel;  // contains tabs/cards
	private JSCompactTabs tabs; // controlling tabs
	private JPanel cardP; // card content
	private CardLayout cardL; // card layout
	private StringList cardNames; // name of created cards

	// constructor
	public GCompactTabs(GNode g, PNamed p,
	String[] gnames, Class[] c, Icon[] icons) {
	    super(g, p);
	    classes = c;
	    items = new ArrayList<Item>();
	    currItem = stack = null;
	    cardNames = new StringList();
	    changeListeners = new ArrayList<ChangeListener>();

	    // enclosing panel
	    panel = new JPanel(null) {
		public void doLayout() { reconfig(); }
	    };
	    setJComp(panel);
	      
	    // tab widget
	    tabs = new JSCompactTabs(gnames, icons);
	    tabs.setSelectIcon(glook().selectIcon());
	    tabs.addCompactTabsListener(this);
	    setHelp(tabs, null); // for mouse motion delivery
	    panel.add(tabs);

	    // cards
	    cardL = new CardLayout();
	    cardP = new JPanel(cardL);
	    panel.add(cardP);
	}

	// project left side constructor
	public static GCompactTabs newProjectLeft(GNode g, PNamed p) {
	    String[] gnames = new String[] { 
		"Projects", "Models", "ParSets", 
		"DataSets", "Notes", "ImageSets" };
	    Class[] classes = new Class[] { 
		GTree.class, GModel.class, GParSet.class, 
		GDataSet.class, GNotes.class, GImageSet.class };
	    Icon[] icons = new Icon[] { 
		g.glook().projectIcon(), g.glook().modelIcon(), 
		g.glook().parsetIcon(), g.glook().datasetIcon(), 
		g.glook().notesIcon(), g.glook().imagesetIcon() };
	    GCompactTabs tabs = new GCompactTabs(g, p,
		gnames, classes, icons);
	    tabs.isLeft = true;
	    return tabs;
	}

	// project right side constructor
	public static GCompactTabs newProjectRight(GNode g, PNamed p) {
	    String[] gnames = new String[] { 
		"Messages", "Plot Pages", "Graphics" };
	    Class[] classes = new Class[] {
		GMessage.class, GPlotPage.class, GGraphicTab.class }; 
	    Icon[] icons = new Icon[] { 
		null, g.glook().plotpageIcon(),
		g.glook().graphicIcon() };
	    GCompactTabs tabs = new GCompactTabs(g, p,
		gnames, classes, icons);
	    return tabs;
	}

	// add a tab
  	public void addTab(String name, GNode gnode){

	    // determine group via classes table
	    int gx = 0;
	    while (gx<classes.length 
	    && !classes[gx].isInstance(gnode))
		gx++;
	    if (gx>=classes.length) gx = 0;
	    Item item = new Item(gx, name, gnode);
	    items.add(item);
	    tabs.addGroupItem(gx, name);

	    // add to JComponent tree so GMain.updateUI() works OK
	    if (gnode.jcomp() != null)
	    	tabs.add(gnode.jcomp());
	}

	// remove a tab
	public void removeTab(GNode gnode){
	    Item item = item(gnode);
	    if (item == null) {
		System.err.println("GCompactTabs can't remove " + gnode);
		return;
	    }
	    removeItem(gnode);
	    tabs.removeGroupItem(item.gx, item.item);
	    if (cardNames.contains(item.item)) {
		cardP.remove(gnode.jcomp());
		cardNames.remove(item.item);
	    }
	    if (item == currItem) {
		currItem = null;
		pushFirst();
	    }
	}

	// rename a tab
	public void renameTab(GNode gnode, String n) { 
	    Item item = item(gnode);
	    if (item == null) return;
	    removeTab(gnode);
	    addTab(n, gnode);
	}

	// reconfig
	private void reconfig() {
	    Dimension pdim = panel.getSize();
	    tabs.setFont(glook().bigFont());
	    tabs.setUnselectedColor(glook().bg_darker());
	    tabs.setLocation(new Point(0,0));
	    int tabH = tabs.getPreferredHeight();
	    tabs.setSize(pdim.width, tabH);
	    cardP.setLocation(new Point(0, tabH));
	    cardP.setSize(pdim.width, pdim.height-tabH);
	}

	// refresh this page
	public void refresh() {
	    if (isLeft) forceParsetIcon();
	    needsRefresh = false;
	    for (int i=0; i<items.size(); i++)
		item(i).gnode.needsRefresh = true;
	    if (currItem == null) return;
	    currItem.gnode.refresh();
//	    hackAccel();
	}

	// hack accelerator confusion (e.g. Find) by switching tabs
	private void hackAccel() {
	    GNode gnode = currItem.gnode;
	    if (! (gnode instanceof GModel)) return;
//	    System.err.println("Active gmodel");
	    JTabbedPane tabs = ((GModel) gnode).tabs();
	    int inx = tabs.getSelectedIndex();
	    int ainx = (inx==0) ? 1 : 0;
	    tabs.setSelectedIndex(ainx);
	    tabs.setSelectedIndex(inx);
	}

	// dump input map
	public String toString(JComponent j) {
	    InputMap map = j.getInputMap(
	    	JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
	    String s = "InputMap#" + map.hashCode();
	    InputMap p = map.getParent();
	    if (p != null) s = s + " parent=" + p.hashCode();
	    KeyStroke[] keys = map.keys();
	    if (keys == null) return s;
	    for (int i=0; i<keys.length; i++) 
	    	s = s + " " + keys[i];
	    return s;
	}
	
	// force parset icon
	private static final int MODELS_GROUP = 1;
	private static final int PARSETS_GROUP = 2;
	private void forceParsetIcon() {
	    if (currItem.gx != MODELS_GROUP) {
		tabs.unforceIcon();
		return;
	    }
	    PModel pmodel = ((GModel) currItem.gnode).pmodel();
	    String pset = pmodel.parSetName.stringVal();
	    tabs.forceIcon(PARSETS_GROUP, pset, 
		glook().parsetIcon(true));
	}

	// push/pop current page 
	public void pushFirst(){
	    if (items.size() == 0) return;
	    stack = item(0);
	    setSelected(stack);
	}
	public void pushLast(){ 
	    if (items.size() == 0) return;
	    stack = item(items.size()-1);
	    setSelected(stack);
	}
	public void pushNode(GNode gnode) {
	    if (items.size() == 0) return;
	    stack = item(gnode);
	    setSelected(stack);
	}
	public void pop(){ 
	    setSelected(stack);
	}

	// set the selected item
	private void setSelected(Item item) {
	    if (currItem != null && currItem.gnode != null)
		currItem.gnode.hidePopups();
	    if (item == null) return;
	    tabs.setSelection(item.gx, item.item);
	    currItem = item;
	    if (! cardNames.containSame(currItem.item)) {
		JComponent c = currItem.gnode.jcomp();
		cardP.add(c, currItem.item);
		cardNames.add(currItem.item);
	    }
	    cardL.show(cardP, currItem.item);
	    refresh();
	}

	// get selected node
	public GNode selectedNode() {
	    return (currItem == null) ? null : currItem.gnode;
	}

	// item list maintenance
	private Item item(int i) { return (Item) items.get(i); }
	private Item item(String s) {
	    for (int i=0; i<items.size(); i++) 
		if (item(i).item.equals(s))
		    return item(i);
	    return null;
	}
	private Item item(GNode g) {
	    for (int i=0; i<items.size(); i++) 
		if (item(i).gnode == g)
		    return item(i);
	    return null;
	}
	private void removeItem(GNode g) {
	    for (int i=0; i<items.size(); i++) {
		if (item(i).gnode != g) continue;
		items.remove(i);
		return;
	    }
	    System.err.println("GCompactTabs.removeItem failed for "
		+ g);
	}

	// listeners
	public void tabItemSelected(JSCompactTabs tabs) {	    
	    setSelected(item(tabs.getSelectedItem()));
	    for (int i=0; i<changeListeners.size(); i++) {
		ChangeListener l = (ChangeListener)
		    changeListeners.get(i);
		l.stateChanged(new ChangeEvent(tabs));
	    }
	}
	public void tabMoused(JSCompactTabs jcomp) {
	    ghelp().showHelp(tabs, tabs.getMousedRectangle(), this);
	}
	public void addChangeListener(ChangeListener l){ 
	    changeListeners.add(l);
	}
	public void lookUpdated() { reconfig(); }

	// help key
	public String helpKey() {
	    if (tabs.getMousedItem() == null)
		return null;
	    Class c = classes[tabs.getMousedGroup()];
	    return "tab" + GHelp.sep + GHelp.shortClassName(c);
	}

	// Item class
	private class Item {
	    public int gx; // group index
	    public String item; // item name
	    public GNode gnode; // corresponding GNode

	    // constructor
	    public Item(int x, String n, GNode g) {
		gx = x;
		item = n;
		gnode = g;
	    }
	    public String toString() {
		return "" + gx + ":" + item + ":" + gnode;
	    }
	}
}
