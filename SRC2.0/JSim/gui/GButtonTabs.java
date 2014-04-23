/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Button-based project tabs

package JSim.gui;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;

public class GButtonTabs extends GAbstractTabs 
implements ActionListener, ComponentListener {
	private GButtonTabs self;  // for 
	private int n; // number of buttons
	private ArrayList<Group> groups; // groups = menus = classes
	private ArrayList<Item> allItems; // flat list of items
	private Item currItem, stack; // current page & stack

	// widgets
	private JPanel panel;  // contains tabs/cards
	private GridLayout buttonLayout; // button layout
	private JPanel buttonPanel; // button panel
	private CardLayout cardLayout; // card layout
	private JPanel cardPanel; // card content

	// layout parms
	private static final Insets buttonInsets = new Insets(0,0,0,0);

	// constructor
	public GButtonTabs(GNode g, PNamed p, String[] groupNames,
	Class[] classes) {
	    super(g, p);
	    self = this;
	    n = groupNames.length;
	    panel = new JPanel(new BorderLayout());
	    setJComp(panel);
	    groups = new ArrayList<Group>();
	    allItems = new ArrayList<Item>();
	    for (int i=0; i<groupNames.length; i++) 
	    	groups.add(new Group(groupNames[i], classes[i]));
	    buttonLayout = new GridLayout(1, 1);
	    buttonPanel = new JPanel(buttonLayout);
	    cardLayout = new CardLayout();
	    cardPanel = new JPanel(cardLayout);
	    panel.add(cardPanel, BorderLayout.CENTER);
	    panel.add(buttonPanel, BorderLayout.NORTH);
	    panel.addComponentListener(this);
	}

	// layout buttonPanel w/ items or groups depending upon size
	private void redoTabs() {
	    Dimension currDim = panel.getSize();
	    if (currDim.width == 0) return;
	    for (int i=0; i<groups.size(); i++)
	    	groups.get(i).menu = null;
	    redoItemTabs();
	    Dimension prefDim = buttonPanel.getPreferredSize();
	    if (prefDim.width > currDim.width)
	    	redoGroupTabs();
//	    buttonPanel.validate();
	    parent().jcomp().validate(); // ensures height correct
	}

	// one button per item
	private void redoItemTabs() {    
	    buttonPanel.removeAll();
	    for (int gx=0; gx<groups.size(); gx++) {
		Group g = groups.get(gx);
		for (int ix=0; ix<g.items.size(); ix++) 
		    addItemButton(g.items.get(ix));
	    }
	    int ncols = buttonPanel.getComponentCount();
	    if (ncols == 1) { // cosmetic placeholder
	        JButton b = new JButton("");
		b.setEnabled(false);
	        buttonPanel.add(b);
	    }
	    if (ncols < 2) ncols += 1;
	    buttonLayout.setColumns(ncols);
	}

	// one button per group
	private void redoGroupTabs() {
	    buttonPanel.removeAll();
	    for (int gx=0; gx<groups.size(); gx++) {
		Group g = groups.get(gx);
		if (g.items.size() == 0) continue;
		if (g.items.size() == 1) 
		    addItemButton(g.items.get(0));
		else
		    addGroupButton(g);
	    }
	    int ncols = buttonPanel.getComponentCount();
	    if (ncols < 2) ncols += 1;
	    buttonLayout.setColumns(ncols);
	}
	
	// add item button to buttonPanel
	private void addItemButton(Item item) {
	    buttonPanel.add(item.button);
	    Color c = (item == currItem) ?
		glook().bg() : glook().bg_darker();
	    item.button.setBackground(c);
	}

	// add group button to buttonPanel
	private void addGroupButton(Group group) {
	    buttonPanel.add(group.button);
	    Color c = (currItem != null && group == currItem.group) ?
		glook().bg() : glook().bg_darker();
	    group.button.setBackground(c);
	    group.button.setText(group.name + " (" + 
	    	group.items.size() + ")");
	}

	// add a tab
  	public void addTab(String name, GNode gnode) {
	    Group group = group(gnode);
	    if (group == null) {
	    	System.err.println(
		    "GButtonTabs.addTabs(): no group for " + gnode);
		return;
	    }
	    Item item = new Item(name, gnode);
	    redoTabs();
	}

	// remove a tab
	public void removeTab(GNode gnode) {
	    Item item = item(gnode);
	    if (item == null) {
	    	System.err.println(
		    "GButtonTabs.removeTab(): no item for " + gnode);
		return;
	    }
	    allItems.remove(item);
	    if (item.group != null)  // null check s/b unnecessary
	    	item.group.items.remove(item);
	    if (item == currItem) {
		currItem = null;
		pushFirst();
	    } else {
	    	redoTabs();
	    }
	}

	// rename a tab
	public void renameTab(GNode gnode, String n) {
	    Item item = item(gnode);
	    if (item == null) return;
	    item.rename(n);
	    redoTabs();
	}

	// push/pop current page 
	public void pushFirst() {
	    if (allItems.size() == 0) return;
	    Item item = allItems.get(0);
	    setSelected(item);
	}
	public void pushLast(){ 
	    if (allItems.size() == 0) return;
	    Item item = allItems.get(allItems.size()-1);
	    setSelected(item);
	}
	public void pushNode(GNode gnode) {
	    Item item = item(gnode);
	    setSelected(item);
	}
	public void pop(){ 
	    setSelected(stack);
	}

	// set selected item
	private void setSelected(Item item) {
	    if (currItem == item || item == null) return;
	    if (currItem != null && currItem.gnode != null)
		currItem.gnode.hidePopups();
	    stack = currItem;
	    currItem = item;

	    // update cardPanel
	    if (item.cardID == null) {
	        item.cardID = "" + item.gnode.hashCode();
	    	JComponent jc = item.gnode.jcomp();
		cardPanel.add(jc, item.cardID);
	    }
	    cardLayout.show(cardPanel, item.cardID);
	    refresh();
	}

		
	// get selected node
	public GNode selectedNode() {
	    if (currItem == null) return null;
	    return currItem.gnode;
	}

	// refresh this page
	public void refresh() {
	    needsRefresh = false;
	    redoTabs();
	    for (int i=0; i<allItems.size(); i++)
		allItems.get(i).gnode.needsRefresh = true;
	    if (currItem == null) return;
	    currItem.gnode.refresh();
	}

	// action event
	public void actionPerformed(ActionEvent event) {
	    try {
	    	JComponent jcomp = (JComponent) (event.getSource());
		Item item = item(jcomp);
		if (item != null)
		    setSelected(item);
		Group group = group(jcomp);
		if (group != null)
		    showMenu(group);
	    } catch (Exception e) {
	    }	
	}

	// show menu for group
	private void showMenu(Group group) {

	    // create menu if redoTabs has been called
	    if (group.menu == null) {
	    	group.menu = new JPopupMenu();
		group.menu.setInvoker(group.button);
		for (int i=0; i<group.items.size(); i++) {
		    Item item = group.items.get(i);
		    item.radio = new JRadioButtonMenuItem(item.name);
		    group.menu.add(item.radio);
		    item.radio.addActionListener(this);
		}
	    }

	    // set selected item, if any
	    for (int i=0; i<group.items.size(); i++) {
		Item item = group.items.get(i);
		item.radio.setSelected(item == currItem);
	    }
	    
	    // make visible under group.button
	    Dimension dim = group.button.getSize();
	    group.menu.show(group.button, 0, dim.height);
	}

	// ComponentListener methods
	public void componentHidden(ComponentEvent e) { }
	public void componentMoved(ComponentEvent e) { }
	public void componentResized(ComponentEvent e) {
	    redoTabs(); 
	}
	public void componentShown(ComponentEvent e) { }

	// Group class
	private class Group {
	    public String name;
	    public Class gclass;
	    public Icon icon;
	    public ArrayList<Item> items;
	    public JButton button; // tab button
	    public JPopupMenu menu;  // popup menu
	    public ButtonGroup bgroup; // for radio selection
	    
	    // constructor
	    public Group(String name, Class c) {
	        this.name = name;
		gclass = c;
		items = new ArrayList<Item>();
		icon = glook().tabIcon(gclass);
		button = new JButton(name, icon);
		button.setMargin(buttonInsets);
		button.setFont(glook().bigFont());
		button.addActionListener(self);
	    }

	}

	// Item class
	private class Item {
	    public String name; // item name
	    public GNode gnode; // corresponding GNode
	    public Group group; // is in this group
	    public String cardID; // in cardPanel
	    public JButton button; // tab button
	    public JRadioButtonMenuItem radio; // radio button

	    // constructor
	    public Item(String n, GNode g) {
		name = n;
		gnode = g;
		group = group(gnode);
		allItems.add(this);
		group.items.add(this);
		button = new JButton(name, group.icon);
		button.setMargin(buttonInsets);
		button.setFont(glook().bigFont());
		button.addActionListener(self);
	    }

	    // rename
	    public void rename(String n) { 
	    	name = n;
		button.setText(name);
		if (radio != null) 
		    radio.setText(name);
	    }

	    // query
	    public String toString() {
	    	return name + ":" + gnode.getClass();
	    }
	}

	//// Group/Item utilities

	// find group for a gnode
	private Group group(GNode gnode) {
	    for (int i=0; i<groups.size(); i++) {
	    	if (groups.get(i).gclass == gnode.getClass())
		    return groups.get(i); }
	    return null;
	}

	// find existing item for a gnode
	private Item item(GNode gnode) {
	    for (int i=0; i<allItems.size(); i++) 
	    	if (allItems.get(i).gnode == gnode)
		    return allItems.get(i);
	    return null;
	}

	// find item for JComponent callback
	private Item item(JComponent jcomp) {
	    for (int i=0; i<allItems.size(); i++) {
	    	Item item = allItems.get(i);
	    	if (item.button == jcomp || item.radio == jcomp)
		    return item;
	    }
	    return null;
	}

	// find group for JComponent callback
	private Group group(JComponent jcomp) {
	    for (int i=0; i<groups.size(); i++)
	    	if (groups.get(i).button == jcomp)
		    return groups.get(i);
	    return null;
	}

}
