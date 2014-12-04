/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// GRT document,  set of pages and menus

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;
import JSim.gui.model.*;

public class GRTDoc extends GRTNode{

	// state
	private GModelPars gpars;
	protected NamedList pages; 
	protected ArrayList<JComponent> pageItems; 
	protected ArrayList<JComponent> menuItems;

	// blank constructor
	protected GRTDoc(GModelPars p) {
	    super(p);
	    gpars = p;
	    clear();
	}

	// document constructor
	public GRTDoc(GModelPars p, Element e) {
	    this(p);
	    reload(e);
	}

	// clear out
	protected void clear() {
	    pages = new NamedList(8);
	    pageItems = new ArrayList<JComponent>(8);
	    menuItems = new ArrayList<JComponent>(8);
	}

	// clear and reload XML 
	protected void reload(Element e) {
	    clear();
	    addXMLChildren(e);

	    // flat or nested menus
	    boolean flat = true;
	    for (int i=0; i<nChild(); i++) {
		if (child(i) instanceof GRTMenu) {
		    flat = false;
		    break;
		}
	    }
	    if (flat) 
		makeFlatMenu();
	    else 	
		makeNestedMenu();
	}
	
	// create child from XML Element
	public void addXMLChild(Element e) {
	    String n = e.getNodeName();
	    if (n.equals("color")) {
		new GRTColor(this, e);
	    } else if (n.equals("page")) {
		GRTPage p = new GRTPage(this, e);
		pages.add(p);
	    } else if (n.equals("menu")) { 
		new GRTMenu(this, e);
	    } else 
		super.addXMLChild(e);
	}

	// create flat pages menu
	private void makeFlatMenu() {
	    for (int i=0; i<pages.size(); i++) {
		GRTPage page = (GRTPage) pages.get(i);
		GAction a = new GRTPage.PageAction(
		    this, page.name());
		JMenuItem item = a.item();
		pageItems.add(item);
	        menuItems.add(item);
	    }
	}

	// create nested pages menu
	private void makeNestedMenu() {
	    for (int i=0; i<nChild(); i++) {
		if (! (child(i) instanceof GRTMenu))
			continue;
		GRTMenu menu = (GRTMenu) child(i);
		menu.addItems();
	    }
	}

	// query
	public int npages() {
	    return pages.size();
	}
	public int nmenuItems() {
	    return menuItems.size(); 
	}
	public GRTPage page(int i) {
	    if (i>pages.size() || i<0) return null;
	    return (GRTPage) pages.get(i);
	}
	public GRTPage page(String name) {
	    return (GRTPage) pages.getByName(name);
	}
	public int pageNo(String name) {
	    for (int i=0; i<pages.size(); i++) 
		if (page(i).name().equals(name))
		    return i;
	    return -1;
	}
	public ArrayList<JComponent> menuItems() { return menuItems; }
	public ArrayList<JComponent> pageItems() { return pageItems; }
	public GModelPars gpars() { return gpars; }
}

 
