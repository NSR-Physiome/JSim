/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// replacement for JPopupMenu allowing scrollbar

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;

abstract public class GPopupMenu {
	// constructor
	public GPopupMenu() { }

	// create
	public static GPopupMenu create(GNode n, boolean scroll) {
	    if (! scroll)
		return new Plain(n);
		
	    // hack for pre Java 1.5 applets
	    boolean hack = ! Util.isJava15();
	    if (hack && n != null) hack = n.isApplet();
	    if (hack)
	    	return new ScrollHack(n);
	    else
	    	return new Scroll(n);
	}

	// abstracts
	abstract public void setVisible(boolean b);
	abstract public void add(JMenuItem item);
	abstract public void show(JComponent j, int x, int y);
	abstract public boolean isVisible();

	// Plain class wraps JPopupMenu
	public static class Plain extends GPopupMenu {
	    private JPopupMenu menu;

	    // constructor
	    public Plain(GNode n) {
		super();
		menu = new JPopupMenu();
	    }

	    // pass methods on to menu
	    public void add(JMenuItem item) { menu.add(item); }
	    public void setVisible(boolean b) { menu.setVisible(b); }
	    public boolean isVisible() { return menu.isVisible(); }
	    public void show(JComponent jcomp, int x, int y) {
		menu.show(jcomp, x, y);
	    }
	}

	// Scrollable class
	public static class Scroll extends GPopupMenu
	implements ActionListener {
	    private JWindow win;
	    private JScrollPane scroll;
	    private JPanel panel;
	    private int ct;
	    private int xmax;
	    private int ymax;

	    // constructor
	    public Scroll(GNode n) {
	    	super();
	    	panel = new JPanel();
	    	scroll = new JScrollPane(panel);
		Window pwin = 
		    SwingUtilities.windowForComponent(n.jcomp());
		win = new JWindow(pwin);
	    	win.setContentPane(scroll);
	    	xmax = 100;
	    	ymax = 15;
	    }

	    // add menu item
	    public void add(JMenuItem item) {
	   	panel.add(item);
		item.addActionListener(this);
	    	Dimension dim = item.getPreferredSize();
	    	if (dim.width>xmax) xmax = dim.width;
	    	if (dim.height>ymax) ymax = dim.height;
	    	ct++;
	    }

	    // action performed,  popdown window
	    public void actionPerformed(ActionEvent e) {
		setVisible(false);
	    }

	    // visible
	    public void setVisible(boolean b) {	
		win.setVisible(b); 
	    }
	    public boolean isVisible() { return win.isVisible(); }

	    // show menu
	    public void show(JComponent jcomp, int x, int y) {
	    	panel.setLayout(new GridLayout(ct, 1));
		Point p = GUtil.getLocationOnScreen(jcomp);
	    	int vct = Math.min(15, ct);
	    	win.setBounds(p.x+x, p.y+y, 
		    xmax+15, (ymax+5)*vct);
	    	setVisible(true);
	    }
	}

	// Scrollable class hack for Java 1.4 Applets
	//    works around window ancestor bug that forces
	//    created JWindows above to display underneath browser
	public static class ScrollHack extends GPopupMenu
	implements ActionListener {
	    private JDialog jdialog;
	    private JScrollPane scroll;
	    private JPanel panel;
	    private int ct;
	    private int xmax;
	    private int ymax;

	    // constructor
	    public ScrollHack(GNode n) {
	    	super();
	    	panel = new JPanel();
	    	scroll = new JScrollPane(panel);
		jdialog = new JDialog(n.frame());
		jdialog.setContentPane(scroll);
	    	xmax = 100;
	    	ymax = 15;
	    }

	    // add menu item
	    public void add(JMenuItem item) {
	   	panel.add(item);
		item.addActionListener(this);
	    	Dimension dim = item.getPreferredSize();
	    	if (dim.width>xmax) xmax = dim.width;
	    	if (dim.height>ymax) ymax = dim.height;
	    	ct++;
	    }

	    // action performed,  popdown window
	    public void actionPerformed(ActionEvent e) {
		setVisible(false);
	    }

	    // visible
	    public void setVisible(boolean b) {	
		jdialog.setVisible(b); 
	    }
	    public boolean isVisible() { return jdialog.isVisible(); }

	    // show menu
	    public void show(JComponent jcomp, int x, int y) {
	    	panel.setLayout(new GridLayout(ct, 1));
		Point p = GUtil.getLocationOnScreen(jcomp);
	    	int vct = Math.min(15, ct);
	    	jdialog.setBounds(p.x+x, p.y+y, 
		    xmax+15, (ymax+5)*(vct+2));
	    	setVisible(true);
	    }
	}
}

