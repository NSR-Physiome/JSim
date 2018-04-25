/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// notes editor

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GSemSim extends GEditor {

	// state
	private JLabel l_hdr;
	private SemSimControl cntl;
	private GSemSimControl gcntl;

	// constructor from PNotes
	public GSemSim(GNode p, PSemSim n) {
	    super(p, n);
	    cntl = n.text;
	    common();

	    // add to project
	    gproject().addTab(this);
	}

	// constructor
	public GSemSim(GNode p, SemSimControl c) {
	    super(p, c);
	    cntl = c;
	    common();
	}

	// common constructor code
	private void common() {

	    // create widgets
	    JRootPane root = new JRootPane();
	    gcntl = new GSemSimControl(this, cntl);
	    ((JTextArea) gcntl.text()).setLineWrap(true);
	    root.getContentPane().add(gcntl.jcomp());
	    setJComp(root);

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(importClearText);
	    menu.add(importInsertText);
	    menu.add(exportText);
	    menu.addSeparator();
	    menu.add(printText);
	    mbar.add(menu);

	    menu = newMenu("Edit");
	    menu.add(cut.item());
	    menu.add(copy.item());
	    menu.add(paste.item());
	    menu.addSeparator();
	    menu.add(gotoLine.item());
	    menu.add(findText.item());
	    menu.add(findAgain.item());
	    mbar.add(menu);

	    root.setJMenuBar(mbar);
	    enableLineUpdate();
	}

	// refresh
	public void refresh() {
	    setTabLabel(l_hdr);
	    gcntl.text().setText(cntl.stringVal());
	    super.refresh();
	}
	    
	// query
	public JTextArea text() {
	    return (JTextArea) gcntl.text();
	}
	public GSemSimControl gcntl_semsim() { return gcntl; }
	public String title() { 
	    return gproject().title() + " Notes";
	}
	public boolean editable() { return true; }
}
