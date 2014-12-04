/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Model source editor

package JSim.gui.model;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelSrc extends GEditor {

	// state
	private JLabel l_hdr;
	private TextControl cntl;
	private GTextControl gcntl;
	private GModelBuildOptions goptions;

	// actions
	public GAction compile;
	public GAction showOptions;

	// constructor
	public GModelSrc(GNode p, TextControl c) {
	    super(p, c);
	    cntl = c;

	    // create widgets
	    JRootPane root = new JRootPane();
	    gcntl = new GTextControl(this, cntl) {
	    	public void setText(String s) {
		    super.setText(s);
		    resetUndo();
		}
	    };
	    root.getContentPane().add(gcntl.jcomp());
	    setJComp(root);

	    // create actions
	    compile = new GAction(this, "Compile") {
		public void doit() throws Xcept {
		    gcntl.update(); // needed for Cntl-K
	    	    gnode.gmain().runJob(
			new GModelJob.Build(gnode.gmodel(), false));
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };
	    compile.setAccel('K');

	    // create actions
	    showOptions = new GAction(this, "Compiler options...") {
	        public void doit() throws Xcept {
		    gcntl.update(); // following above, needed?
		    GModel gmodel = gnode.gmodel();
		    if (goptions == null)
		    	goptions = new GModelBuildOptions(
			    gmodel, gmodel.pmodel());
		    goptions.show();
		}
	    };	       	   
	    showOptions.setAccel('O');

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(importClearText.item());
	    menu.add(importInsertText.item());
	    menu.add(exportText.item());
	    menu.addSeparator();
	    menu.add(printText.item());
	    mbar.add(menu);

	    menu = newMenu("Edit");
	    menu.add(undo.item());
	    menu.add(redo.item());
	    menu.add(cut.item());
	    menu.add(copy.item());
	    menu.add(paste.item());
	    menu.addSeparator();
	    menu.add(gotoLine.item());
	    menu.add(findText.item());
	    menu.add(findAgain.item());
	    menu.addSeparator();
	    menu.add(compile.item());
	    menu.add(showOptions.item());
	    mbar.add(menu);

	    mbar.add(compile.button());

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);
	    enableLineUpdate();
	}

	    
	// set caret position after compile error
	public static void setCaret(GModelJob gjob) {
	    try {
		PModelBuildJob job = (PModelBuildJob) gjob.pjob();
		XceptPos pos = job.mmlPos();
		if (pos.filename != null) return;
		if (pos.charno < 0) return;
		GModel gmodel = gjob.gmodel;
		JTextArea txt = gmodel.gsrc().text();
		txt.requestFocus();
		txt.setCaret(txt.getCaret()); // otherwise disappears!!
		txt.setCaretPosition(pos.charno);
	    } catch (Exception e) {
		// nothing useful for user,  print here if debugging
	    } 
	}
	    
	// refresh
	public void refresh() {
	    compile.setEnabled(); // compile button
	    setTabLabel(l_hdr);
	    super.refresh();
	}

	// query
	public JTextArea text() {
	    return (JTextArea) gcntl.text();
	}
	public GTextControl gcntl() { return gcntl; }
	public String title() {
	    return gmodel().title() + " Source Code";
	}
	    
}
