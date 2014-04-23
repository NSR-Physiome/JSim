/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model optimization report viewer in GUI

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelOptimReport extends GEditor {

	// state
	private JLabel l_hdr;
	private JScrollBar vscroll;
	private JTextArea text;

	// constructor from PNotes
	public GModelOptimReport(GModelOptim g) {
	    super(g, g.gmodel().pmodel());

	    // create widgets
	    JRootPane root = new JRootPane();
	    text = new JTextArea();
	    text.setBackground(glook().dark());
	    text.setEditable(false);
	    setIndentedBorder(text);
	    JScrollPane scroll = new JScrollPane(text);
	    vscroll = scroll.getVerticalScrollBar();
	    root.getContentPane().add(scroll);
	    setJComp(root);

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(exportText.item());
	    menu.add(printText.item());
	    mbar.add(menu);

	    menu = newMenu("Edit");
	    menu.add(copy.item());
	    menu.add(gotoLine.item());
	    menu.add(findText.item());
	    menu.add(findAgain.item());
	    mbar.add(menu);

	    // run button / menu
	    mbar.add(gmodel().optimRun.button("Run"));
	    gmodel().addRunsMenu(mbar);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);

	}

	// query
	public JTextArea text() {  return text; }
	public String title() {
	    return gmodel().title() + " Optimization Report";
	}

	// refresh: scroll/caret positioning
	//   if at top -> stay at top; if at bottom -> stay at bottom
	public void refresh() {
	    setTabLabel(l_hdr);
	    OptimResults results;
	    results = gmodel().pmodel().rt().optimResults();
	    String s = "no report available";
	    if (results != null) {
	        try {
	    	    OptimReport rpt = new OptimReport(results,
		        server().optimAlgs());
		    s = rpt.getReport();
		} catch (Xcept e) {
		    s = s + "\n" + e;
		}
	    }

	    // save figure current position as fraction 
	    int oval = vscroll.getValue();
	    int oamt = vscroll.getVisibleAmount();
	    int omax = vscroll.getMaximum();
	    float frac = (oval - 0f) / (omax - oamt);

	    // update text
	    text().setText(s);

	    // update scroll value, caret position based on fraction
	    int nsiz = s.length();
	    int nmax = vscroll.getMaximum();
	    int npos = (int) (frac * nsiz);
	    int nval = (int) (frac * nmax);
	    vscroll.setValue(nval);
	    text().setCaretPosition(npos);

	    super.refresh();
	}
}
