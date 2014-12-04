/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model monte-carlo report viewer in GUI

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelMonteReport extends GEditor {

	// state
	private PModelMonte pmonte;
	private JLabel l_hdr;
	private JPanel cpanel; // controls (which opt# if detail)
	private GControl goptn; // which opt# control
	private JScrollBar vscroll;
	private JTextArea text;
	private int view;  // 0=summary, 1=detail
	public GAction a_view, exportAll;

	// view
	public static final int SUMMARY=0;
	public static final int DETAIL=1;
	public static String[] viewNames = new String[] {
	    "Summary", "Detail" 
	};
	

	// constructor from PNotes
	public GModelMonteReport(GModelMonte g) {
	    super(g, g.gmodel().pmodel());
	    pmonte = g.gmodel().pmodel().monte();

	    // export all 
	    exportAll = new GAction(this, "Export all optim reports...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.TEXT);
		    if (info == null) return;
		    File f = info.file;
		    UtilIO.writeText(f, getAllDetails());
		}
	    };

	    // create widgets
	    JRootPane root = new JRootPane();
	    JPanel panel = new JPanel(new BorderLayout());
	    root.setContentPane(panel);
	    setJComp(root);

	    // control panel
	    cpanel = new JPanel(new GridLayout(1,4));
	    cpanel.add(new JLabel("opt# ", SwingConstants.RIGHT));
	    goptn = GControl.create(this, pmonte.graphOptNo);
	    goptn.addAuxNode(this);
	    cpanel.add(goptn.jcomp());
	    cpanel.add(new JLabel(""));
	    cpanel.add(new JLabel(""));
	    panel.add(cpanel, BorderLayout.NORTH);

	    // text area
	    text = new JTextArea();
	    text.setBackground(glook().dark());
	    text.setEditable(false);
	    setIndentedBorder(text);
	    JScrollPane scroll = new JScrollPane(text);
	    vscroll = scroll.getVerticalScrollBar();
	    panel.add(scroll, BorderLayout.CENTER);

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(exportText.item());
	    menu.add(printText.item());
	    menu.addSeparator();
	    menu.add(exportAll.item());
	    mbar.add(menu);

	    menu = newMenu("Edit");
	    menu.add(copy.item());
	    menu.add(gotoLine.item());
	    menu.add(findText.item());
	    menu.add(findAgain.item());
	    mbar.add(menu);

	    menu = newMenu("View");
	    ButtonGroup group = new ButtonGroup();
	    for (int i=0; i<viewNames.length; i++) {
		GAction gact = new GViewAction(this, 
		   viewNames[i], i);
	    	JRadioButtonMenuItem b_view = gact.radio();
		b_view.setSelected(i==0);
	    	menu.add(b_view);
	    	group.add(b_view);
	    }
	    mbar.add(menu);
	    
	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);

	}

	// query
	public JTextArea text() {  return text; }
	public String title() {
	    return gmodel().title() + " Monte Carlo Report";
	}

	// refresh: scroll/caret positioning
	//   if at top -> stay at top; if at bottom -> stay at bottom
	public void refresh() {
	    setTabLabel(l_hdr);
	    cpanel.setVisible(view == DETAIL);
	    String txt = "no report available";
	    try {
	    	String s = (view == SUMMARY) ?
	    	    getSummary() : getDetail();
		if (! Util.isBlank(s)) txt = s;
	    } catch (Xcept e) {
		txt = txt + "\n" + e;
	    }
	    text().setText(txt);
	    text().setCaretPosition(0);
	    super.refresh();
	}
	
	// get summary report
	private String getSummary() throws Xcept {
	    PModel pmodel = gmodel().pmodel();
	    MoptData moptData = pmodel.rt().getMoptData();
	    if (moptData == null) return null;
	    PModelMonteReport rpt = 
		new PModelMonteReport(pmonte,
		moptData);
	    return rpt.getReport();
	}
	
	// get detail report
	private String getDetail() throws Xcept {
	    PModel pmodel = gmodel().pmodel();
	    MoptData moptData = pmodel.rt().getMoptData();
	    if (moptData == null) return null;
	    int optn = pmonte.graphOptNo.val();
	    OptimResults res = moptData.optimResults(optn-1);
	    if (res == null) return null;
	    StringBuffer buf = new StringBuffer();
	    buf.append("==== Optimization #" + optn + "\n");
	    OptimReport rpt = new OptimReport(
		res, pmodel.server().optimAlgs());
	    buf.append(rpt.getReport());
	    return buf.toString();
	}
	    	
	// write all details to file report
	private String getAllDetails() throws Xcept {
	    PModel pmodel = gmodel().pmodel();
	    StringBuffer buf = new StringBuffer();
	    MoptData moptData = pmodel.rt().getMoptData();
	    if (moptData == null) return null;
	    for (int i=0; i<moptData.nsegments(); i++) {
	    	OptimResults res = moptData.optimResults(i);
		if (res == null) continue;
		buf.append("==== Optimization #" + (i+1) + "\n");
	    	OptimReport rpt = new OptimReport(
		    res, pmodel.server().optimAlgs());
		buf.append(rpt.getReport());
	    }
	    return buf.toString();
	}
	    	
	// view selection Action
	public class GViewAction extends GAction {
	    private int w; // PM
	    public GViewAction(GNode n, String s, int ww) {
		super(n, s);
		w = ww;
	    }
	    public void doit() throws Xcept {
		view = w;
		refresh();
	    }
	}
}
