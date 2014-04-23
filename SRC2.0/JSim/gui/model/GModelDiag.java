/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Model diagnostics

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;
import JSim.aserver.*;

public class GModelDiag extends GEditor {

	// state
	private JButton b_warns;
	private JTextArea text;
	private int which;  // ASModel.TEXT_*, TEXT_PLAN, or -1 
	private JLabel l_hdr;

	// CONSTANTS
	public static final int TEXT_NSRUNIT = 1234;
	public static final int TEXT_NSRUNITCGS = 1235;	
	public static final int TEXT_PROFILE = 1236;	
	
	// actions
	public GAction showWarnings;
	public GAction viewSIUnits, viewCGSUnits, viewMML, viewFlat, viewJava,
	    viewPlan, viewMathML, viewXMML, viewGraphML, viewSBML,
	    viewAntimony, viewCellML, viewMatlab, viewProfile;

	// constructor
	public GModelDiag(GModel gmodel) {
	    super(gmodel, gmodel.pmodel());
	    which = -1;	

	    // create actions
	    showWarnings = new GAction(this, "Show Warnings") {
	    	public void doit() throws Xcept {
	 	    String[] warns =
		    gmodel().pmodel().rt().getTextWarnings(which, null);
	    	    gproject().messages(warns);
		    gproject().pushMessages();
		}
	    };
	    viewSIUnits = new GViewAction(this, 
		"View standard SI units file", TEXT_NSRUNIT);		
	    viewCGSUnits = new GViewAction(this, 
		"View standard CGS units file", TEXT_NSRUNITCGS);		
	    viewMML = new GViewAction(this, 
		"View MML text", ASModel.TEXT_MML);
	    viewFlat = new GViewAction(this, 
		"View flat MML text", ASModel.TEXT_FLATMML);
	    viewJava = new GViewAction(this, 
		"View java text", ASModel.TEXT_JAVA);
	    viewPlan = new GViewAction(this, 
		"View plan text", ASModel.TEXT_PLAN);
	    viewMathML = new GViewAction(this, 
		"View MathML", ASModel.TEXT_MATHML);
	    viewXMML = new GViewAction(this, 
		"View XMML", ASModel.TEXT_XMML);
	    viewGraphML = new GViewAction(this, 
		"View GraphML", ASModel.TEXT_GRAPHML);
	    viewSBML = new GViewAction(this, 
		"View SBML", ASModel.TEXT_SBML);
	    viewAntimony = new GViewAction(this, 
		"View Antimony", ASModel.TEXT_ANTIMONY);
	    viewCellML = new GViewAction(this, 
		"View CellML", ASModel.TEXT_CELLML);
	    viewMatlab = new GViewAction(this, 
		"View Matlab", ASModel.TEXT_MATLAB);
	    viewProfile = new GViewAction(this, 
		"View Runtime Profile", TEXT_PROFILE);

	    // create widgets
	    JRootPane root = new JRootPane();
	    JPanel gwarns = new JPanel();
	    b_warns = new JButton(showWarnings);
	    gwarns.add(b_warns);
	    text = new JTextArea();
	    text.setBackground(glook().dark());
	    text.setEditable(false);
	    setIndentedBorder(text);
	    JScrollPane scroll = new JScrollPane(text);
	    JPanel jpanel = new JPanel(new BorderLayout());
	    jpanel.add(gwarns, BorderLayout.NORTH);
	    jpanel.add(scroll, BorderLayout.CENTER);
	    root.getContentPane().add(jpanel);
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
	    menu.add(gotoLine.item());
	    menu.add(findText.item());
	    menu.add(findAgain.item());
	    mbar.add(menu);

	    menu = newMenu("View");
	    ButtonGroup group = new ButtonGroup();
	    JRadioButtonMenuItem b_view;
	    b_view = viewSIUnits.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewCGSUnits.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewPlan.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewMML.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewFlat.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewJava.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewMathML.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewXMML.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewGraphML.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewSBML.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewAntimony.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewCellML.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    b_view = viewMatlab.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    menu.addSeparator();
	    b_view = viewProfile.radio();
	    menu.add(b_view);
	    group.add(b_view);
	    mbar.add(menu);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);
	    enableLineUpdate();
	}	    

	// query
	public JTextArea text() { return text; }
	public String title() {
	    switch (which) {
	    case ASModel.TEXT_MML:
		return gmodel().title() + " MML source code";
	    case ASModel.TEXT_FLATMML:
		return gmodel().title() + " flattened MML source code";
	    case ASModel.TEXT_JAVA:
		return gmodel().title() + " Java source code";
	    case ASModel.TEXT_PLAN:
		return gmodel().title() + " Planner Synopsis";
	    case TEXT_NSRUNIT:
		return "JSim Standard SI Units File";
	    case TEXT_NSRUNITCGS:
		return "JSim Standard CGS Units File";
	    case TEXT_PROFILE:
		return "JSim RunTime Profile";
	    }
	    return gmodel().title();
	}

	// refresh
	public void refresh() {
	    setTabLabel(l_hdr);
	    String s = "???";
	    try {
	    	if (which<0) {
		    s = "no view selected";
	    	} else if (which == TEXT_NSRUNIT) {
		    s = server().getCommonText("nsrunit.mod");
	    	} else if (which == TEXT_NSRUNITCGS) {
		    s = server().getCommonText("nsrunitcgs.mod");
	    	} else if (which == TEXT_PROFILE) {
		    s = getProfileText();
	    	} else {
		    s = gmodel().pmodel().rt().getText(which, null); 
	    	} 
	    } catch (Xcept e) {
	    	s = e.cleanMessage();
	    }
	    text.setText(s);
	    text.setCaretPosition(0);

	    b_warns.setVisible(false);
	    try {
	    	String[] warns =
		    gmodel().pmodel().rt().getTextWarnings(which, null);
		if (warns != null && warns.length > 0) {
		    b_warns.setVisible(true);
		    b_warns.setText("Show  " + 
		    	warns.length + " translation warning(s)");
		}
	    } catch (Xcept e) {
	    	e.printStackTrace();
	    }		    

	    super.refresh();
	}

	// view selection Action
	public class GViewAction extends GAction {
	    private int w; // PM
	    public GViewAction(GNode n, String s, int ww) {
		super(n, s);
		w = ww;
	    }
	    public void doit() throws Xcept {
		which = w;
		refresh();
	    }
	}

	// get profile text
	public String getProfileText() {
	    try {
	    	ProfileData pdata = gmodel().pmodel().rt().getProfile();
		if (pdata == null) return 
		    "No RunTime profile is currently available";
	    	ProfileReport prpt = new ProfileReport(pdata);
	    	return prpt.getReport();
	    } catch (Exception e) {
	    	return "Error generating RunTime profile:\n" + e;
	    }
	}
}


