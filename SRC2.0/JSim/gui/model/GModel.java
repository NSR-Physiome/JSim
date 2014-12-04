/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Model 

package JSim.gui.model;

import java.util.ArrayList;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import JSim.util.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.xsim.*;
import JSim.gui.*;

public class GModel extends GTabs {

	// state info
	private PModel pmodel;
	private JRootPane root;
	private GNotes gnotes;
	private GModelEaselTab geasel;
	private GModelSrc gsrc;
	private GModelPars gpars;
	private GModelDiag gdiag;
	private GModelLoops gloops;
	private GModelSens gsens;
	private GModelOptim goptim;
	private GModelMonte gmonte;
	private GModelBrowser gbrowser;
	private GImageSet gimages;
	private GModelMemory gmemory;
	private boolean editable; 
	private GModelHelpDB helpDB;
	private GModelForwardIC forwardICDialog;

	private static String knew = "<new parameter set>";

	// actions
	public GAction loadSet, loadXSet, revert, forwardIC, 
	    storeSet, storeSetAs, print, exportEPS; // Parset menu
	public GAction singleRun, loopsRun, optimRun, monteRun,
	   sensRun; // All_runs menu

	// constructor
	public GModel(GNode p, PModel m) throws Xcept {
	    super(p, m);
	    pmodel = m;
	    editable = true;

	    // load project parameter set
	    loadSet = new GAction(this, "Load project parameter set ...") {
		public void doit() throws Xcept {
		    StringList opts = makeParsetOptionsList();
		    if (opts.size() == 0) throw new Xcept(
			"No parameter sets available in this project");
		    String msg = "Choose parameter set to load";
		    String opt = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, "Load parameter set", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().parsetIcon(),
			    opts.array(), null);
		    if (opt == null) return;
		    String name = getNameFromParsetOption(opt);
		    PModel pmodel = gmodel().pmodel();
		    pmodel.loadParSet(name);
		    gproject().refresh();
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };    

	    // load XSim parameter file
	    loadXSet = new GAction(this, "Import XSim parameter file ...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.XSIMPAR);
		    if (info == null) return;
		    XSParFile xsparf = new XSParFile(info.file);
		    PModel pmodel = gmodel().pmodel();
		    xsparf.assignModelPars(pmodel);
		    StringList report = xsparf.report();
		    String s = report.toString("\n", false);
		    gproject().message("\n" + s);
		    gproject().message(" ");
		    gproject().pushMessages();
		    gproject().refresh();
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };

	    // revert action
	    revert = new GAction(this, "Revert to model defaults") {
		public void doit() throws Xcept {
		    gmodel().pmodel().defaultParSet();
		    refresh();
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };

	    // forward ICs action
	    forwardIC = new GAction(this, "Forward ODE ICs from last run") {
		public void doit() throws Xcept {
		    if (forwardICDialog == null) 
		    	forwardICDialog = new GModelForwardIC(
			    gmodel(), gmodel().pmodel());
		    forwardICDialog.show();
		    refresh();
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };

	    // store parameter set under existing name
	    storeSet = new GAction(this, "Store parameter set") {
		public void doit() throws Xcept {
		    storeParSet(false);
		}
		public String revisedText() {
		    return "Store parameter set " +
	    		(gmodel().pmodel().getParsModified() ?
			     "(needed)" : "(not needed)");
		}
		
	    };			

	    // store parameter set as
	    storeSetAs = new GAction(this, "Store parameter set as ...") {
		public void doit() throws Xcept {
		    storeParSet(true);
		}
	    };			

	    // print action
	    print = new GAction(this, "Print...") {
	        public void doit() throws Xcept {
		    GNode g = gmodel().selectedNode();
		    g.startPrintJob();
		}
	    };
	    
	    // export EPS action
	    exportEPS = new GAction(this, "Export EPS graphics ..."){
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.EPS);
		    if (info == null) return;
		    File f = info.file;
		    gnode.exportEPS(f);
		}
	    };
	 
	    // run model action
	    singleRun = new GAction(this, "Single Run") {
		public void doit() throws Xcept {
	    	    gnode.gmain().runJob(
			new GModelJob.SingleRun(gnode.gmodel()));
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };
	    singleRun.setAccel('R');

	    // run loops action
	    loopsRun = new GAction(this, "Loops Run") {
		public void doit() throws Xcept {
	    	    gnode.gmain().runJob(
			new GModelJob.Loops(gnode.gmodel()));
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };
	    loopsRun.setAccel('L');

	    // run loops action
	    sensRun = new GAction(this, "Sensitivity Run") {
		public void doit() throws Xcept {
	    	    gnode.gmain().runJob(
			new GModelJob.Sens(gnode.gmodel()));
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };
	    sensRun.setAccel('Y');

	    // optimize action
	    optimRun = new GAction(this, "Optimize") {
		public void doit() throws Xcept {
	    	    gnode.gmain().runJob(
			new GModelJob.Optim(gnode.gmodel()));
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };
	    optimRun.setAccel('P');

	    // Monte Carlo action
	    monteRun = new GAction(this, "Monte Carlo") {
		public void doit() throws Xcept {
	    	    gnode.gmain().runJob(
			new GModelJob.Monte(gnode.gmodel()));
		}
		public boolean sbEnabled() { return gnode.editable(); }
	    };
	    monteRun.setAccel('M');

	    // widgets
	    root = new JRootPane();
	    root.getContentPane().add(jcomp(), BorderLayout.CENTER);
	    setJComp(root);
	    setTabPlacement(SwingConstants.BOTTOM);
//	    setTabPlacement(SwingConstants.TOP);
//	    tabs().setTabLayoutPolicy(JTabbedPane.WRAP_TAB_LAYOUT);
	    makeContent();  // omitting breaks recompile
	    gproject().addTab(this);
	}

	// make content
	public void makeContent() {
	    super.makeContent();

	    if (! pmodel.easelVariant.isBlank()) {
	    	geasel = new GModelEaselTab(this);
		addTab("Easel", geasel);
	    }

	    gsrc = new GModelSrc(this, pmodel.modelSource);
	    addTab("Source", gsrc);

	    gpars = new GModelPars(this);
	    addTab("Run Time", gpars);

	    gbrowser = new GModelBrowser(this, pmodel.browser());
	    addTab("Browser", gbrowser);

	    gdiag = new GModelDiag(this);
	    addTab("Debug", gdiag);

	    gloops = new GModelLoops(this);
	    addTab("Loops", gloops);

	    gsens = new GModelSens(this, pmodel.sens());
	    addTab("Sensitivity", gsens);

	    goptim = new GModelOptim(this);
	    goptim.makeTabs(); 
	    addTab("Optimizer", goptim);

	    gmonte = new GModelMonte(this);
	    gmonte.makeTabs(); 
	    addTab("Monte Carlo", gmonte);

	    gnotes = new GNotes(this, pmodel.notes);
	    addTab("Notes", gnotes);

	    gimages = new GImageSet(this, pmodel.images());
	    addTab("Images", gimages);

	    gmemory = new GModelMemory(this, pmodel());
	    addTab("Memory", gmemory);

	    helpDB = new GModelHelpDB(this); // no tab 
	}

	// do stuff
	public void setEditable(boolean b) { editable = b; }
	public void showParsTab() {
	    int pinx = 1;
	    if (! pmodel.easelVariant.isBlank()) pinx++;
	    setSelectedIndex(pinx);
	    // must also set focus in text box for accels to work
	}

	// create runs menu
	public void addRunsMenu(JMenuBar mbar) {
	    JMenu menu = newMenu("All_Runs");
	    menu.add(singleRun.item());
	    menu.add(loopsRun.item());
	    menu.add(optimRun.item());
	    menu.add(sensRun.item());
	    mbar.add(menu);
	}

	// create ParSet menu
	public void addParSetMenu(JMenuBar mbar) {
	    JMenu menu = newMenu("ParSet");
	    JMenuItem g_curr = new JMenuItem("Current:");
	    g_curr.setEnabled(false);
	    menu.add(g_curr);
	    menu.add(loadSet.item());
	    menu.add(loadXSet.item());
	    menu.add(revert.item());
	    menu.add(forwardIC.item());
	    menu.addSeparator();
	    menu.add(storeSet.item());
	    menu.add(storeSetAs.item());
//	    menu.addSeparator();  items below need work
//	    menu.add(print.item());
//	    menu.add(exportEPS.item());
	    mbar.add(menu);
	}

	// refresh ParSet menu
	public void refreshParSetMenu(JMenuBar mbar) {
	    JMenu menu = null;
	    for (int i=0; i<mbar.getMenuCount(); i++) {
		if (mbar.getMenu(i) == null) continue;
		if (mbar.getMenu(i).getText().equals("ParSet")) {
		    menu = mbar.getMenu(i);
		    break;
		}
	    }
	    if (menu == null) return;
	    JMenuItem g_curr = menu.getItem(0);
	    String psname = pmodel().parSetName.val();
	    String slocked = "";
	    if (Util.isBlank(psname)) {
	        psname = "<default>";
	    } else {
	        try { 
		    slocked = parsetLockedString(psname); 
		} catch (Exception e) { 
		    System.err.println("" + e);
		}
	    } 
	    g_curr.setText("Current: " + psname + slocked);
	}

	// get parset locked clause
	private String parsetLockedString(String psname) throws Xcept {
	    PNamed p = gproject().project().child(psname);
	    if (! (p instanceof ParSet)) return "";
	    boolean locked = ((ParSet) p).locked.val();
	    return locked ? " (locked)" : " (not locked)";
	}

	// realize custom RTML
	public void realizeCustom() {
	    try {
	    	pmodel.customBuilt.setVal(false);
	    	gpars().reloadCustom(null);
		Element rtml = pmodel.customRTML().rtml();
	    	gpars().reloadCustom(rtml);
	    	pmodel.customBuilt.setVal(rtml != null);
	    } catch (Xcept e) {
		gproject().warning("Error realizing custom RTML: " +
		    e.cleanMessage());
	    }
	}

	// store parameter set dialog
	public void storeParSet(boolean saveAs) throws Xcept {
 	    Project proj = gproject().project();
	    PModel pmodel = gmodel().pmodel();
	    
//	    StringList options = proj.children(ParSet.class).nameList();
	    StringList options = makeParsetOptionsList();

	    if (options.size() == 0) saveAs = true;
	    String curr = pmodel.parSetName.val();
	    String sname = saveAs ? knew : curr;
	    if (saveAs && options.size() > 0) {
		options.add(knew);
		sname = (String) JOptionPane.showInputDialog(
		    jcomp(), "Store parameter set", "Select parameter set", 
		    JOptionPane.QUESTION_MESSAGE, 
		    glook().parsetIcon(),
		    options.array(), knew);
	    	if (sname == null) return;
		sname = getNameFromParsetOption(sname);
	    }
	    if (sname.equals(knew)) {
		sname = proj.newChildName("pars", true);
		sname = (String) JOptionPane.showInputDialog(
		jcomp(), "Store new parameter set", "Enter parameter set name", 
		    JOptionPane.QUESTION_MESSAGE, 
		    glook().parsetIcon(),
		    null, sname);
		if (sname == null) return;
		if (proj.child(sname) != null) {
		    gmain().errorPopup(jcomp(),
		    	"Store aborted: <" + sname + "> already exists in this project");
		    return;
		}
	    } else if (! curr.equals(sname)) {
	    	int stat = JOptionPane.showConfirmDialog(jcomp(), 
		   "Replacing existing parset " + sname + 
		   " with current parameters?",
		   "Replace Parameter Set", JOptionPane.YES_NO_OPTION);
		if (stat != JOptionPane.OK_OPTION) return;
	    }

	    // check lock
	    ParSet parset = (ParSet) proj.child(sname);
	    if (parset != null && parset.locked.val()) {
	    	int stat = JOptionPane.showOptionDialog(jcomp(),
		   "ParSet " + sname + " is locked.\n" +
		   "Are you really sure you wish to replace it?",
		   "Locked Parameter Set", JOptionPane.YES_NO_OPTION,
		   JOptionPane.WARNING_MESSAGE, null,
		   new String[] { "YES", "NO" }, "NO");
		boolean ok = stat == JOptionPane.YES_OPTION;
	        if (! ok) throw new Xcept(
		    "Canceled by user");
	    }
		   	
	    // do store
	    parset = gmodel().pmodel().storeParSet(sname);
	    GNode gnode = gproject().gtree().gnode(parset);
	    if (gnode == null)
	    	gproject().add(parset);

	    gproject().message("Store parameters in parameter set " + sname);
	    gproject().refresh();
	}

	// create parset options list for selection dialog (name + locked status)
	private StringList makeParsetOptionsList() {
	    Project proj = gproject().project();
	    PNamed.List sets = proj.children(ParSet.class);
	    StringList opts = new StringList();
	    for (int i=0; i<sets.size(); i++) {
		ParSet parset = (ParSet) sets.pnamed(i);
		String opt = parset.name() + 
		    (parset.locked.val() ? " (locked)" : " (unlocked)");
		opts.add(opt);
	    }
	    return opts;
	}

	// return parset name from option
	private String getNameFromParsetOption(String opt) {
	    if (opt.equals(knew)) return opt;
	    int inx = opt.indexOf(' ');
	    return (inx > 0) ? opt.substring(0, inx) : opt;
	} 

	// refresh
	public void refresh() {
	    singleRun.setEnabled(); 
	    loopsRun.setEnabled(); 
	    optimRun.setEnabled(); 
	    helpDB.refresh(); // not GNode, so not automatic
	    super.refresh();
	}

	// query
	public PModel pmodel() { return pmodel; }
	public GModelSrc gsrc() { return gsrc; }
	public GModelPars gpars() { return gpars; }
	public GModelDiag gdiag() { return gdiag; }
	public GModelOptim goptim() { return goptim; }
	public GImageSet gimages() { return gimages; }
	public GHelpDB helpDB() { return helpDB; }
	public String title() { 
	    return gproject().title() + 
	        " Model " + pmodel.name();
	}
	public boolean editable() { return editable; }
}

