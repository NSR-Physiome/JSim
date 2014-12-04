/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model run-time Monte Carlo configuration

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelMonteConf extends GNode   {
	private JMenuBar mbar;
	private JLabel l_hdr;
	private JPanel panel;
	private JLabel jtitle;
	private GVertTable gpars;
	private GAction storePars, storeNoisy;
	
	// constructor
	public GModelMonteConf(GModelMonte g) {
	    super(g, g.gmodel().pmodel());

	    // create blank pane
	    setJComp(new JRootPane());
	    makeContent();  // should not be reqd, but is, FIX
	}

	// make content
	public void makeContent() {
	    super.makeContent();

	    // store par data
	    storePars = new GAction(this, "Store pars in project dataset ...") {
		public void doit() throws Xcept {
		    Data.List dlist = getParData();
		    gproject().pushTree();
		    String msg = "Store par data under name";
		    Project proj = gproject().project();
		    String n = proj.newChildName("data", true);
		    String nname = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, "Enter name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().datasetIcon(),
			    null, n);
		    gproject().popTree();
		    if (nname == null) return;
		    PDataSet dset = new PDataSet(proj, nname);
		    dset.importData(dlist);
		    gproject().add(dset);
		    gproject().refresh();
		}
	    };		

	    // store noisy data 
	    storeNoisy = new GAction(this, "Store noisy data in project dataset ...") {
		public void doit() throws Xcept {
		    Data.List dlist = getNoisyData();
		    gproject().pushTree();
		    String msg = "Store noisy data under name";
		    Project proj = gproject().project();
		    String n = proj.newChildName("data", true);
		    String nname = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, "Enter name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().datasetIcon(),
			    null, n);
		    gproject().popTree();
		    if (nname == null) return;
		    PDataSet dset = new PDataSet(proj, nname);
		    dset.importData(dlist);
		    gproject().add(dset);
		    gproject().refresh();
		}
	    };		

	    // create panel
	    panel = new JPanel(null) {
		public void doLayout() { reconfig(); }
	    };
	    PModelMonte pmonte = gmodel().pmodel().monte();

	    // page title
	    jtitle = new JLabel("Model Monte Carlo Configuration");
	    jtitle.setFont(glook().bigFont());
	    panel.add(jtitle);

	    // left-side controls
	    gpars = new GVertTable(this,
		new Control[] { 
		    pmonte.noptims,
		    pmonte.randomSeed,
		    pmonte.dist,
		    pmonte.magnitude,
		    pmonte.addMethod  },
		9, // width
		new String[] { 
		    "# optimzations",	
		    "random seed",	
		    "distribution",	
		    "magnitude",	
		    "add method" }
		);
	    gpars.gcntl(2).addAuxNode(this); // dist change refreshes
	    panel.add(gpars.jcomp());

	    // menubar
	    mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu = newMenu("File");
            menu.add(storePars.item());
            menu.add(storeNoisy.item());
	    mbar.add(menu);
 
	    // run button
	    mbar.add(gmodel().monteRun.button("Run"));
	    gmodel().addRunsMenu(mbar);

	    helpLinks().addHelpMenu(this, mbar);

	    // set content
	    JRootPane root = (JRootPane) jcomp();
	    root.setJMenuBar(mbar);
	    JScrollPane scroll = new JScrollPane(panel);
	    root.setContentPane(scroll);
	}

	// reconfig spacing
	private void reconfig() {
	    if (needsContent) makeContent();

	    // spacing parameters
	    int sp = glook().fontSize()/2;
	    int w = jtitle.getPreferredSize().width + 2*sp ;
	    int y = 0;

	    // title
	    Dimension pdim = centerJComp(jtitle, 0, w, y);
	    y += pdim.height + 4*sp;

	    // gpars
	    pdim = gpars.centerJComp(0, w, y);
	    y += pdim.height + 3*sp;

	    // panel size
	    panel.setPreferredSize(new Dimension(w, y));
	}

	// refresh
	public void refresh() {
	    setTabLabel(l_hdr);
	    PModelMonte monte = gmodel().pmodel().monte();

	    // misc
	    gmodel().refreshParSetMenu(mbar);
	    super.refresh();
	}

	// get ParData
	private Data.List getParData() throws Xcept {
	    MoptData moptData = gmodel().pmodel().rt().getMoptData();
	    if (moptData == null) throw new Xcept(
	    	"Par data not available: run Monte Carlo first.");
	    return moptData.parData();
	}

	// get Noisy data
	private Data.List getNoisyData() throws Xcept {
	    Data[][] noisyData = gmodel().pmodel().monte().getNoisyData();
	    if (noisyData == null) throw new Xcept(
	    	"Noisy data not available: run Monte Carlo first.");
	    return new Data.List(noisyData);
	}
}

