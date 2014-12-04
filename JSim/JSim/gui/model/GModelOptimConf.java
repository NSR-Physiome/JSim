/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model run-time optimization configuration

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelOptimConf extends GNode implements GLineTable.Listener {
	private JMenuBar mbar;
	private JLabel l_hdr;
	private JPanel panel;
	private JLabel jtitle, jpars, jmatch;
	private GVertTable gleft, gright;
	private GParTable gpars;
	private GMatchTable gmatches;	

	// constructor
	public GModelOptimConf(GModelOptim g) {
	    super(g, g.gmodel().pmodel());

	    // create blank pane
	    setJComp(new JRootPane());
	    makeContent();  // should not be reqd, but is, FIX
	}

	// make content
	public void makeContent() {
	    super.makeContent();

	    // create panel
	    panel = new JPanel(null) {
		public void doLayout() { reconfig(); }
	    };
	    PModelOptim optim = gmodel().pmodel().optim();

	    // page title
	    jtitle = new JLabel("Model Optimizer Configuration");
	    jtitle.setFont(glook().bigFont());
	    panel.add(jtitle);

	    // left-side controls
	    gleft = new GVertTable(this,
		new Control[] { optim.alg, 
		    optim.maxCalls, optim.errTol, 
		    optim.reportPrec, optim.calcCovMat, 
		    optim.stepTol,
		    optim.populationSize, optim.mutationRate, optim.crossoverRate },
		9, // wider because of alg selection box
		new String[] { "Algorithm", 
		    "Max # runs", "Min RMS error", 
		    "Report precision", "Covariance mat?",
		    "Min par step", 
		    "Population", "Mutation rate", "Crossover rate" });
	    gleft.gcntl(0).addAuxNode(this); // alg change refreshes
	    gleft.gcntl(4).addAuxNode(this); // covmat change refreshes
	    panel.add(gleft.jcomp());

	    // right-side controls
	    gright = new GVertTable(this,
		new Control[] { 
		    optim.maxIters, 
		    optim.gradTol, optim.eps,
		    optim.initTemp, optim.selectMethod, optim.mutationStep,
                    optim.eliteCutoff, 
		    optim.npoints },
		8, 
		new String[] { 
		    "Max # iter", 
		    "Min gradient", "Relative error", 
		    "Init temp",  "Select Method", "Mutation step", "Elite cutoff",
		    "# grid points"
		    });
	    panel.add(gright.jcomp());

	    // pars
	    jpars = new JLabel("Parameters to vary");
	    jpars.setFont(glook().bigFont());
	    panel.add(jpars);
	    gpars = new GParTable(this, optim);
	    gpars.addListener(this);
	    panel.add(gpars.jcomp());

	    // matches
	    jmatch = new JLabel("Data to Match");
	    jmatch.setFont(glook().bigFont());
	    panel.add(jmatch);
	    gmatches = new GMatchTable(this, optim);
	    gmatches.addListener(this);
	    panel.add(gmatches.jcomp());

	    // menubar
	    mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;
	    gmodel().addParSetMenu(mbar);

	    // run button
	    mbar.add(gmodel().optimRun.button("Run"));
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

	    // reconfig sub tables
	    if (gpars.needsReconfig()) gpars.reconfig();
	    if (gmatches.needsReconfig()) gpars.reconfig();

	    // spacing parameters
	    int sp = glook().fontSize()/2;
	    int w = gleft.jcomp().getPreferredSize().width 
		+ gright.jcomp().getPreferredSize().width + 2*sp ;
	    int y = 0;

	    // title
	    Dimension pdim = centerJComp(jtitle, 0, w, y);
	    y += pdim.height + sp;

	    // gleft/gright
	    int wmid = w/2;
	    pdim = gleft.centerJComp(0, wmid, y);
	    Dimension gdim = gright.centerJComp(wmid, w, y+3*sp);
	    y += Math.max(pdim.height, gdim.height) + 3*sp;

	    // pars table
	    pdim = centerJComp(jpars, 0, w, y);
	    y += pdim.height;
	    pdim = gpars.centerJComp(0, w, y);
	    y += pdim.height + sp;

	    // matches table
	    pdim = centerJComp(jmatch, 0, w, y);
	    y += pdim.height;
	    pdim = gmatches.centerJComp(0, w, y);
	    y += pdim.height + 3*sp;

	    panel.setPreferredSize(new Dimension(w, y));
	}

	// refresh
	public void refresh() {
	    setTabLabel(l_hdr);
	    if (gpars.needsReconfig() || gmatches.needsReconfig())
		reconfig();
	    PModelOptim optim = gmodel().pmodel().optim();
	    OptimAlg alg;
	    try {
	    	alg = server().optimAlgs().alg(optim.alg.val());
	    } catch (Xcept e) {
	    	alg = null; 
	    }

	    // set gleft/gright row visibility
	    for (int r=0; r<gleft.nrows(); r++) {
	    	String n = gleft.gcntl(r).cntl().name();
		boolean vis = alg != null && alg.parNeeded(n);
		if (r < 5) vis = true;
		gleft.setRowVisible(r, vis);
	    }
	    for (int r=0; r<gright.nrows(); r++) {
	    	String n = gright.gcntl(r).cntl().name();
		boolean vis = alg != null && alg.parNeeded(n);
		gright.setRowVisible(r, vis);
	    }

	    // set gpars column visibility
	    boolean vBounds = alg != null && alg.boundsNeeded();
	    boolean vXstep = alg != null && alg.parNeeded("xstep");
	    boolean vCovmat = optim.calcCovMat.val();
	    gpars.setVisible(3, vBounds);
	    gpars.setVisible(4, vBounds);
	    gpars.setVisible(5, vXstep || vCovmat);

	    // misc
	    gmodel().refreshParSetMenu(mbar);
	    super.refresh();
	    jcomp().repaint(); // needed for ParTable reconfig
	}

	// GLineTable.Listener method
	public void tableSizeChanged(GLineTable l) {
	    reconfig();
	    refresh(); 
	}

	// expandable table for one PModelOptim.Loop
	public static class GParTable extends GLineTable {

	    private PModelOptim  optim;

	    // constructor
	    public GParTable(GNode p, PModelOptim o) {
		super(p, 
		    new String[] { "Parameter", "", "Start", 
			"Min", "Max", "Step", "OK" },
		    new int[] { 7, 1, 6, 6, 6, 4, 2},
		    4);
		optim = o;
	    }
	
	    // control # rows
	    public IntControl nRowsCntl() { return optim.npars; }

	    // blank test
	    public boolean isBlank(int row) {
		PModelOptim.Par par = optim.par(row);
		return par.par.isBlank();
	    } 

	    // create GControl
	    public GControl makeGCntl(int row, int col) {
		PModelOptim.Par par = optim.par(row);
		switch(col) {
		case 0: 
		    GStringControl gcntl = new GStringControl(this, par.par, 10);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 1: 	
		    return new GMenuAux(this, (GStringControl) gcntl(row, 0));
		case 2: 	
		    gcntl = new GSlaveControl(this, (GStringControl) gcntl(row, 0), 10);
		    gcntl.setValidBlack((GStringControl) gcntl(row, 0));
		    return gcntl;
		case 3: 	
		    gcntl = new GStringControl(this, par.min, 10);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack((GStringControl) gcntl(row, 0));
		    gcntl.setBlankNaN(true);
		    return gcntl;
		case 4: 	
		    gcntl = new GStringControl(this, par.max, 10);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack((GStringControl) gcntl(row, 0));
		    gcntl.setBlankNaN(true);
		    return gcntl;
		case 5: 	
		    gcntl = new GStringControl(this, par.step, 6);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack((GStringControl) gcntl(row, 0));
		    gcntl.setBlankNaN(true);
		    return gcntl;
		case 6:
		    GControl gcntl1 = new GEnableControl(this, par);
		    return gcntl1;	
		}
		return null;
	    }
	}

	// expandable table for one PModelOptim.Loop
	public static class GMatchTable extends GLineTable {

	    private PModelOptim  optim;

	    // constructor
	    public GMatchTable(GNode p, PModelOptim o) {
		super(p, 
		    new String[] { "DataSet", "Curve", "", "Par/Expr", "", 
			"Pwgt", "Cwgt", "OK" },
		    new int[] { 6, 5, 1, 5, 1, 4, 4, 2 },
		    4);
		optim = o;
	    }
	
	    // control # rows
	    public IntControl nRowsCntl() { return optim.nmatches; }

	    // blank test
	    public boolean isBlank(int row) {
		PModelOptim.Match match = optim.match(row);
		return match.data.isBlank() && 
		    match.expr.isBlank();
	    } 

	    // create GControl
	    public GControl makeGCntl(int row, int col) {
		PModelOptim.Match match = optim.match(row);
		switch(col) {
		case 0: 
		    GMenuControl gsrc = new GMenuControl(this, match.dataSrc);
		    gsrc.addAuxNode(gmodel());
		    return gsrc;
		case 1:
		    GStringControl gcntl = new GStringControl(this, match.data, 8);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 2: 	
		    return new GMenuAux(this, (GStringControl) gcntl(row, 1));
		case 3: 
		    gcntl = new GStringControl(this, match.expr, 8);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 4: 	
		    return new GMenuAux(this, (GStringControl) gcntl(row, 3));
		case 5: 	
		    gcntl = new GStringControl(this, match.pointWgts, 6);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 6: 	
		    gcntl = new GStringControl(this, match.curveWgt, 6);
		    gcntl.setValidBlack((GStringControl) gcntl(row, 5));
		    return gcntl;
		case 7:
		    GControl gcntl1 = new GEnableControl(this, match);
		    return gcntl1;	
		}
		return null;
	    }
	}
}

