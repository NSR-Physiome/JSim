/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model run-time loop control

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelLoops extends GNode implements GLineTable.Listener {

	// state
	private JMenuBar mbar;
	private JLabel l_hdr;
	private JPanel panel;
	private GLoop loop0, loop1;
	private JLabel statLabel;

	// constructor
	public GModelLoops(GModel gmodel) {
	    super(gmodel, gmodel.pmodel());

	    // create widgets
	    setJComp(new JRootPane());
	}

	// make content 
	public void makeContent() {
	    super.makeContent();
	    panel = new JPanel(null) {
		public void doLayout() { reconfig(); }
	    };
	    PModelLoops loops = gmodel().pmodel().loops();
	    loop0 = new GLoop(this, loops.inner());
	    loop0.table.addListener(this);
	    panel.add(loop0.jcomp());
	    loop1 = new GLoop(this, loops.outer());
	    loop1.table.addListener(this);
	    panel.add(loop1.jcomp());
	    statLabel = new JLabel("");
	    panel.add(statLabel);
	    JScrollPane scroll = new JScrollPane(panel);

	    // menubar
	    mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    gmodel().addParSetMenu(mbar);
	    mbar.add(gmodel().loopsRun.button("Run"));
	    gmodel().addRunsMenu(mbar);

	    helpLinks().addHelpMenu(this, mbar);

	    // root update
	    JRootPane root = (JRootPane) jcomp();
	    root.setJMenuBar(mbar);
	    root.setContentPane(scroll);
	    
	}

	// reconfig spacing
	public void reconfig() {
	    if (needsContent) makeContent();
	    if (loop0.needsReconfig()) loop0.reconfig();
	    if (loop1.needsReconfig()) loop1.reconfig();

	    // spacing parameters
	    int sp = glook().fontSize();
	    int w0 = loop0.jcomp().getPreferredSize().width;
	    int w1 = loop1.jcomp().getPreferredSize().width;
	    int w = Math.max(w0, w1);
	    int y=0;

	    // position widgets
	    Dimension dim = loop0.centerJComp(0, w, y);
	    y += dim.height + sp;
	    dim = loop1.centerJComp(0, w, y);
	    y += dim.height + sp;
	    dim = centerJComp(statLabel, 0, w, y);
	    y += dim.height + 2*sp;

	    // panel size
	    panel.setPreferredSize(new Dimension (w, y));
	}

	// refresh loops page
	public void refresh() {
	    if (needsContent) makeContent();
	    setTabLabel(l_hdr);
	    if (loop0.needsReconfig() || loop1.needsReconfig()) 
		reconfig();
	    String stat = "Model is not compiled.";
	    PModel pmodel = gmodel().pmodel();
	    if (pmodel.rt().isBuilt()) {
		int ict = pmodel.loops().inner().nloops();
		int oct = pmodel.loops().outer().nloops();
	        stat = "# loop iterations: " + ict + " inner;  " +
		    oct + " outer;  " + (ict*oct) + " total.";
	    }
	    statLabel.setText(stat);
	    gmodel().refreshParSetMenu(mbar);
	    super.refresh();
	}

	// GLineTable.Listener method
	public void tableSizeChanged(GLineTable l) {
	    reconfig();
	    refresh(); 
	}

	// one loop control
	public static class GLoop extends GNode {
	    PModelLoops.Loop loop;
	    JPanel panel;
	    JLabel jtitle, l_ntimes;
	    GControl g_mode;
	    GStringControl g_ntimes;
	    GLoopTable table;
	    private int saveMode;

	    // constructor
	    public GLoop(GNode p, PModelLoops.Loop l) {
		super(p, l);
		loop = l;
		panel = new JPanel(null) {
		    public void doLayout() { reconfig(); }
	    	};

		String title = Util.capitalize(loop.name()) +
		    " Loop Configurator       ";
		jtitle = new JLabel(title);
		jtitle.setFont(glook().bigFont());
		panel.add(jtitle);
		g_mode = new GMenuControl(this, loop.mode);
		g_mode.addAuxNode(gmodel());
		panel.add(g_mode.jcomp());
		l_ntimes = new JLabel("# times ");
		panel.add(l_ntimes);
		g_ntimes = new GStringControl(this,
		    loop.ntimes, 3);
		g_ntimes.addAuxNode(gmodel());
		panel.add(g_ntimes.jcomp());

		table = new GLoopTable(this, loop);
		panel.add(table.jcomp());
		setJComp(panel);

		saveMode = -1;
	    }

	    // does this loop need reconfig
	    public boolean needsReconfig() {
		return table.needsReconfig() || loop.mode.val() != saveMode;
	    }

	    // reconfig
	    public void reconfig() {
		if (table.needsReconfig()) table.reconfig();
		
		// spacing parameters
		int sp = glook().fontSize()/2;
	    	int y=0;

		// widget visibility
		table.jcomp().setVisible(loop.mode.val()!=0);

		// title
		int x = 0;
		x += setBounds(jtitle, x, y) + sp;

		// mode
		x += setBounds(g_mode.jcomp(), x, y) + sp;

		// #times
		l_ntimes.setVisible(loop.ntimesRequired());
		g_ntimes.jcomp().setVisible(loop.ntimesRequired());
		x += setBounds(l_ntimes, x, y) + sp;
		x += setBounds(g_ntimes.jcomp(), x, y) + sp;

		// table
	    	y += 3*sp;
		int w = x;
		if (table.jcomp().isVisible()) 
		    w = Math.max(w, table.jcomp().getPreferredSize().width);
		if (table.jcomp().isVisible()) {
	    	    Dimension dim = table.centerJComp(0, w, y);
	    	    y += dim.height + 3;
		}

		// panel dimensions
		y += 2*sp;
	    	panel.setPreferredSize(new Dimension (w, y));
		saveMode = loop.mode.val();
	    }

	    // set pref bounds for component
	    private int setBounds(JComponent jcomp, int x, int y) {
		jcomp.setLocation(x, y);
		Dimension dim = jcomp.getPreferredSize();
		jcomp.setSize(dim);
		return dim.width;
	    }

	    // refresh
	    public void refresh() {
	    	if (needsReconfig()) reconfig();
		super.refresh();
	    }
	}

	// expandable table for one PModelLoops.Loop
	public static class GLoopTable extends GLineTable {

	    private PModelLoops.Loop loop;

	    // constructor
	    public GLoopTable(GNode p, PModelLoops.Loop l) {
		super(p, 
		    new String[] { "Parameter", "", "Start", 
			"Other Values", "OK" },
		    new int[] { 6, 1, 6, 10, 2},
		    4);
		loop = l;
	    }
	
	    // control # rows
	    public IntControl nRowsCntl() { return loop.npars; }

	    // blank test
	    public boolean isBlank(int row) {
		return loop.looppar(row).isBlank();
	    } 

	    // create GControl
	    public GControl makeGCntl(int row, int col) {
		PModelLoops.LoopPar looppar = loop.looppar(row);
		switch(col) {
		case 0: 
		    GStringControl gcntl = new GStringControl(this, looppar.par, 10);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 1: 	
		    return new GMenuAux(this, (GStringControl) gcntl(row, 0));
		case 2: 	
		    gcntl = new GSlaveControl(this, (GStringControl) gcntl(row, 0), 10);
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 3: 	
		    gcntl = new GStringControl(this, looppar.vals, 20);
		    gcntl.addAuxNode(gmodel());
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 4:
		    GControl gcntl1 = new GEnableControl(this, looppar);
		    gcntl1.addAuxNode(gmodel());
		    return gcntl1; 	
		}
		return null;
	    }
	}
}

