/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model run-time sensitivity control

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelSens extends GNode implements GLineTable.Listener {
	private PModelSens sens;
	private JMenuBar mbar;
	private JPanel panel;
	private JLabel l_hdr;
	private JLabel jtitle;
	private GSensTable table;

	// constructor
	public GModelSens(GNode p, PModelSens s) {
	    super(p, s);
	    sens = s;

	    // create widgets
	    setJComp(new JRootPane());
	}

	// make content
	public void makeContent() {
	    super.makeContent();

	    panel = new JPanel(null) {
		public void doLayout() { reconfig(); }
	    };
	    jtitle = new JLabel(
		"Sensitivity Analysis Configurator");
	    jtitle.setFont(glook().bigFont());
	    panel.add(jtitle);
	    table = new GSensTable(this, sens);
	    table.addListener(this);
	    panel.add(table.jcomp());
	    JScrollPane scroll = new JScrollPane(panel);

	    // menubar
	    mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    gmodel().addParSetMenu(mbar);
	    mbar.add(gmodel().sensRun.button("Run"));
	    gmodel().addRunsMenu(mbar);

	    helpLinks().addHelpMenu(this, mbar);

	    // root additions
	    JRootPane root = (JRootPane) jcomp();
	    root.setContentPane(scroll);
	    root.setJMenuBar(mbar);
	}

	// reconfig
	public void reconfig() {
	    if (needsContent) makeContent();

	    // spacing parameters
	    int sp = glook().fontSize()/2;
	    int w = table.jcomp().getPreferredSize().width;
	    int y=0;

	    // position widgets
	    Dimension dim = centerJComp(jtitle, 0, w, y);
	    y += dim.height + sp;
	    dim = table.centerJComp(0, w, y);
	    y += dim.height + 3*sp;

	    panel.setPreferredSize(new Dimension (w, y));
	}

	// refresh
	public void refresh() {
	    if (needsContent) makeContent();
	    if (table.needsReconfig()) reconfig();
 	    setTabLabel(l_hdr);
	    gmodel().refreshParSetMenu(mbar);
	    super.refresh();
	}

	// GLineTable.Listener method
	public void tableSizeChanged(GLineTable l) {
	    reconfig();
	    refresh(); 
	}

	// expandable table for one PModelSens
	public static class GSensTable extends GLineTable {
	    private PModelSens sens;

	    // constructor
	    public GSensTable(GNode p, PModelSens s) {
		super(p, 
		    new String[] { "Parameter", "", "Value", "Delta", "OK" },
		    new int[] { 7, 1, 7, 7, 2},
		    12);
		sens = s;
	    }
	
	    // control # rows
	    public IntControl nRowsCntl() { return sens.npars; }

	    // blank test
	    public boolean isBlank(int row) {
		return sens.senspar(row).isBlank();
	    } 

	    // create GControl
	    public GControl makeGCntl(int row, int col) {
		PModelSens.SensPar senspar = sens.senspar(row);
		switch(col) {
		case 0: 
		    GStringControl gcntl = new GStringControl(this, senspar.par, 10);
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
		    gcntl = new GStringControl(this, senspar.delta, 10);
		    gcntl.setValidBlack(true);
		    return gcntl;
		case 4:	
		    GControl gcntl1 = new GEnableControl(this, senspar);
		    return gcntl1;
		}
		return null;
	    }
	}
}

