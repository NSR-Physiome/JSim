/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model-run Memory control

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import JSim.util.*;
import JSim.gui.*;
import JSim.project.*;

public class GModelMemory extends GNode {
	private JRootPane root;
	private JPanel panel;
	private StringList domains; 
	private JLabel l_title, l_data, l_domain, l_N;
	private GControl g_storeGrids;
	private Hashtable<String, JLabel> l_xs;
	private Hashtable<String, GStringControl> gnths;

	// constructor
	public GModelMemory(GModel gmodel, PModel pmodel) {
	    super(gmodel, pmodel);
	    root = new JRootPane();
	    panel = new JPanel(null) {
		public void doLayout() { 
		   reconfig(); 
		}
	    };
	    root.setContentPane(panel);
	    setJComp(root);
	    l_xs = new Hashtable<String, JLabel>();
	    gnths = new Hashtable<String, GStringControl>();
	}
	
	// make content
	public void makeContent() {
	    l_title = new JLabel("Memory Allocation / Model Data Storage");
	    l_title.setFont(glook().bigFont());
	    panel.add(l_title);
	    l_data = new JLabel("Data storage:", JLabel.CENTER);
	    panel.add(l_data);
	    g_storeGrids = new GMenuControl(this, pmemory().storeGrids) {
	    	public String[] makeLabels() {
		    return new String[] { "all points", "every Nth point" };
		}
	    };
	    g_storeGrids.addAuxNode(this);
	    panel.add(g_storeGrids.jcomp());
	    l_domain = new JLabel("Domains", JLabel.CENTER);
	    panel.add(l_domain);
	    l_N = new JLabel("N", JLabel.CENTER);
	    panel.add(l_N);

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    root.setJMenuBar(mbar);
	    JLabel l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    gmodel().addParSetMenu(mbar);
	    gmodel().addRunsMenu(mbar);
	    helpLinks().addHelpMenu(this, mbar);

	    needsContent = false;
	}

	// reconfig
	public void reconfig() {
	    if (needsContent) makeContent();
	    
	    // create missing domain labels/gcontrols
	    domains = pmemory().domains();
	    if (domains == null) domains = new StringList();
	    for (int i=0; i<domains.size(); i++) {
	    	String x = domains.get(i);
		if (l_xs.get(x) == null) {
		    JLabel l_x = new JLabel(x, JLabel.CENTER);
		    l_xs.put(x, l_x);
		    panel.add(l_x);
		}
		if (gnths.get(x) == null) {
		    IntControl pnth = pmemory().nthControl(x);
		    GStringControl gnth = new GStringControl(this, pnth, 5);	
		    gnths.put(x, gnth);
		    panel.add(gnth.jcomp());
		}
	    }
	    
	    // spacing parameters
	    int sp = glook().fontSize()/2;
	    int w = l_title.getPreferredSize().width;
	    int y=0;
	    int wleft = w/5;
	    int wright = 4*w/5;
	    int wcol = w/5;
	    int wmid = w/2;
	
	    // position title
	    Dimension dim = centerJComp(l_title, 0, w, y);
	    y += dim.height + sp;

	    // position storeGrids select
 	    centerJComp(l_data, wleft, wmid, y);
	    dim = centerJComp(g_storeGrids.jcomp(), wmid, wright, y);
	    y += dim.height + sp;
	    wleft += 30;
	    wright -= 30;
	    
	    // position domain/N table headers
	    centerJComp(l_domain, wleft, wmid, y);
	    dim = centerJComp(l_N, wmid, wright, y);
	    y += dim.height;
	    boolean nvis = pmemory().storeGrids.val() == PModelMemory.GRID_NTH;
	    l_domain.setVisible(true);
	    l_N.setVisible(nvis);

	    // position domain labels/controls
	    Iterator<String> xs = gnths.keySet().iterator();
	    while (xs.hasNext()) {
	    	String x = xs.next();
		boolean xvis = domains.contains(x);
		JLabel l_x = l_xs.get(x);
		l_x.setVisible(xvis);
		GStringControl gnth = gnths.get(x);
		gnth.jcomp().setVisible(xvis && nvis);
		if (! xvis) continue;
		centerJComp(l_x, wleft, wmid, y);
		dim = centerJComp(gnth.jcomp(), wmid, wright, y);
		y += dim.height;
	    }
	}

	// refresh
	public void refresh() {
	   reconfig();
	   super.refresh();
	}

	// query
	public GModel gmodel() { return (GModel) parent(); }
	public PModel pmodel() { return (PModel) pnamed(); }
	public PModelMemory pmemory() { return pmodel().vars().memory(); }
}
