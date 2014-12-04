/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// GDataSet Filter dialog

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public class GDataSetFilter extends GNode {
	// state
	private JDialog popup;
	private JLabel l_delta, l_dim;
	private GControl gtype, gmin, gmax, gdelta;
	private GMenuControl gdim;
	private GAction ok, cancel;
	protected int maxdim;

	// constructor
	public GDataSetFilter(GDataSetView g, PDataSet p) {
	    super(g, p);
	}

	// show the dialog
	public void show() {
	    if (popup == null) 
		createPopup();
	    refresh();
	    popup.setLocationRelativeTo(parent().jcomp());
	    popup.setVisible(true);
	}

	// create the popup
	private void createPopup() {
	    PDataSet dataset = dataset();
	    popup = new JDialog(parent().frame(), 
		"Filter data set " + dataset.name(), true);
	    JPanel panel = new JPanel(new GridLayout(7, 2));
	    ghelp().registerKeystrokes(panel);
	    popup.setContentPane(panel);

	    // title line
	    JLabel l = new JLabel("Filter");
	    l.setFont(glook().bigFont());
	    l.setHorizontalAlignment(JLabel.CENTER);
	    panel.add(l);
	    l = new JLabel("Control");
	    l.setFont(glook().bigFont());
	    l.setHorizontalAlignment(JLabel.CENTER);
	    panel.add(l);

	    // type line
	    panel.add(new JLabel("Type"));
	    gtype = GControl.create(this, dataset.filterType);
	    gtype.addAuxNode(this);
	    panel.add(gtype.jcomp());

	    // dim line
	    l_dim = new JLabel("Dimension");
	    panel.add(l_dim);
	    gdim = new GMenuControl(this, dataset.filterDim) {
		public String[] makeLabels() {
		    int n = ((GDataSetFilter) parent()).maxdim;
		    if (n<1) n=1;
		    String[] labels = new String[n];
		    for (int i=0; i<labels.length; i++) 
			labels[i] = "" + i;
		    return labels;
		}
	    };
	    gdim.addAuxNode(this);
	    panel.add(gdim.jcomp());

	    // min line
	    panel.add(new JLabel("Min"));
	    gmin = GControl.create(this, dataset.filterMin);
	    panel.add(gmin.jcomp());

	    // max line
	    panel.add(new JLabel("Max"));
	    gmax = GControl.create(this, dataset.filterMax);
	    panel.add(gmax.jcomp());

	    // delta line
	    l_delta = new JLabel("Delta");
	    panel.add(l_delta);
	    gdelta = GControl.create(this, dataset.filterDelta);
	    panel.add(gdelta.jcomp());

	    // OK/Cancel
	    ok = new GAction(this, "OK") {
	       	public void doit() throws Xcept {
		    int[] inxs = gview().getSelectedIndices();
		    Xcept e = null;
		    try {
		    	dataset().filterData(gview().getSelectedData());
		    } catch (Xcept x) {
			e = x;
		    }
		    popup.setVisible(false);
		    gview().reloadData();
		    gnode.gproject().project().revalidate();
		    gnode.gproject().refresh();
		    gview().setSelectedIndices(inxs);	    
		    if (e != null) throw e;
	        }
	    };
	    JButton b = new JButton(ok);
	    b.setBackground(Color.white);
	    panel.add(b);
	    cancel = new GAction(this, "Cancel") {
	       	public void doit() {
		    popup.setVisible(false);
	        }
	    };
	    b = new JButton(cancel);
	    b.setBackground(Color.white);
	    panel.add(b);

	    // dialog size
	    panel.setSize(panel.getPreferredSize());
	    popup.pack();
	}

	// refresh
	public void refresh() {
	    if (popup == null || refreshing) return;
	    refreshing = true;

	    // figure maxdim
	    Data.List dlist = gview().getSelectedData();
	    maxdim = 0;
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		if (i==0 || data.ndim()<maxdim)
		    maxdim = data.ndim();
	    }
	    gdim.resetLabels();

	    // refresh defaults may err
	    try {
		refreshDefaults();
	    } catch (Xcept e) {
		gproject().message(e);
	    }

	    // set visibility
	    boolean bdelta = 
		dataset().filterType.val() != DataFilter.CROP;
	    boolean bdim = maxdim > 1;
	    l_delta.setVisible(bdelta);
	    gdelta.jcomp().setVisible(bdelta);
	    l_dim.setVisible(bdim);
	    gdim.jcomp().setVisible(bdim);

	    // superclass refresh
	    refreshing = false;
	    super.refresh();
	}

	// refresh defaults
	private void refreshDefaults() throws Xcept {
	    if (dataset().filterDim.val() >= maxdim)
		dataset().filterDim.setVal(0);

	    // default min, max, delta based on selected data
	    double nmin = Double.NaN;
	    double nmax = Double.NaN;
	    double ndelta = Double.NaN;
	    Data.List dlist = gview().getSelectedData();
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		if (data.ndim() == 0) continue;
		GridData grid = data.grid(dataset().filterDim.val());
	        if (i==0 || grid.min()<nmin) nmin = grid.min();
	        if (i==0 || grid.max()>nmax) nmax = grid.max();
		double xdelta = (grid.max()-grid.min())/(grid.ct()-1);
		if (i==0 || xdelta<ndelta) ndelta = xdelta;
	    }
	    dataset().filterMin.setVal(nmin);
	    dataset().filterMax.setVal(nmax);
	    dataset().filterDelta.setVal(ndelta);
	}

	// query
	private GDataSetView gview() {
	    return (GDataSetView) parent(); 
	}
	private PDataSet dataset() {
	    return (PDataSet) pnamed();
	}

}
