/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// expandable table

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import JSim.util.*;
import JSim.project.*;

abstract public class GLineTable extends GNode {

	// state
	private JPanel jtable;	// top-level component
	private int currNRows;
	private String[] hdrs;
	private int[] colWidths;
	private GControl.List[] gcntls;
	private boolean[] visible;
	private boolean needsReconfig;
	private ArrayList<Listener> listeners;

	// constructor
	public GLineTable(GNode p, String[] h, int[] cw, int rows) {
	    super(p, null);
	    hdrs = h;
	    colWidths = cw;
	    jtable = new JPanel(null);
	    setJComp(jtable);
	    currNRows = -1;
	    gcntls = new GControl.List[nCols()];
	    visible = new boolean[nCols()];
	    for (int i=0; i<nCols(); i++) {
		gcntls[i] = new GControl.List();
		visible[i] = true;
	    }
	    needsReconfig = true;
	    listeners = new ArrayList<Listener>(2);
	}

	// add listener
	public void addListener(Listener l) {
	    listeners.add(l);
	}

	// theme updated (font size)
	public void lookUpdated() {
	    needsReconfig = true;
	    gcntls = new GControl.List[nCols()];
	    for (int i=0; i<nCols(); i++) 
		gcntls[i] = new GControl.List();
	}

	// adjust visibility
	public void setVisible(int i, boolean v) {
	    if (v == visible[i]) return;
	    visible[i] = v;
	    needsReconfig = true;
	}

	// refresh
	public void refresh() {
	    trailingBlank();   
	    if (needsReconfig()) reconfig();
	    super.refresh();
	}

	// adjust nRows to insure exactly one trailing blank 
	private void trailingBlank() {
	    int n = nRows()-1;
	    while (n>=0 && isBlank(n)) n--;
	    n += 2;
	    if (nRows() == n) return;

	    try {
		nRowsCntl().setVal(n);
	    } catch (Xcept e) { 
 	    }
	    for (int c=0; c<nCols(); c++) {
		int sz = gcntls[c].size();
		for (int i=n; i<sz; i++) {
		    GControl gcntl = gcntls[c].gcntl(n);
		    gcntl.jcomp().setVisible(false); // otherwise, display hangs
		    gcntl.destroy();
		    gcntls[c].remove(n);
		}
	    }

	    // notify listeners # lines has changed
	    for (int i=0; i<listeners.size(); i++) {
		Listener l = (Listener) listeners.get(i);
	        l.tableSizeChanged(this);
	    }
	}

	// reconfigure when nRows changed
	public void reconfig() {
	    jtable.removeAll();

	    // header line
	    int x = 0;
	    for (int c=0; c<nCols(); c++) {
		JLabel l = new JLabel(hdrs[c], JLabel.CENTER);
		int w = colWidths[c] * glook().fontSize();
		l.setBounds(x, 0, w, rowHeight());
		l.setVisible(visible[c]);
		jtable.add(l);
		x += w;
	    }	

	    // data entry lines 
	    JPanel jlines = new JPanel(
		new GridLayout(nRows(), 1));
	    jtable.add(jlines);
	    jlines.setBounds(0, rowHeight(), rowWidth(), nRows()*rowHeight());
	    for (int r=0; r<nRows(); r++) {
	    	JPanel jline = new JPanel(null);
		x = 0;
		for (int c=0; c<nCols(); c++) {
		    GControl gcntl = gcntl(r, c);
		    if (gcntl == null) continue;
		    JComponent jcomp = gcntl.jcomp();
		    jline.add(jcomp);
		    int w = colWidths[c] * glook().fontSize();
		    jcomp.setBounds(x, 0, w, rowHeight());
		    x += w;
		    jcomp.setVisible(visible[c]);
		}
	    	jline.setSize(
		    new Dimension(rowWidth(), rowHeight()));
	        jlines.add(jline);
	    }

	    // table preferred size
	    Dimension dim = new Dimension(rowWidth(), (nRows()+1)*rowHeight());
	    jtable.setPreferredSize(dim);

	    // save to check next refresh
	    currNRows = nRows();
	    needsReconfig = false;
	}

	// query
	public int nRows() { return nRowsCntl().val(); }
	public int nCols() { return colWidths.length; }
	public boolean needsReconfig() {
	    return needsReconfig || nRows() != currNRows;
	}
	public GControl gcntl(int row, int col) {
	    for (int r=gcntls[col].size(); r<=row; r++) {
		GControl gcntl = makeGCntl(r, col);
		gcntls[col].add(gcntl);
		Control cntl = gcntl.cntl();
		if (cntl != null) cntl.revalidate();
	    }
	    return gcntls[col].gcntl(row);
	} 
	public int rowWidth() { 
	    int w = 0;
	    for (int i=0; i<nCols(); i++) 
		w += colWidths[i];
	    return w * glook().fontSize();
	}
	public int rowHeight() { 
	    return (int) (glook().fontSize() * 1.5);
	}
	public int pad() { return 4; } // pixels
	
	// abstract stuff
	abstract public IntControl nRowsCntl();
	abstract public boolean isBlank(int row);
	abstract public GControl makeGCntl(int r, int c);

	// GLineTable.Listener
	public static interface Listener {
	    public void tableSizeChanged(GLineTable l);
	}	
}

