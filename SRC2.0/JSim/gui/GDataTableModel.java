/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim Data class TableModel

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;


public class GDataTableModel extends AbstractTableModel 
implements CellEditorListener {
	private int mode;
	private int nrows, ncols;
	private Data.List datas;
	private GridData[] grids;

	// table mode constants
	private static final int EMPTY = 0;  // no data
	private static final int SCALAR = 1;  // name, y
	private static final int COLUMN = 2; // x, y1, y2, ...
	private static final int TABLE = 3; // 2D (x,y) 
	private static final int MULTI = 4; // x1, x2, ... y1, y2...
	
	// constructor
	public GDataTableModel() {
	    super();
	    clear();
	}
	
	// clear table
	public void clear() { 
	   datas = null;
	   grids = null;
	   setMode(EMPTY); 
	}

	// table changed
	public void tableChanged(TableModelEvent e) {
	    System.err.println("" + e);
	}

	// load data into table
	public void loadData(Data.List list) throws Xcept {
	    clear();
	    if (list.size() == 0) return;
	    datas = list;
	    Data d0 = data(0);
	    grids = new GridData[d0.ndim()];
	    for (int i=0; i<grids.length; i++)
	    	grids[i] = d0.grid(i);

	    // dimensional consistency checks
	    for (int i=1; i<ndata(); i++) {
		Data d = data(i);
		if (d.hasSameGridOrder(d0)) continue;
		clear();
		throw new Xcept(d, d0,
		    "Data curves have incompatible dimensions for coediting");
	    }		    

	    // format depends on #grids & #data
	    switch (ngrids()) {
	    case 0:  setMode(SCALAR); break;
	    case 1:  setMode(COLUMN); break;
	    case 2:  setMode(ndata()==1 ? TABLE : MULTI); break;
	    default: setMode(MULTI); break;
	    }    	
	}

	// set mode
	private void setMode(int m) {
	    mode = m;
	    switch(mode) {
	    case EMPTY:
	    	nrows = 0;
		ncols = 1;
		break;
	    case SCALAR:
	    	nrows = ndata();
		ncols = 2;
		break;
	    case COLUMN:
		nrows = grid(0).ct();
	        ncols = 1 + ndata();
		break;
	    case TABLE:
	    	nrows = vertTableGrid().ct() + 1;
	    	ncols = horizTableGrid().ct() + 1;
		break;
	    case MULTI:
	    	nrows = data(0).nsamples();
		ncols = ngrids() + ndata();
		break;
	    }
	    fireTableStructureChanged();
	}

	// simple query
	public int ngrids() { 
	    return (grids == null) ? 0 : grids.length; 
	}
	public int ndata() {
	    return (datas == null) ? 0 : datas.size();
	}
	public GridData grid(int i) {
	    if (grids == null) return null;
	    return grids[i];
	};
	public Data data(int i) { 
	    if (datas == null) return null;
	    return datas.data(i); 
	}
	public String legend(Data data) {
	    if (data == null) return "?";
	    String s = data.name();
	    return (Util.isBlank(s)) ? data.desc() : s;
	}
	public GridData vertTableGrid() { return grid(0); }
	public GridData horizTableGrid() { return grid(1); }
	public int getColumnCount() { return ncols; }
	public int getRowCount() { return nrows; }
	public Object getValueAt(int r, int c) {
	    Data data = data(r, c);
	    if (data == null) return null;
	    if (ngrids() == 0 && c == 0)
		return legend(data);
	    int inx = inx(r, c);
	    try {
	        return Util.pretty(data.realVal(inx));
	    } catch (Exception e) {
	    	return null;
	    }
	}
	public Class getColumnClass(int c) { return String.class; }
	public boolean isCellEditable(int r, int c) {
	    if (data(r, c) == null) return false;
	    if (ngrids() == 0 && c == 0) return false;
	    if (inx(r, c) < 0) return false;
	    return true;
	}

	// column name
	public String getColumnName(int c) {
	    switch(mode) {
	    case EMPTY:  
	        return "Empty";
	    case SCALAR: 
	    	return (c==0) ? "name" : "value";
	    case COLUMN: 
	    	if (c==0) return legend(grid(0));
		return legend(data(c-1));
	    case TABLE:	
	    	return (c==0) ? legend(vertTableGrid()) : "";
	    case MULTI:
	    	if (c < ngrids())
		    return legend(grid(c));
		return legend(data(c-ngrids()));
	    default:
	    	return null;
	    }
	}

	// which data for row, column
	public Data data(int r, int c) {
	    switch(mode) {
	    case EMPTY:  return null;
	    case SCALAR: return data(r);
	    case COLUMN: 
	    	if (c==0) return grid(0);
		return data(c-1);
	    case TABLE:	
	    	if (c==0) return vertTableGrid();
		if (r==0) return horizTableGrid();
		return data(0);
	    case MULTI:
	    	if (c < ngrids())
		    return grid(c);
		return data(c-ngrids());
	    default:
	    	return null;
	    }
	}

	// which data sample inx for row, column
	//   -1 if inoperative
	public int inx(int r, int c) {
	    switch (mode) {
	    case EMPTY:
	    	return -1;
	    case SCALAR:
	    	return 0;
	    case COLUMN:
	    	return r;
	    case TABLE:
	    	if (c==0) return r-1;
		if (r==0) return c-1;
	    	return (c-1)*vertTableGrid().ct() + r-1;
	    case MULTI:
	        if (c >= ngrids()) return r;
		try { 
		    int[] gpos = data(0).gridPos(r);
		    return gpos[c];
		} catch (Xcept e) {
		    return -1;
		}
	    default:
	    	return -1;
	    }
	}

	// CellEditorListener
	public void editingStopped(ChangeEvent e) {
	    System.err.println("stopped:" + e);
	}
	public void editingCanceled(ChangeEvent e) {
	    System.err.println("canceled:" + e);
	}
	

}

