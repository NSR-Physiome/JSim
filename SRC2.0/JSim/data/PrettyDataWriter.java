/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// write data in pretty table/cell format

package JSim.data;

import JSim.util.*;
import JSim.expr.*;
import java.io.*;
import java.net.URL;
import java.util.ArrayList;

public class PrettyDataWriter extends DataWriter.Blocked {

	// state
	private PrintWriter writer;
	private ArrayList<Block> blocks;  // data blocks
	private int[] cellWidths;
	private int cell;	// current cell

	// constructor 
	public PrettyDataWriter(PrettyDataFormat f) { 
	    super(f);
	    blocks = new ArrayList<Block>();
	}

	// write all data
	public void writeData(Writer wrt, Data.List dlist) throws Xcept {
	    setBlocks(dlist);
	    writeData(wrt, 0, nlines());
	}
	
	// set data blocks
	public void setBlocks(Data.List dlist) throws Xcept {
	    blocks = new ArrayList<Block>();

	    // segregate data by dimension
	    Data.List dlist0 = new Data.List(dlist.size());
	    Data.List dlistG = new Data.List(dlist.size());
	    Data.List dlist1 = new Data.List(dlist.size());
	    Data.List dlist2 = new Data.List(dlist.size());
	    ArrayList<Data.List> alist3 = new ArrayList<Data.List>();
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);

		// store unused grids
		if (data instanceof GridData) {
		    if (!ndataHasGrid(dlist, (GridData) data))
			dlistG.add(data);
		    continue;
		}

		// 0D, 1D, 2D RealNData
		switch (data.ndim()) {
		case 0:
		    dlist0.add(data);
		    continue;
		case 1:
		    dlist1.add(data);
		    continue;
		case 2:
		    dlist2.add(data);
		    continue;
		}

		// 3D+ sorted by Grids into alist3
		Data.List dlist3 = null;
		for (int j=0; j<alist3.size(); j++) {
		    Data.List jlist = (Data.List) alist3.get(j);
		    if (! data.hasSameGridOrder(jlist.data(0)))
			continue;
		    dlist3 = jlist;
		    break;
		}
		if (dlist3 == null) {
		    dlist3 = new Data.List(4);
		    alist3.add(dlist3);
		}
		dlist3.add(data);
	    }

	    // 0D block
	    if (dlist0.size() > 0)
	    	blocks.add(new Block0(dlist0));

	    // domain blocks
	    for (int i=0; i<dlistG.size(); i++) {
		Data.List dblock = new Data.List(1);
		dblock.add(dlistG.data(i));
		blocks.add(new Block1(dblock));
	    }

	    // 1D blocks max 6 columns
	    int ncols=6;
	    int ntbl = (dlist1.size()+ncols-1) / ncols;
	    for (int i=0; i<ntbl; i++) {
		Data.List dblock = new Data.List(ncols);
		for (int j=i*ncols; j<(i+1)*ncols; j++) 
		    if (j<dlist1.size())
			dblock.add(dlist1.data(j));
		blocks.add(new Block1(dblock));
	    }

	    // 2D blocks
	    for (int i=0; i<dlist2.size(); i++) {
		Data.List dblock = new Data.List(1);
		dblock.add(dlist2.data(i));
		blocks.add(new Block2(dblock));
	    }
	
	    // 3D+ blocks
	    for (int i=0; i<alist3.size(); i++) {
		Data.List dblock = (Data.List) alist3.get(i);
		blocks.add(new BlockN(dblock));
	    }
	}

	// does some NData in list have this same grid?
	private boolean ndataHasGrid(Data.List dlist, 
	GridData grid) throws Xcept {
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		if (data instanceof GridData) continue;
		if (data.hasSameGrid(grid))
		    return true;
 	    }
	    return false;
	}

	// write some data
	public void writeData(Writer wrt, int lmin, int lmax) throws Xcept {
	    writer = new PrintWriter(wrt);
	    for (int i=0; i<blocks.size(); i++) {
		Block block = block(i);
		int bmin = Math.max(block.lmin(), lmin) - block.lmin();
		int bmax = Math.min(block.lmax(), lmax) - block.lmin();
		if (bmax > bmin) {
//System.err.println("write block " + bmin + " to " + bmax);
		    block.write(bmin, bmax);
		}
		bmax = Math.min(block.lmax()+1, lmax) - block.lmin();
		if (bmax > bmin)
		    println();
	    }
	}

	// query
	public int nlines() {
	    int n = blocks.size()-1;
	    return (n<0) ? 0 : block(n).lmax();
	}
	public int width() {
	    int w = 0;
	    for (int i=0; i<blocks.size(); i++) 
		w = Math.max(w, block(i).width());
	    return w;
	}
	private Block block(int i) { return (Block) blocks.get(i); }

	// write preformatted text
	private void println(String s) {
	    writer.println(s);
	    cell = 0;
	}
	private void println() { 
	    println("");
	}

	// write text into a cell
	private void printCell(String s) {
	    if (s == null) s = "";
	    int slen = s.length();
	    int clen = numCellWidth();
	    if (cellWidths != null) {
		if (cell>=cellWidths.length)
		    cell = cellWidths.length-1;
		clen = cellWidths[cell++];
	    }
	    clen -= slen;
	    if (clen < 0) clen = 0;
	    int c1len = clen/2;
	    int c2len = clen - c1len;
	    StringBuffer s1 = new StringBuffer();
	    for (int i=0; i<c1len; i++) s1.append(" ");
	    s1.append(s);
	    for (int i=0; i<c2len; i++) s1.append(" ");
	    writer.print(s1.toString());
	}

	// write double into a cell
	private void printCell(double d) {
	    printCell(pretty(d));
	}

	// write a unit into a cell
	private void printCell(Unit u) {
	    String s = (u==null) ? "" : u.pubName();
	    if (s.equals(Unit.dimless)) s = "";
	    printCell(s);
	}

	// numeric cell width
	private int numCellWidth() { 
	    return precision() + 7;
	}

	// set cell widths
	private void setCellWidths(int[] w) { cellWidths = w; }

	// get grid
	private GridData grid0(Data data) throws Xcept {
	    if (data instanceof GridData) 
		return (GridData) data;
	    if (! (data instanceof RealNData))
		throw new Xcept(data, "Unsupported Data class");
	    RealNData ndata = (RealNData) data;
	    if (ndata.ndim() == 0) return null;
	    if (ndata.ndim() == 1) return ndata.grid(0);
	    throw new Xcept(ndata,
		"Dimension to large for 1D tabular output");
	}

	//// abstract data block class
	public abstract class Block {
	    public Data.List dlist;
	    private int lmin;

	    // constructor
	    public Block(Data.List d) {
		dlist = d;
		lmin = nlines();
		if (lmin > 0) lmin++; // skip line between blocks
	    }

	    // abstract methods
	    public abstract void write(int wmin, int wmax) throws Xcept;
	    public abstract int lct();
	    public abstract int width();

	    // query
	    public int lmin() { return lmin; }
	    public int lmax() { return lmin + lct(); }
	}

	//// 0D scalar data block
	public class Block0 extends Block {

	    // constructor
	    public Block0(Data.List d) { super(d); }

	    // query
	    public int lct() { return dlist.size(); }
	    public int width() { return 30 + numCellWidth(); }

	    // write data
	    public void write(int wmin, int wmax) throws Xcept {
	    	setCellWidths(new int[] { 30, numCellWidth() });
		for (int i=wmin; i<wmax; i++) {
		    Data data = dlist.data(i);
	    	    cell=0;
	    	    printCell(data.glegend());
	    	    double val = data.realVal(0);
	    	    printCell(val);
	    	    printCell(data.unit());
	    	    println();
		}
	    }
	}

	//// 1D table block
	public class Block1 extends Block {
	    private GridData t; // combined grid
	    private int legw;  // horiz space for legend
	    private int nlegs;  // # legend lines

	    // constructor
	    public Block1(Data.List d) throws Xcept { 
		super(d); 
		
	    // create combined data grid t
	    t = null;
	    for (int i=0; i<dlist.size(); i++) {
		GridData t0 = grid0(dlist.data(i));
		if (t0 == null) continue;
		if (t == null) { t = t0; continue; }
		t = t.combine(t0);
	    }
	    if (t == null) throw new Xcept(
		"No GridData for 1D tabular output");

	    // calculate legend space
	    legw = numCellWidth()-2;
	    int legMax = 1;
	    for (int i=0; i<dlist.size(); i++) {
		String l = dlist.data(i).glegend();
		legMax = Math.max(legMax, l.length());
	    }
	    nlegs = legMax/legw + 1;
	    }

	    // query
	    public int lct() { return t.ct() + nlegs + 1; }
	    public int width() { 
		return numCellWidth() * (1 + dlist.size());
	    }

	    // write data
	    public void write(int wmin, int wmax) throws Xcept {
	    	setCellWidths(new int[] { numCellWidth() });

	    	// legend line(s)
		int legmin = wmin;
		int legmax = Math.min(wmax, nlegs);
	    	for (int l=legmin; l<legmax; l++) {
	    	    printCell(l==0 ? t.glegend() : "");
		    for (int i=0; i<dlist.size(); i++) {
			if (dlist.data(i) instanceof GridData)
			    continue;
		    	String l0 = dlist.data(i).glegend();
		    	int i0 = l*legw;
		    	int i1 = Math.min(i0+legw, l0.length());
		    	String l1 = (i1>i0) ?
			    l0.substring(i0, i1) : "";
		    	printCell(l1);
		    }
	    	    println();
	    	}
	    
	    	// units line
		if (wmin <= nlegs && wmax > nlegs) {
	    	    printCell(t.unit());
	    	    for (int i=0; i<dlist.size(); i++) {
			if (dlist.data(i) instanceof GridData)
			    continue;
		    	printCell(dlist.data(i).unit());
		    }
	    	    println();
		}
	    
	    	// numeric data
		int tmin = Math.max(wmin-nlegs-1, 0);
		int tmax = Math.min(wmax-nlegs-1, t.ct());
	    	for (int i=tmin; i<tmax; i++) {
		    double tval = t.realVal(i);
		    printCell(tval);
		    for (int j=0; j<dlist.size(); j++) {
		    	Data data = dlist.data(j);
			if (data instanceof GridData)
			    continue;
		    	GridData t0 = grid0(data);
		    	if (t0 == null) {
			    printCell(tval);
			    continue;
		    	}
		        int t0x = t0.nearestInx(tval);
		    	if (t0.realVal(t0x) != tval) {
			    printCell("*");
			    continue;
		    	}
		    	double val = (data instanceof GridData) ?
			    tval : ((RealNData) data).realVal(t0x);
	 	    	printCell(val);
		    }
		    println();
	    	}		
	    }
	}

	//// 2D tabular data block
	public class Block2 extends Block {
	    private RealNData data;
	    private GridData t;
	    private GridData x;

	    // constructor
	    public Block2(Data.List d) { 
		super(d); 
	        data = (RealNData) dlist.data(0);
		t = data.grid(0);
		x = data.grid(1);
	    }

	    // query
	    public int lct() { return 4 + t.ct(); }
	    public int width() {
		return numCellWidth() * (1 + x.ct());
	    }

	    // write data
	    public void write(int wmin, int wmax) throws Xcept {
	    	setCellWidths(new int[] { numCellWidth() });
	    	cell=0;

		// data legend/units line
		if (wmin <= 0 && wmax > 0) {
		    printCell(data.glegend());
		    printCell(data.unit());
		    println();
		}

		// t/x legend line
		if (wmin <= 1 && wmax > 1) {
		    printCell(t.glegend());
		    printCell(x.glegend());
		    println();
		}

		// t/x units line
		if (wmin <= 2 && wmax > 2) {
		    printCell(t.unit());
		    printCell(x.unit());
		    println();
		}

		// x values line
		if (wmin <= 2 && wmax > 2) {
		    printCell("");
		    for (int i=0; i<x.ct(); i++) 
			printCell(x.realVal(i));
		    println();
		}

		// t and data values lines
		int tmin = Math.max(0, wmin-4);
		int tmax = wmax-4;
		int[] tx = new int[2];
		for (int i=tmin; i<tmax; i++) { 
		    printCell(t.realVal(i));
		    tx[0] = i;
		    for (int j=0; j<x.ct(); j++) {
			tx[1] = j;
			int k = data.inx(tx);
			int[] gpos = data.gridPos(k);
			double val = data.realVal(k);
			printCell(val);
		    }
		    println();
	        }
	    }
	}

	//// 3+D list data block
	public class BlockN extends Block {

	    // constructor
	    public BlockN(Data.List d) { super(d); }

	    // query
	    public int lct() { 
		return dlist.data(0).nsamples() + 3; 
	    }
	    public int width() {
	    	RealNData data0 = (RealNData) dlist.data(0);
		int ct = data0.ndim() + dlist.size();
		return numCellWidth() * ct;
	    }

	    // write data
	    public void write(int wmin, int wmax) throws Xcept {

	    	// initialize cell layout
	    	setCellWidths(new int[] { numCellWidth() });
	    	cell=0;
	    	RealNData data0 = (RealNData) dlist.data(0);
	    	int ndim = data0.ndim();
	    	int ndat = dlist.size();

	    	// names line
		if (wmax > 0) {
		    for (int i=0; i<ndim; i++) 
			printCell(data0.grid(i).glegend());
		    for (int i=0; i<ndat; i++) 
			printCell(dlist.data(i).glegend());
		    println();
		}

	    	// units line
		if (wmin <= 1 && wmax > 1) {
		    for (int i=0; i<ndim; i++) 
			printCell(data0.grid(i).unit());
		    for (int i=0; i<ndat; i++) 
			printCell(dlist.data(i).unit());
		    println();
		}

	    	// values lines
		int vmin = Math.max(0, wmin-2);
		int vmax = Math.min(wmax-2, data0.nsamples());
	    	for (int i=vmin; i<vmax; i++) {
	    	    int[] gpos0 = data0.gridPos(i);
	    	    for (int j=0; j<ndim; j++) {
		    	GridData grid = data0.grid(j);
		    	printCell(grid.realVal(gpos0[j]));
		    }
	    	    for (int j=0; j<ndat; j++) {
		    	RealNData data = (RealNData) dlist.data(j);
		    	printCell(data.realVal(i));
		    }
	    	    println();
	    	}
	    }
	}
}
