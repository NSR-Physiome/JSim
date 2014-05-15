/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// post-run every Nth point data storage

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 
import java.util.*;

public final class RTDataStoreNth {
	private RTDataStore store;
	private Hashtable<RegularGridData, RegularGridData> newGrids; // grids by name
	private Hashtable<RegularGridData, Integer> gridNths;
	private Hashtable<RegularGridData, Integer> gridFlops;
	
	// constructor
	public RTDataStoreNth(RTDataStore store) {
	    this.store = store;
	    newGrids = new Hashtable<RegularGridData, RegularGridData>();
	    gridNths = new Hashtable<RegularGridData, Integer>();
	    gridFlops = new Hashtable<RegularGridData, Integer>();
	}
	
	// get every nth data
	protected Data getData(RTContext ctxt, Data data) throws Xcept {
	    if (data == null) return data;
	    if (data instanceof RegularGridData)
	    	return getGridData(ctxt, (RegularGridData) data);
	    if (data instanceof RealNData)
	    	return getNData(ctxt, (RealNData) data);
	    throw new Xcept("RTDataSotreNth doesn't support " 
	    	+ data.getClass());
	}

	// get every nth data from a grid
	private GridData getGridData(RTContext ctxt, RegularGridData grid) throws Xcept {
	    RegularGridData ngrid = newGrids.get(grid);
	    if (ngrid != null) return ngrid;
	    
	    // check if reduce grid
	    int nth = getNth(ctxt, grid);
	    if (nth < 2) {
	    	newGrids.put(grid, grid);
		return grid;
	    }

	    // create reduced grid
	    // System.err.println("RTDataStoreNth creating grid " + grid.desc());
	    int dct = (grid.ct()+nth-1)/nth;
	    int flop = (grid.ct()-1) % nth;
	    double dmin = grid.min() + flop* grid.delta();
	    double dmax = grid.max();
	    ngrid = new RegularGridData(
		grid.name(), grid.unit(), dmin, dmax, dct);
	    ngrid.setName(grid.desc());
	    ngrid.setGroup(grid.group());
	    newGrids.put(grid, ngrid);
	    gridNths.put(grid, nth);
	    gridFlops.put(grid, flop);
	    return ngrid;
	}
	    
	// get Nth from RegularGridData
	private int getNth(RTContext ctxt, RegularGridData grid) throws Xcept {
	    String xname = grid.desc();
	    RTVar v = model().getVar(xname);
	    if (! (v instanceof RTRealDomain)) return 1;
	    RTRealDomain x = (RTRealDomain) v;
	    return ctxt.getPostNth(x);
	}
	    
	// get every nth data from a RealNData
	private RealNData getNData(RTContext ctxt, RealNData odata) 
	throws Xcept {
	    // create new grid array
	    int ndim = odata.ndim();
	    GridData[] ogrids = new GridData[ndim];
	    GridData[] ngrids = new GridData[ndim];
	    boolean reduced = false;
	    for (int i=0; i<ndim; i++) {
	    	ogrids[i] = odata.grid(i);
		ngrids[i] = getGridData(ctxt, (RegularGridData) ogrids[i]);
		if (ngrids[i].ct() < ogrids[i].ct())
		    reduced = true;
		if (ngrids[i] == null) {
		    // System.err.println("  bad grid: " + ogrids[i].desc());
		    return null;
		}
	    }
	    if (! reduced) return odata;

	    // create new RealNData
	    RealNData ndata = new RealNData(odata.desc(), 
	    	odata.unit(), ngrids);
	    ndata.setName(odata.desc());
	    ndata.setGroup(odata.group());

	    // fill in ndata samples
	    for (int i=0; i<ndata.nsamples(); i++) {
	    	int[] npos = ndata.gridPos(i);
		int[] opos = new int[ndim];
		for (int j=0; j<ndim; j++) {
		    GridData grid = odata.grid(j);
		    int nth = gridNths.get(grid);
		    int flop = gridFlops.get(grid);
		    opos[j] = flop + nth*npos[j];
		}
		double d = odata.realVal(opos);
//		System.err.println("npos " + Util.pretty(npos) 
//		    + " opos=" + Util.pretty(opos) + " val=" + d);
		ndata.set(i, d);
 	    }

	    return ndata;
	}

	// simple query
	public RTModel model() { return store.model(); }
}


