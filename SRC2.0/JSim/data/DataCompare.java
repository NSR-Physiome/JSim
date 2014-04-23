/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// data comparison calculations

package JSim.data;
import JSim.util.*;
import java.util.ArrayList;
import java.io.Serializable;

public class DataCompare implements DiagInfo {

	// state vars
	private Data.List data;  // model data to analyze
	private Data.List refs;  // reference data
	private Data.List pointWgts; // point weights
	private double[] curveWgts; // curve weights

	// constructors
	public DataCompare(Data.List r) {
	    refs = r;
	}
	public DataCompare(Info info) throws Xcept {
	    data = new Data.List(info.data);
	    if (info.refs != null)
	    	refs = new Data.List(info.refs);
	    if (info.pointWgts != null)
	    	pointWgts = new Data.List(info.pointWgts);
	    curveWgts = info.curveWgts;
	}

	// independent copy
	public DataCompare copy() throws Xcept {
	    DataCompare c = new DataCompare(refs.copy());
	    if (data != null) c.data = data.copy();
	    if (pointWgts != null) c.pointWgts = pointWgts.copy();
	    if (curveWgts != null) {
		c.curveWgts = new double[curveWgts.length];
		for (int i=0; i<curveWgts.length; i++)
		    c.curveWgts[i] = curveWgts[i];
	    }
	    return c;
	}

	// query
	public int n() { return refs.size(); }
	public String diagInfo() { 
	    return "Data Residual";
	}
	public Data.List data() { return data; }
	public Data.List refs() { return refs; }
	public Data.List pointWgts() { return pointWgts; }
	public double[] curveWgts() { return curveWgts; }
	public Info info() throws Xcept { return new Info(this); }

	// set internal data
	public void setData(Data.List list) throws Xcept {
	    data = matchRefs(list);
	}
	public void setPointWgts(Data.List list) throws Xcept {
	    pointWgts = matchRefs(list);
	}
	public void setCurveWgts(double[] w) throws Xcept {
	    if (w != null && w.length != n()) 
		throw new Xcept(this,
		"curve weights array mismatch");
	    curveWgts = w;
	}

	// interpolate olist onto refs grids (if needed)
	public Data.List matchRefs(Data.List olist) throws Xcept {
	    if (olist == null) return null;
	    if (olist.size() != n()) throw new Xcept(this, 
		"mismatched data list length");
	    Data.List nlist = new Data.List(n());
	    for (int i=0; i<n(); i++) {
		Data rdata = refs.data(i);
		Data odata = olist.data(i);
		Data ndata = odata;

		// check dims, 0D data OK (same for all points)
		if (ndata.ndim() == 0) {
		    nlist.add(ndata);
		    continue;
		}
		if (odata.ndim() != rdata.ndim()) 
		    throw new Xcept(this,
			"mismatched data dimension");
		int ndim = odata.ndim();

		// resample if necessary
		GridData[] ngrids = new GridData[ndim];
		boolean resample = false;
		for (int j=0; j<ndim; j++) {
		    ngrids[j] = rdata.grid(j);
		    if (! rdata.grid(j).sameSamplesAs(odata.grid(j)))
			resample = true;
		}
		if (resample) {
		    if (ndata instanceof RealNData)
			ndata = new RealNData(
			    (RealNData) odata, ngrids, true);
		    else 
			ndata = rdata.grid(0);
		}
		nlist.add(ndata);
	    }
	    return nlist;
	}

	// RMS error between data and refs
	public double rmsError() throws Xcept {
	    if (data == null) throw new Xcept(this,
		"data not specified");
	    double curveWgtTot = 0;
	    double tot = 0;
	    for (int i=0; i<data.size(); i++) {
		double itot = 0;
		Data d = data.data(i);
		Data r = refs.data(i);
		Data p = (pointWgts != null) ?
		    pointWgts.data(i) : null;
		int n = r.nsamples();
		double pointWgtTot= 0;
		for (int j=0; j<n; j++) {
		    double dval = (d.ndim() == 0) ?
			d.realVal(0) : d.realVal(j);
		    if (Double.isNaN(dval)) continue;
		    double del = dval - r.realVal(j);
		    del = del*del;
		    double pwgt = 1;
		    if (p != null) 
			pwgt = (p.ndim() == 0) ?
			    p.realVal(0) : p.realVal(j); 
		    del *= pwgt;
		    itot += del;
		    pointWgtTot += pwgt;
		}
		if (pointWgtTot == 0) throw new Xcept(
		    "rmsError: point weights total=0. Perhaps misaligned model/ref domains?");
		itot = itot / pointWgtTot;
	        double cwgt = (curveWgts != null) ?  
		    curveWgts[i] : 1;
		itot *= cwgt;
		tot += itot;
		curveWgtTot += cwgt;
	    }
	    if (curveWgtTot != 0)
		tot = tot / curveWgtTot;
	    return Math.sqrt(tot);
        }

	// residuals between data and refs
	public Data.List residuals(boolean weighted) throws Xcept {
	    Data.List list = new Data.List(refs.size());
	    for (int i=0; i<refs.size(); i++)
		list.add(residual(i, weighted));
	    return list;
	}

	// one residual
	public RealNData residual(int i, boolean weighted) throws Xcept {
	    if (data == null) throw new Xcept(this,
		"data not specified");
	    Data d = data.data(i);
	    Data r = refs.data(i);
	    Data p = (pointWgts != null && weighted) ?
		pointWgts.data(i) : null;
	    
	    double cwgt = (curveWgts != null && weighted) ?
		curveWgts[i] : 1;
	    GridData[] grids = new GridData[r.ndim()];
	    for (int j=0; j<grids.length; j++) 
		grids[j] = r.grid(j);
	    String desc = d.legend() + " - " + r.legend();
	    RealNData resid = new RealNData(desc, 
		r.unit(), grids);
	    int ct = r.nsamples();
	    for (int j=0; j<ct; j++) {
		double x = d.realVal(j) - r.realVal(j);
		if (p != null) {
		    double pwgt = (p.ndim() == 0) ?
			p.realVal(0) : p.realVal(j);
		    x *= pwgt;
		}
	        x *= cwgt;
	        resid.set(j, x);
	    }
	    return resid;
	}

	// Serializable Info
	public static class Info implements Serializable {
	    public DataInfo[] data;  // data to analyze
	    public DataInfo[] refs;  // reference data
	    public DataInfo[] pointWgts; // point weights
	    public double[] curveWgts; // curve weights

	    // constructor
	    public Info(DataCompare c) throws Xcept {
		data = c.data.info();
		if (c.refs != null) 
		    refs = c.refs.info();
		if (c.pointWgts != null) 
		    pointWgts = c.pointWgts.info();
		curveWgts = c.curveWgts;
	    }
	}		
}
