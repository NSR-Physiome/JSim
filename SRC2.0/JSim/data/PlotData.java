/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// N-dimensional Plot data: XY, XYZ or higher

package JSim.data;
import JSim.util.*;

public class PlotData extends Data {

	// state
	private RealNData base; // base data
	private Data[] data; // data overriding grids
	private GridData[] grids; // data overriding grids

	// data wrap constructor
	public PlotData(Data b) throws Xcept {
	    super(b.name(), b.desc(), b.unit());
	    if (b instanceof RealNData) {
		base = (RealNData) b;
	    } else if (b instanceof GridData) {
		base = new RealNData((GridData) b);
	    } else throw new Xcept(b,
		"Unsupported base class in PlotData constructor");
	    grids = new GridData[ndim()];
	    for (int i=0; i<ndim(); i++)
		grids[i] = base.grid(i);
	    data = new Data[ndim()+1];
	}

	// grid resample constructor
	public PlotData(Data b, Data[] d) throws Xcept {
	    this(b);
	    data = d;
	    if (data.length != ndim()) throw new Xcept(
		"Data array length wrong in PlotData constructor");

	    // update grids
	    boolean resamp = false;
	    for (int i=0; i<ndim(); i++) {
		if (data[i] == null) continue;
		if (data[i].ndim() != 1) throw new Xcept(data[i],
		    "Incorrect data dimension in PlotData constructor");
		if (data[i] instanceof GridData)
		    grids[i] = (GridData) data[i];
		else if (data[i] instanceof RealNData)
		    grids[i] = ((RealNData) data[i]).grid(0);
		else throw new Xcept(data[i],
		    "Unsupported data class in PlotData constructor");
		if (! grids[i].sameSamplesAs(base.grid(i)))
		    resamp = true;
	    }

	    // resample ?
	    if (resamp)
		base = new RealNData(base, grids, false);
	}

	// calculate extrema
	public void calcExtrema() {
	    base.calcExtrema();
	    for (int i=0; i<data.length; i++) 
	        if (data[i] != null)
		    data[i].calcExtrema();
	}

	// query
	public DataInfo info(Subset sset) { return null; }
	public int ndim() { return base.ndim(); }
	public int nsamples() { return base.nsamples(); }
	public double[] samples() throws Xcept {
	    return base.samples();
	}
	public double min() { return base.min(); }
	public double minpos() { return base.minpos(); }
	public double max() { return base.max(); }
	public GridData grid(int i) { return grids[i]; }
	public RealNData base() { return base; }
	public Data data(int i) {
	    if (i > ndim()) return null;
	    if (i == ndim()) return base;
	    return (data[i] == null) ? grids[i] : data[i];
	}
	public double[] samples(int i) throws Xcept { 
	    return data(i).samples();
	}
	public int nsamples(int i) { return data(i).nsamples();	}
	public double realVal(int i) throws Xcept { return base.realVal(i); }
	public double realVal(double[] vals) throws Xcept {
	    return base.realVal(vals); // ???
	}
	public String legendGrids() { return ""; }

	// add useful RealNData to Data.List
	public void addData(Data.List list) {
	    for (int i=0; i<=ndim(); i++)
		if (data(i) instanceof RealNData)
		    list.add(data(i));
	}
}
