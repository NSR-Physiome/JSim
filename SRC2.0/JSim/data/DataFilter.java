/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// data filters

package JSim.data;
import JSim.util.*;
import java.util.ArrayList;

public class DataFilter {

	// filter type definitions
	public static final int CROP = 0;
	public static final int RESAMPLE = 1;
	public static final int BLOCK = 2;
	public static final int NORMAL = 3;

	// DataFilter
	protected int type;  // CROP, RESAMPLE, BLOCK, NORMAL
	protected int dim;	// dimension to filter
	protected double min;	// min grid value
	protected double max;	// max grid value
	protected double delta; // new grid delta value

	// constructor
	public DataFilter(int t) { 
	    type = t;
	}

	// set properties
	public void setType(int t) { type = t; }
	public void setDim(int d) { dim = d; }
	public void setMin(double m) { min = m; }
	public void setMax(double m) { max = m; }
	public void setDelta(double d) { delta = d; }

	// filter a data item
	public Data filter(Data data) throws Xcept {
	    if (dim<0 || dim>= data.ndim()) throw new Xcept(data,
		"Invalid filter dimension specified");
	    switch (type) {
	    case CROP: return crop(data);
	    case RESAMPLE: return resample(data);
	    default: throw new Xcept(data,
		"Data filter type " + type + " not supported");
	    }
	}

	// crop a data item
	private Data crop(Data data) throws Xcept {
	    GridData t0 = data.grid(dim);
	    if (t0.min() >= min && t0.max() <= max)
		return data;

	    // calculate new grid
	    int imin = t0.nearestInx(min);
	    int imax = t0.nearestInx(max);
	    if (imin>=imax) throw new Xcept(data,
		"Insufficient data within crop window");
	    GridData t1 = null;
	    if (t0 instanceof RegularGridData) {
	    	double nmin = t0.realVal(imin);
	    	double nmax = t0.realVal(imax);
		t1 = new RegularGridData(t0.desc(), t0.unit(),
		    nmin, nmax, imax-imin+1);
	    } else {
		double[] samp0 = t0.samples();
		double[] samp1 = new double[imax-imin+1];
		for (int i=imin; i<=imax; i++) 
		    samp1[i] = samp0[i-imin];
		t1 = new IrregularGridData(t0.desc(), 
		    t0.unit(), samp1);
	    }

	    // resample
	    return resample(data, t1);
	}

	// resample a data item
	private Data resample(Data data) throws Xcept {
	    GridData t0 = data.grid(dim);
	    double nmin = Math.max(min, t0.min());
	    double nmax = Math.min(max, t0.max());
	    int ct = ((int) ((nmax-nmin)/delta + 1.5));
	    nmax = nmin + (ct-1)*delta;
	    GridData t1 = new RegularGridData(t0.desc(),
		t0.unit(), nmin, nmax, ct);
	    return resample(data, t1);
	}

	// resample data at given grid
	private Data resample(Data data, GridData t1) 
	throws Xcept {
	    // handle non-RealNData
	    if (data instanceof GridData) {
		t1.setName(data.name());
		return t1;
	    }
	    if (! (data instanceof RealNData)) throw new Xcept(
		data, "DataFilter.crop() does not support " +
		data.getClass());

	    // resample RealNData
	    GridData[] grids = new GridData[data.ndim()];
	    for (int i=0; i<grids.length; i++) 
		grids[i] = (i == dim) ? t1 : data.grid(i);
	    Data rdata = new RealNData((RealNData) data, grids, false);
	    rdata.setName(data.name());
	    return rdata;		
	}

}

	
