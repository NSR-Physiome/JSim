/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// n-dimensional sampled data

package JSim.data;
import JSim.util.*;
import JSim.expr.*;

public class RealNData extends Data {
	private GridData[] grids;  // grid domains for data
	private int ngrids;	// # grids
	private int nsamples;	// # samples
	private double[] samples; // samples
	private double min, minpos, max; // min, min positive, max samples
	private double close; // grid closeness criterion

	private int interp;	// Interp method, see below
	public final static int NEAREST_INTERP = 0;
	public final static int OLDLINEAR_INTERP = 1;
	public final static int VELDHUIZEN_INTERP = 2;
	public final static int MULTILINEAR_INTERP = 3;
	public final static int MULTILINEARNAN_INTERP = 4;

	// constructors
	public RealNData(String d, Unit u, GridData[] g) 
	throws Xcept {
	    super(d, u);
	    grids = g;
	    ngrids = (grids == null) ? 0 : grids.length; 
	    nsamples = 1;
	    close = 1;
	    for (int i=0; i<ndim(); i++) {
		GridData x = grids[i];
		if (x == null) throw new Xcept(d + 
		    " data grid undefined");
		nsamples *= x.ct();
		double xclose = (x.max()-x.min())/ (x.ct()-1);
		if (i==0 || xclose<close) close=xclose;
	    }
	    close = close * 1e-7;  // single-precision accuracy
	    samples = new double[nsamples];
	    for (int i=0; i<nsamples; i++) 
		samples[i] = Double.NaN;
	    interp = MULTILINEARNAN_INTERP;
	}
	public RealNData(String d, Unit u, 
	GridData[] g, double[] samp) throws Xcept {
	    this(d, u, g);
	    set(samp);
	}

	// convert grid to sampled
	public RealNData(GridData g) throws Xcept {
	    this(g.desc, g.unit, new GridData[] { g });
	    set(g.samples());
	}

	// resample grid
	public RealNData(RealNData base, GridData[] g,
	boolean nanOutOfRange) throws Xcept {
	    this(base.desc, base.unit, g);
	    if (ndim() != base.ndim()) throw new Xcept(
		"Data resampling dimesion conflict");

	    // fill in data
	    double[] gval = new double[ndim()];
	    for (int i=0; i<nsamples(); i++) {
		int[] gpos = gridPos(i);
		for (int j=0; j<ndim(); j++) 
		    gval[j] = grid(j).realVal(gpos[j]);
		samples[i] = base.realVal(gval);
		if (nanOutOfRange && base.outOfRange(gval))
		    samples[i] = Double.NaN;
	    }
	}

	// set data values
	public void set(int inx, double dat) {
	    samples[inx] = dat;
	}
	public void set(int[] gpos, double dat) throws Xcept {
	    int inx = inx(gpos);
	    samples[inx] = dat;
	}
	public void set(double dat) throws Xcept { 
	    if (ndim() != 0) throw new Xcept(this,
		"set() invalid for ndim()>0");
	    set(0, dat); 
	}
	public void set(double[] dat) throws Xcept {
	    if (dat.length != nsamples) throw new Xcept(this,
		"Mismatched array length in set()");
	    for (int i=0; i<nsamples; i++) 
		samples[i] = dat[i];
	}
	public void setSome(double[] dat) throws Xcept {
	    int n = Math.min(nsamples, dat.length);
	    for (int i=0; i<n; i++) 
		samples[i] = dat[i];
	}

	// subset import
	public void set(Subset sset, double[] dat) throws Xcept {
	    if (sset == null || ndim() != 1) {
		set(dat);
		return;
	    }

	    // 1D reduction
	    int hix = Math.min(sset.hix, samples.length-1);
	    for (int i=sset.lox; i<=hix; i++)
		samples[i] = dat[i-sset.lox];
	}

	// subset export
	public double[] samples(Subset sset) throws Xcept {
	    if (sset == null || ndim() != 1) return samples;
	    if (sset.lox > sset.hix) return samples; // ???

	    // 1D reduction
	    double[] dat = new double[sset.hix-sset.lox+1];
	    for (int i=sset.lox; i<=sset.hix; i++)
		dat[i-sset.lox] = (i>=samples.length) ?
		    Double.NaN : samples[i];
	    return dat;
	}

	// interp
	public void setInterp(int i) throws Xcept {
	    switch (i) {
	    case NEAREST_INTERP:
	    case OLDLINEAR_INTERP:
	    case VELDHUIZEN_INTERP:
	    case MULTILINEAR_INTERP:
	    case MULTILINEARNAN_INTERP:
		interp = i;	
		return;
	    }
	    throw new Xcept(this, "Unknown interpolation method");    
	}

	// query
	public DataInfo info(Subset sset) throws Xcept { 
	    return new Info(this, sset); 
	}
	public int ndim() { return ngrids; }
	public GridData grid(int i) { return grids[i]; }
	public int nsamples() { return nsamples; }
	public double realVal(int[] gpos) throws Xcept {
	    return samples[inx(gpos)];
	}
	public double realVal(int i) { return samples[i]; }
	public double realVal() throws Xcept { 
	    if (ndim() != 0) throw new Xcept(this,
		"realVal() invalid for ndim()>0");
	    return realVal(0); 
	}
	public double[] samples() { return samples; }
	public void calcExtrema() {
	    min = minpos = max = Double.NaN;
	    for (int i=0; i<samples.length; i++) {
		double d = samples[i];
		if (Double.isNaN(d)) continue;
		if (Double.isInfinite(d)) continue;
		if (Double.isNaN(min) || d<min) min = d;
		if (d>0 && (Double.isNaN(minpos) || d<minpos)) 
		    minpos = d;
		if (Double.isNaN(max) || d>max) max = d;
	    }
	}
	public double min() { return min; }
	public double minpos() { return minpos; }
	public double max() { return max; }

	// legend grids
	public String legendGrids() { 
	    if (ndim() == 0) return "";
	    String s = "(";
	    for (int i=0; i<ndim(); i++) {
		if (i>0) s = s + ",";
		GridData grid = grid(i);
		String gs = grid.name;
		if (Util.isBlank(gs)) gs = grid.desc;
		if (Util.isBlank(gs)) gs = "*";
		s = s + gs;
	    }
	    return s + ")";
	}

	// which interpolated algorithm?
	public double realVal(double[] vals) throws Xcept {
	    if (ndim() != vals.length) throw new Xcept(this,
	        "incorrect #args for RealNData interpolation");
	    switch (interp) {
	    case OLDLINEAR_INTERP:
	    	return realValOld(vals);
	    case VELDHUIZEN_INTERP:
	    	return realValVeldhuizen(vals);
	    case MULTILINEAR_INTERP:
		return realValMulti(vals);
	    case MULTILINEARNAN_INTERP:
		return realValMultiNaN(vals);
	    default: 
	    	throw new Xcept(this,
		    "Unsupported interpolation method=" + interp);
	    }
	}

	// original (flawed) interpolated query
	public double realValOld(double[] vals) throws Xcept {
	    int[] loarr = new int[ndim()];
	    int[] hiarr = new int[ndim()];
	    double frac = 0;
	    for (int i=0; i<ndim(); i++) {
		GridData x = grid(i);
		double val = vals[i];

		// j = low side index
		int j = x.nearestInx(val);
		double jval = x.realVal(j);
		if (j>0 && jval>val && val>x.min() && val<x.max()) 
		    jval = x.realVal(--j);

		// k = high side index
		int k = (j<x.ct()-1) ? j+1 : j;
		double kval = x.realVal(k);
		double xfrac = (j==k) ? 
		    0 : (val-jval)/(kval - jval);

		if (xfrac <= 0) {
		    xfrac = 0;
		    k=j;
		} else if (xfrac >= 1) {
		    xfrac = 0;
		    j=k;
		}

		// store
		loarr[i] = j;
		hiarr[i] = k;
		frac += xfrac*xfrac;
	    }
	    frac = Math.sqrt(frac);
	    double ret = 0;
	    if (frac < 1) ret += (1-frac)*realVal(loarr);
	    if (frac > close) ret += frac*realVal(hiarr); 
	    return ret;
	}

	// Multilinear interpolation
	public double realValMulti(double[] vals) throws Xcept {
	    if (ndim() > 32)
	    throw new Xcept(this,"Interpolation: more than 32 dimensions");
	    int[] loarr = new int[ndim()];
	    int[] hiarr = new int[ndim()];
	    int[] coord = new int[ndim()];
	    double[] frac = new double[ndim()];
	    for (int i=0; i<ndim(); i++) {
	        //get grid coordinate 
	        GridData x = grid(i);
	        double val = vals[i];

	        // j = low side index
	        int j = x.nearestInx(val);
	        double jval = x.realVal(j);
	        if (j>0 && jval>val && val>x.min() && val<x.max()) 
	            jval = x.realVal(--j);

	        // k = high side index
	        int k = (j<x.ct()-1) ? j+1 : j;
	        double kval = x.realVal(k);

	        // relative distance from lower node
	        double xfrac = (j==k) ? 
	        0 : (val-jval)/(kval - jval);
	        // val is close to node
	        if (xfrac <= close) {
	            xfrac = 0;
	            k=j;
	        } else if (xfrac >= 1 - close) {
	            xfrac = 0;
	            j=k;
	        }

	        // store
	        loarr[i] = j;
	        hiarr[i] = k;
	        frac[i] = xfrac; 
	    }
	    double w; 
	    double ret = 0.0;
	    // loop over all edges of enclosing hypercube
	    for (long i=0; i<(1 << ndim()); i++) {
	        w = 1.0; // basis function associated with grid point i
		// loop over edge coordinates
	        for (int j=0; j<ndim(); j++) {
                    if ((1 << j & i) == 0 ){
	                // the edge i is lower 
	                // if the corresponding bit in i is not set 
	                coord[j] = loarr[j]; 
	                w *= 1 - frac[j];
	            } else {
	                // the edge i is higher
	                // if the corresponding bit in i is set 
	                coord[j] = hiarr[j]; 
	                w *= frac[j];
	            }
	        }
	        ret += w*realVal(coord);
	    }
	    return ret;
	}

	// NaN protectected Multilinear interpolation/extrapolation
	public double realValMultiNaN(double[] ovals) throws Xcept {
	    int ndim = ndim();	    
	    if (ndim > 30)
	        throw new Xcept(this,"Interpolation does not support more than 30 dimensions");

	    // load bounding hypercube indexes: loarr, hiarr
	    double[] nvals = new double[ndim];
	    int[] loarr = new int[ndim];
	    int[] hiarr = new int[ndim];
	    for (int i=0; i<ndim; i++) {
	        GridData x = grid(i);
	        double val = ovals[i];
		double xdelta = 1e-7* (x.max() - x.min());
		if (val < x.min())
		    val = x.min();
		else if (val > x.max())
		    val = x.max();
		nvals[i] = val;
	        int j = x.nearestInx(val);
//System.err.println(x.desc() + ": nearestInx(" + val + ")=" + j + " realVal(" + j + ")=" + x.realVal(j));
		if (j>0 && x.realVal(j) + xdelta >= val) 
		   j--;
		else if (j == x.ct()-1) 
		   j--;
	        loarr[i] = j;
		hiarr[i] = j+1;
	    }

	    // back off if realVal(hiarr) is NaN
	    if (Double.isNaN(realVal(hiarr))) {
	        for (int i=0; i<ndim; i++) {
		    if (hiarr[i] <2) continue;
		    hiarr[i]--;
		    if (Double.isNaN(realVal(hiarr)))
		    	hiarr[i]++;
		    else 
			loarr[i]--;
		}
	    }
		    
	    // calculate fractions	    
	    double[] frac = new double[ndim];
	    for (int i=0; i<ndim; i++) {
	        GridData x = grid(i);
		double jval = x.realVal(loarr[i]);
		double kval = x.realVal(hiarr[i]);
		frac[i] = (nvals[i] - jval) / (kval - jval);		    
//System.err.println(x.desc() + " val=" + nvals[i] + " j=" + loarr[i] + 
//" k=" + hiarr[i] + " frac=" + frac[i]);
	    }

	    // sum frac weighted vals (vtot) and weights (wtot)
	    //    over all edges(vertices?) of hypercube
	    double wtot = 0;
	    double vtot = 0;
	    double vsave = Double.NaN;  // save a non-NaN value, if any
	    int[] coord = new int[ndim];
	    long npts = 1 << ndim;
	    for (long i=0; i<npts; i++) {
	        double w = 1.0; // basis function associated with grid point i
		// loop over edge coordinates
	        for (int j=0; j<ndim(); j++) {
                    if ((1 << j & i) == 0 ){
	                // the edge i is lower 
	                // if the corresponding bit in i is not set 
	                coord[j] = loarr[j]; 
	                w *= 1 - frac[j];
	            } else {
	                // the edge i is higher
	                // if the corresponding bit in i is set 
	                coord[j] = hiarr[j]; 
	                w *= frac[j];
	            }
	        }
	        double v = realVal(coord);
//System.err.println(desc() + ".realVal(" + Util.pretty(coord) + ")=" + v + " w=" + w);
	        if(!Double.isNaN(v)) {
	            vtot += v*w;
	            wtot += w;
		    vsave = v;
	        }
//System.err.println("  vtot=" + vtot + " wtot=" + wtot);
	    }
	    double ret = (wtot == 0) ? vsave : vtot/wtot;
//System.err.println("ret=" + ret);
	    return ret;
	}

	// interpolated query
	// stepwise linear interpolation
	// adapted from Todd Veldhuizen, 
	// Master's Theseis, Univ Waterloo MAY 1998
	// http://osl.iu.edu/~tveldhui/papers/MAScThesis/node33.html
	public double realValVeldhuizen(double[] vals) throws Xcept {
	    int[] loarr = new int[ndim()];
	    int[] hiarr = new int[ndim()];
	    IntDoub permu[] = new IntDoub[ndim()];
	    double frac = 0;
	    for (int i=0; i<ndim(); i++) {
	        //get grid coordinate 
	        GridData x = grid(i);
	        double val = vals[i];

	        // j = low side index
	        int j = x.nearestInx(val);
	        double jval = x.realVal(j);
	        if (j>0 && jval>val && val>x.min() && val<x.max()) 
	            jval = x.realVal(--j);

	        // k = high side index
	        int k = (j<x.ct()-1) ? j+1 : j;
	        double kval = x.realVal(k);

	        // relative distance from lower node
	        double xfrac = (j==k) ? 
	        0 : (val-jval)/(kval - jval);
	        // val is close to node
	        if (xfrac <= close) {
	            xfrac = 0;
	            k=j;
	        } else if (xfrac >= 1 - close) {
	            xfrac = 0;
	            j=k;
	        }

		// store
		loarr[i] = j;
		hiarr[i] = k;
		permu[i] = new IntDoub();
		permu[i].i = i;
		permu[i].a = xfrac;
	    }
	    java.util.Arrays.sort(permu);
	    int[] coord = loarr;
	    double coeff = 1.0;
	    double ret = 0.0;
	    for (int i=0; i<ndim(); i++) {
		int j;
		coeff -= permu[i].a;
		ret += coeff*realVal(coord);
		coeff = permu[i].a;
		j = permu[i].i;
		coord[j] = hiarr[j];
	    }
	    ret += coeff*realVal(coord);
	    return ret;
	}

	// sum over entire range
	public double sum() {
	    double tot = 0;
	    for (int i=0; i<nsamples; i++) {
		if (! Double.isNaN(samples[i]))
		    tot += samples[i];
	    }
	    return tot;
	}
	    
	// 1D sum over restricted range
	public double sum(double xmin, double xmax) throws Xcept {
	    if (ndim() != 1) throw new Xcept(
	    	"sum(min,max) supports only 1D data: " + this);
	    GridData xgrid = grid(0);
	    double tot = 0;
	    for (int i=0; i<nsamples; i++) {
	    	double x = xgrid.realVal(i);
		if (x < xmin || x > xmax) continue;
		double s = samples[i];
		if (! Double.isNaN(s))
		    tot += s;
	    }
	    return tot;
	}

	// integral (RegularGridData only, for now)
	public double integral() throws Xcept {
	    double tot = 0;
	    for (int i=0; i<nsamples; i++) {
		int[] gpos = gridPos(i);
		int wgt = 1;
		for (int j=0; j<ndim(); j++) {
		    if (gpos[j] == 0) continue;
		    if (gpos[j] == grid(j).ct()-1) continue;
		    wgt *= 2;
		}
		tot += realVal(i)*wgt;
	    }
	    for (int i=0; i<ndim(); i++) {
		if (! (grid(i) instanceof RegularGridData))
		    throw new Xcept(this,
			"integral() currently supports only equal-spaced grids");
		double xdelta = ((RegularGridData) grid(i)).delta();
	    	tot *= (xdelta/2);
	    }
	    return tot;
	}

	// 1D integral over restricted range
	public double integral(double xmin, double xmax) throws Xcept {
	    if (ndim() != 1) throw new Xcept(
	    	"integral(min,max) supports only 1D data: " + this);
	    if (! (grid(0) instanceof RegularGridData)) throw new Xcept(
	    	"integral(min,max) supports only regular grids: " + this);
	    RegularGridData xgrid = (RegularGridData) grid(0);
	    double delta2 = xgrid.delta() / 2;
	    if (xgrid.min() > xmin) xmin = xgrid.min();
	    if (xgrid.max() < xmax) xmax = xgrid.max();
	    double tot = 0;
	    for (int i=0; i<nsamples; i++) {
		double s = samples[i];
		if (Double.isNaN(s)) continue;
	    	double x = xgrid.realVal(i);
		double xlo = Math.max(xmin, x - delta2);
		double xhi = Math.min(xmax, x + delta2);
		if (xhi <= xlo) continue; 
		tot += s * (xhi - xlo);
	    }
	    return tot;
	}

	// serializable info
	public static class Info extends DataInfo {
	    public DataInfo[] grids;
	    public double[] samples;

	    // constructors
	    public Info() { }
	    public Info(RealNData data, Subset sset) throws Xcept {
		super(data);
		int n = data.ndim();
		if (n>0) {
		    grids = new DataInfo[n];
		    for (int i=0; i<n; i++) 
			grids[i] = data.grid(i).info();
		}
		samples = data.samples(sset);
		subset = sset;
	    }
	}

	// test program
	public static void main(String[] args) throws Exception{
	    int ndim = args.length/2;
	    GridData[] grids = new GridData[ndim];
	    int[] gpos = new int[ndim];
	    for (int i=0; i<ndim; i++) {
		grids[i] = new RegularGridData("x", null,
		    0.0, 1.1, Util.toInt(args[i]));
		gpos[i] = Util.toInt(args[i+ndim]);
	    }
	    RealNData data = new RealNData("u", null, grids);
	    int inx = data.inx(gpos);
	    int[] gpos1 = data.gridPos(inx);
	    System.out.print("" + inx);
	    for (int i=0; i<gpos1.length; i++)
		System.out.print(" " + gpos1[i]);
	    System.out.println("");
	}

	// Auxiliary class for permuting sort
	static class IntDoub implements Comparable {
	    public double a;
	    public int i;
	    public int compareTo(Object o) 
	    { 
		return Double.compare(((IntDoub) o).a,a);  
	    }
	}

}

