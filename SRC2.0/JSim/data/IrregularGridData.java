/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  stored data for unevenly spaced grid

package JSim.data;
import JSim.util.*;
import JSim.expr.*;

public class IrregularGridData extends GridData {
	private double[] samples;
	private double minpos;	// minimum positive value
	private int ct;

	// empty constructor
	public IrregularGridData(String d, Unit u) 
	throws Xcept {
	    super(d, u);
	    samples = new double[0];
	    ct = 0;
	    minpos = Double.NaN;
	}

	// full-data constructor
	public IrregularGridData(String d, Unit u, double[] s) 
	throws Xcept {
	    this(d, u);
	    samples = (double[]) s.clone();
	    ct = samples.length;
	    if (ct<2) throw new Xcept(
		"#data error in IrregularDomainData constructor");
	    for (int i=0; i<ct; i++) {
		if (Double.isNaN(minpos) && samples[i]>0) 
		    minpos = samples[i];
		if (i==0) continue;
		if (samples[i]<=samples[i-1]) throw new Xcept(
		    "Samples ordering error in IrregularDomainData constructor");
	    }
	}

	// domain methods
	public DataInfo info(Subset sset) { return new Info(this); }
	public double min() { return samples[0]; }
	public double max() { return samples[ct-1]; }
	public int ct() { return ct; }
	public double[] samples() { return (double[]) samples.clone(); };
	public double minpos() { return minpos; }

	// value for ith domain point
	public double realVal(int i) throws Xcept {
	    if (i<0 || i>=ct) throw new Xcept(
		"Index out of bounds error in IrregularDomainData");
	    return samples[i];
	}

	// get nearest grid inx, 
	//   not very inefficent, 
	//   should have cache and do binary search
	public int nearestInx(double v) throws Xcept {
	    if (ct==0) throw new Xcept(
		"nearestInx() called for empty grid");
	    if (v<=samples[0]) return 0;
	    int i=1;
	    while (i<ct && v>samples[i]) i++;
	    if (i>=ct) return ct-1;
	    double dlow = v-samples[i-1];
	    double dhi = samples[i]-v;
	    return (dlow<dhi) ? (i-1) : i;	     
	}	

	// add point to grid,  return inx
	public int addPoint(double v) throws Xcept {
	    if (ct > 0 && v <= max()) throw new Xcept(
		"grid midpoint addition not yet supported");
	    if (Double.isNaN(minpos) && v>0) minpos = v;
	    double[] samp = new double[ct+1];
	    for (int i=0; i<ct; i++) samp[i] = samples[i];
	    samp[ct] = v;
	    ct++;
	    samples = samp;
	    return ct-1;
	}

	// serializable info
	public static class Info extends DataInfo {
	    public double[] samples;

	    // constructors
	    public Info() { }
	    public Info(IrregularGridData data) {
		super(data);
		samples = data.samples;
	    }
	}


}

