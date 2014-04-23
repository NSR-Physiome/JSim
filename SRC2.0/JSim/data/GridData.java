/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// stored data grid

package JSim.data;
import JSim.util.*;
import JSim.expr.*;

public abstract class GridData extends Data {

	// constructor
	public GridData(String d, Unit u) throws Xcept {
	    super(d, u);
	}

	// abstract methods
	abstract public int ct(); // # grid points in domain
	abstract public int nearestInx(double v) throws Xcept; // nearest grid inx

	// easy query
	public int ndim() { return 1; }
	public int nsamples() { return ct(); }
	public GridData grid(int i) throws Xcept {
	   if (i != 0) throw new Xcept(this, "illegal GridData grid() index");
	   return this;
	}
	public String legendGrids() { return ""; }

	// return entire array
	public double[] samples() throws Xcept { 
	    int ct = ct();
	    double[] arr = new double[ct];
	    for (int i=0; i<ct; i++) arr[i] = realVal(i);
	    return arr;
	}

	// calculate extrema,  nothing to do
	public void calcExtrema() { }

	// same grids
	public boolean sameAs(GridData grid) throws Xcept {
	    if (this == grid) return true;
	    if (! sameStr(name(), grid.name())) return false;
	    if (! sameStr(desc(), grid.desc())) return false;
	    if (! Unit.same(unit(), grid.unit())) return false;
	    return sameSamplesAs(grid);
	}

	// check string equivalent
	boolean sameStr(String s1, String s2) {
	    if (Util.isBlank(s1) && Util.isBlank(s2))
		return true;
	    if (s1 == null || s2 == null) return false;
	    return s1.equals(s2);
	}

	// same grid samples
	public boolean sameSamplesAs(GridData grid) throws Xcept {
	    if (this == grid) return true;
	    int ct = grid.ct();
	    if (ct != ct()) return false;
	    for (int i=0; i<ct; i++) 
		if (realVal(i) != grid.realVal(i)) 
		    return false;
	    return true;
	}

	// same within given error
	public boolean sameSamplesAs(GridData grid, double error)
	throws Xcept {
	    if (this == grid) return true;
	    if (ct() != grid.ct()) return false;
	    for (int i=0; i<ct(); i++) 
		if (Math.abs(realVal(i) - grid.realVal(i)) > error) 
		    return false;
	    return true;
	}

  	// is point out of range, within fudge factor?
	public boolean outOfRange(double x) throws Xcept {
	    double fudge = 1e-7 * (max() - min()) / (ct() - 1);
	    return (x > max() + fudge) || (x < min() - fudge);
	}

	// interpolated query
	public double realVal(double[] vals) throws Xcept {
	    if (vals.length != 1) throw new Xcept(this,
		"realVal() array length is incorrect");
	    return vals[0];
	}

	// combine two grids
	public GridData combine(GridData grid) throws Xcept {
	    if (sameSamplesAs(grid)) return this;
	    int jct = ct();
	    int kct = grid.ct();
	    double[] arr = new double[jct + kct];
	    int i = 0;
	    int j = 0;
	    int k = 0;

	    // loop taking minimum values from each grid
	    //     works only up until first NaN
	    while(j<jct || k<kct) {
		double jval = (j<jct) ? realVal(j) : Double.NaN;	
		double kval = (k<kct) ? grid.realVal(k) : Double.NaN;
		if (jval == kval) {
		    arr[i++] = jval;
		    j++;
		    k++;
		} else if (jval < kval || Double.isNaN(kval)) {
		    arr[i++] = jval;
		    j++;
		} else {
		    arr[i++] = kval;
		    k++;
		} 
	    }

	    // make smaller array,  if needed
	    if (i<jct + kct) {
		double[] arr1 = new double[i];
		for (j=0; j<i; j++) arr1[j] = arr[j];
		arr = arr1;
	    }

	    // return new Grid
	    return new IrregularGridData("combined", null, arr);
	}
}

