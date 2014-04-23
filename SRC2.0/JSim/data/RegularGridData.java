/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  stored data for evenly spaced grid

package JSim.data;
import JSim.util.*;
import JSim.expr.*;

public class RegularGridData extends GridData {
	private double min;
	private double max;
	private int ct;

	// constructor
	public RegularGridData(String d, Unit u,
	double mmin, double mmax, int cct) throws Xcept {
	    super(d, u);
	    min = mmin;
	    max = mmax;
	    ct = cct;
	    if (min>=max) throw new Xcept(this,
		"Min/max error");
	    if (ct<2) throw new Xcept(this,
		"#data error");
	}

	// domain methods
	public DataInfo info(Subset sset) { return new Info(this); }
	public double min() { return min; }
	public double max() { return max; }
	public int ct() { return ct; }

	// minimum positive value,  NaN if none
	public double minpos()  {
	    if (min>0) return min;
	    if (max<=0) return Double.NaN;
	    int i = (int) Math.ceil(-min*(ct-1)/(max-min));
	    try {
	    	double minpos = realVal(i);
	    	return (minpos == 0) ? realVal(i+1) : minpos;
	    } catch (Xcept e) {
		return Double.NaN;
	    }
	}

	// value for ith grid point
	public double realVal(int i) throws Xcept {
	    if (i<0 || i>=ct) throw new Xcept(this,
		"Index out of bounds");
	    return min + i*(max-min)/(ct-1);
	}

	// delta value
	public double delta() throws Xcept {
	    return (max-min)/(ct-1);
	}

	// get nearest grid inx
	public int nearestInx(double v) throws Xcept {
	    int inx = (int) ((ct-1)*(v-min)/(max-min) + 0.5);
	    if (inx < 0) return 0;
	    if (inx >= ct) return ct-1;
	    return inx;
	}

	// sameSamplesAs
	public boolean sameSamplesAs(GridData grid) throws Xcept {
	    if (grid == this) return true;
	    if (! (grid instanceof RegularGridData))
		return super.sameSamplesAs(grid);
	    RegularGridData g = (RegularGridData) grid;
	    if (g.min() != min()) return false;
	    if (g.max() != max()) return false;
	    if (g.ct() != ct()) return false;
	    return true;
	}

	// all NaNs?
	public boolean allNaN() { return false; }

	// serializable info
	public static class Info extends DataInfo {
	    public double min;
	    public double max;
	    public int ct;

	    // constructors
	    public Info() { }
	    public Info(RegularGridData data) {
		super(data);
		min = data.min;
		max = data.max;
		ct = data.ct;
	    }
	}
	    
}

