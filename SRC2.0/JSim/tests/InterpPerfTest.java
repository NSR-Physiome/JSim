/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// performance test for RealNData.realVal(double[] xvals)

package JSim.tests; import JSim.data.*;
import JSim.util.*;

public class InterpPerfTest {
	public GridData[] grids;
	public RealNData rdata;

	// constructor
	public InterpPerfTest(int alg, int n, int gct) throws Xcept {
	    grids = new GridData[n];
	    for (int i=0; i<n; i++) 
	    	grids[i] = new RegularGridData("x" + i, null,
		    0, i+1, gct);
	    rdata = new RealNData("u", null, grids);
	    rdata.setInterp(alg);
	    int ct = (int) Math.pow(gct, n);
	    for (int i=0; i<ct; i++) {
	    	int[] gpos = rdata.gridPos(i);
		double[] xvals = new double[n];
		for (int j=0; j<n; j++) 
		    xvals[j] = grids[j].realVal(gpos[j]);
		double rval = f(xvals);
//		System.out.println(fmsg(xvals, rval));
		rdata.set(i, rval);
	    }
	}
	    
	// f(grids)
	public double f(double[] xvals) {
	    double tot = 0;
	    double mult = 1;
	    for (int i=0; i<xvals.length; i++) {
		tot = tot + xvals[i] * mult;
		mult *= 10;
	    }
	    return tot;
	}
	
	// show f value
	public String fmsg(double[] xvals, double f) {
	    String s = "f(";
	    for (int i=0; i<xvals.length; i++) {
		if (i>0) s = s + ",";
		s = s + Util.pretty(xvals[i]);
	    }
	    s = s + ")=" + Util.pretty(f);
	    return s;
	}

	// N random queries
	public long query(int nquery, double thresh) throws Xcept {
	    int ndim = rdata.ndim();
	    double[] xvals = new double[ndim];
	    long tstart = System.currentTimeMillis();

	    for (int i=0; i<nquery; i++) {
	    	for (int j=0; j<ndim; j++) 
		    xvals[j] = (j+1)*Math.random();
		double rval = rdata.realVal(xvals);
		if (thresh > 0) {
		    double fval = f(xvals);
		    if (Math.abs(fval-rval) > thresh) {
		    	String msg = fmsg(xvals, fval) +
		    	    "  rdataInterp=" + rval;
		    	System.out.println(msg); 
		    }
		}
	    }
	    
	    return System.currentTimeMillis() - tstart;
	}

	// mainline
	public static void main(String[] args) throws Xcept {
	    if (args.length != 5) throw new Xcept(
	    	"Usage: InterpPerfTest alg ndim gridct nquery errThresh");
	    int alg = Util.toInt(args[0]);
	    int ndim = Util.toInt(args[1]);
	    int gridCt = Util.toInt(args[2]);
	    int nquery = Util.toInt(args[3]);
	    double thresh = Util.toDouble(args[4]);
	    InterpPerfTest itest = new InterpPerfTest(alg, ndim, gridCt);
	    long msec = itest.query(nquery, thresh);
//	    System.err.println("  " + nquery + " queries in " +
//	    	msec + " msec");
	    System.err.println("  ndim=" + ndim + " ct=" + gridCt +
	        " query rate=" + (nquery*1000/msec) + "/sec");
	}
}
