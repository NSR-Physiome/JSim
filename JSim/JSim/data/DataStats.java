/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// Basic statistic calculations on RealNData

package JSim.data;

import JSim.util.*;

import java.io.*;
import java.util.*;

public class DataStats {

	// estimate mean
	public static double mean(RealNData data) {
	    double n = 0;
	    double tot = 0;
	    for (int i=0; i<data.nsamples(); i++) {
	    	double d = data.realVal(i);
		if (Double.isNaN(d)) continue;
		tot += d;
		n += 1;
	    }
	    return tot / n;
	}

	// estimate standard deviation (unbiased, N-1 version)
	public static  double sd(RealNData data) {
	    return sd(data, mean(data));
	}
	public static double sd(RealNData data, double mean) {
	    double n = 0;
	    double tot = 0;
	    for (int i=0; i<data.nsamples(); i++) {
	    	double d = data.realVal(i);
		if (Double.isNaN(d)) continue;
		tot += (d-mean) * (d-mean);
		n += 1;
	    }
	    return Math.sqrt(tot/ (n-1));
	}
	
	// estimate covariance 2 data curves
	//   must have same # of samples
	public static double covar(RealNData x,  RealNData y) {
	    return covar(x, mean(x), sd(x), y, mean(y), sd(y));
	}
	public static double covar(RealNData x, double xmean, double xsd,
	RealNData y, double ymean, double ysd) {
	    if (x.nsamples() != y.nsamples()) return Double.NaN;
	    int ct = x.nsamples();
	    int n = 0;
	    double tot = 0;
	    for (int i=0; i<ct; i++) {
	    	double xi = x.realVal(i);
		double yi = y.realVal(i);
		if (Double.isNaN(xi)) continue;
		if (Double.isNaN(yi)) continue;
	    	tot += (xi-xmean)*(yi-ymean);
		n++;
	    }
	    return tot/((n-1)*xsd*ysd);
	}
}

