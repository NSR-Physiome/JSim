/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Construct InterpolatedSeries object 

package JSim.fgen; 

import java.io.*;
import java.lang.Math;
import java.text.DecimalFormat;
//
// CONSTRUCT THE InterpolatedSeries OBJECT
//
public class FgenInterp {
        public  double[] t;
        public  double[] ft;
        public  int length;
        public   FgenInterp ( double tstart, double tend, double dt) {
		if( tend>tstart && dt >0 ) {
			this.length = (int) ((tend-tstart)/dt+1.0);
			this.t = new double[length];
			this.ft = new double[length];
			for (int i=0; i<length; i++ ) {
				t[i]=tstart+ (double)(i)*dt;
				ft[i]=0.0;
			}
		}
		else {
			this.length = 1;
			this.t = new double[length];
			this.ft = new double[length];
			t[0]=0;
			ft[0]=0;
		}
	}
        public   FgenInterp () {
	    this.length = 1;
	    this.t = new double[length];
	    this.ft = new double[length];
	    t[0]=0;
	    ft[0]=0;
	}

	public void print(PrintStream ps) {
		final String DELIM ="\t";
		final DecimalFormat SCIFOR5 = new DecimalFormat("0.00000E0");
                for( int i=0; i<length; i++) {
                        ps.println(" "+(float)t[i]+DELIM+SCIFOR5.format(ft[i]));
		}
	}
	public void printAbs(PrintStream ps) {
		final String DELIM ="\t";
		final DecimalFormat SCIFOR5 = new DecimalFormat("0.00000E0");
                for( int i=0; i<length; i++) {
                        ps.println(" "+(float)t[i]+DELIM+SCIFOR5.format(Math.abs(ft[i])));
		}
	}
	public void printNonZero(PrintStream ps) {
		final String DELIM ="\t";
		final DecimalFormat SCIFOR5 = new DecimalFormat("0.00000E0");
                for( int i=0; i<length; i++) {
	            if (ft[i]!=0.) {
                        ps.println(" "+(float)t[i]+DELIM+SCIFOR5.format(ft[i]));
	            }
		}
	}
	public void normalize(double area) {
		if(length>0 ) {
			double sum = 0;
			for (int i =0; i<length; i++) {
				sum+=ft[i];
			}
			if(area>0 ) {sum=sum*(t[1]-t[0])/area;}
			else        {sum=sum*(t[1]-t[0]);}
			if(sum>0) {
				for (int i =0; i<length; i++ ) {
					ft[i]=ft[i]/sum;
				}
			}	
		}
		
	}		
	public int peak() {
		double peak = ft[0];
		int locationPeak=0;
		for (int i=0; i<length; i++) {
			peak = Math.max(peak,ft[i]);
			if(peak==ft[i]) {locationPeak=i;}
		}
		return locationPeak;
	}
	public double maxVal() {
	    int peaklocation=peak();
	    return ft[peaklocation];
        }
	public void zeroLowVals( double cutoff ) {
// cutoff should be (fraction of peak)*maxVal
		for (int i=0; i<length; i++) {
		    if( ft[i]<cutoff ) ft[i]=0;
		}
	}

	public int lastNonZero() {
		int locationTail=length-1;
		for (int i=length-1; i>0; i--) {
			if(ft[i]==0) {locationTail=i;}
			else {return locationTail-1;}
		}
		return locationTail;
	} 
	public int fractionPeak(double frpeak) {
	// Find place on curve tail where ft = frpeak*MaxPeak.
		double newfrpeak=Math.min(Math.max(frpeak,1e-10),0.9999);
		int peaklocation=peak();
		double cutoff=ft[peaklocation]*newfrpeak;
		for( int i=peaklocation; i<length-1; i++) {
			if(ft[i]>=cutoff && cutoff>=ft[i+1]) {return i;}
		}
		return length;
	}			
	public void truncTail (double frpeak) {
	// truncate tail beyond frpeak
	    double newfrpeak=Math.min(Math.max(frpeak,1e-10),0.9999);
	    int peaklocation=peak();
	    double peakmax = maxVal();
	    double cutoff=peakmax*newfrpeak;
	    for( int i=peaklocation; i<length; i++) {
                        if(ft[i]<cutoff) ft[i]=0.0;
                }
        }

	    


	 public double valueAt(double tnow) {
// binary search of independent variable until interval located, linear interpolation

	    int n = length;
	    if (tnow < t[0]) return 0;
            else if (tnow > t[n-1]) return 0;
            int jlo = 0;
            int jhi = n-1;
            while(jhi - jlo > 1) {
                int j = (jhi+jlo) >> 1;
                if (t[j] > tnow) jhi = j;
                else jlo = j;
            }
            double dt = t[jhi] - t[jlo];
            if (dt == 0) return 0;
            double ftAtT = ft[jlo] + (ft[jhi]-ft[jlo])/dt*(tnow-t[jlo]);
            return ftAtT;
	}

}
