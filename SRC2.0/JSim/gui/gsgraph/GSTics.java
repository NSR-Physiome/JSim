/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// generate attractive tic intervals and labels && contour levels

package JSim.gui.gsgraph;

import JSim.util.*;

import java.util.*;

public class GSTics {

        // instance fields
	private boolean log;  // log scale?
	private int nchars;  // # chars avail or 0 for contours
        private double axismin;  // min value on axis
        private double axismax;  // max value on axis
	private double[] ticValues; 
	private StringList ticLabels;
	private int ticPrec;    // precision for axis rendering
	private double minDelta; // distance between closest tics

	// temp fields
	private boolean logTicFormat; // log tics always 1eN
	private ArrayList<Double> ticList; // for building subtics
	
	// constructor 
        public GSTics(
        double min,     // minimum data or user range,  or NaN if no data 
        double max,     // maximum data or user range,  or NaN if no data
        boolean l,    // true if log scale, otherwise linear
        boolean auto,    // if true, min and max come from data, 
                              // otherwise from user
	int nchars) // #chars available for tics,  or 0 if contours
        {
	    // bullet-proof NaNs, Infs
	    if (Double.isInfinite(min)) min = Double.NaN;
	    if (Double.isInfinite(max)) max = Double.NaN;
	    if (Double.isNaN(min)) min = max;
	    if (Double.isNaN(max)) max = min;
	    if (Double.isNaN(max)) min = max = 1;
	
	    // ensure axismin <= axismax
	    axismin = min;
	    axismax = max;
            if (axismax < axismin) {
                double temp = axismax;
                axismax = axismin;
                axismin = temp;
            }

	    // store nchars, log 
	    //   log scale only if positive min
	    this.nchars = nchars;
	    log = l;
	    if (axismin <= 0) log = false;

	    // make linear or log tics
	    if (log) 
		makeLogTics(auto);
	    else
		makeLinearTics(auto);
	}

	//// LINEAR SECTION

	// linear tic calculation
	private void makeLinearTics(boolean auto) {

            // ensure axismin < axismax
            if (axismin == axismax) {
		double d = 
		    (axismin==0) ? 1d : Math.abs(axismin);
                axismin -= d;
                axismax += d;
            }

	    // protect against too small a range
	    double range = axismax - axismin;
	    if (axismin/range > THRESH || axismax/range > THRESH) {
		double center = (axismin==0) ? axismax : axismin;
		axismin -= center/THRESH;
		axismax += center/THRESH;
	    }

	    // decrement so deltax has at least 2 tics
	    range = axismax - axismin;
	    int deltax = (int) (3*Math.log(range)/LOG10);
	    while (linearNtics(axismin, axismax, linearDelta(deltax)) < 2)
		deltax--;

	    // autoscale axis range expands min/max to delta
	    //   don't do this for contour plots or log (may get 0)
	    if (auto && !isContour() && !log) {
		double d = linearDelta(deltax-2);
		axismin = d * Math.floor(axismin/d);
		axismax = d * Math.ceil(axismax/d);
	    	while (linearNtics(axismin, axismax, linearDelta(deltax+1)) >= 2)
		    deltax++;
	    } 

	    // decrease delta/deltax so long as space avail and #tics in  {2-6}
	    while (true) {
		double delta = linearDelta(deltax-1);
		int prec = linearTicPrecision(axismin, axismax, delta);
		int n = linearNtics(axismin, axismax, delta);
		int maxTics = 6;
 		if (n<2 || n>maxTics) break; // <2 for pathological 

		// test space avail
		if (nchars > 0) {
		    int tspace = (int) Math.floor(nchars*(delta/(axismax-axismin)));
		    double[] vals = linearTicValues(axismin, axismax, delta);
		    int tchars = ticMaxChars(vals, prec);
		    if (tchars >= tspace) break;
		}

		// decrement deltax and continue
		deltax--;
	    }
	    double delta = linearDelta(deltax);

	    // set final prec, delta, tics w/ no subtics
	    ticPrec = linearTicPrecision(axismin, axismax, delta);
	    minDelta = delta;
	    ticValues = linearTicValues(axismin, axismax, delta);
	    ticLabels = ticLabels(ticValues);
	    if (isContour()) return;

	    // ticValues/ticLables w/ subtics
	    double[] tics = ticValues;
	    int ntics = tics.length;
	    ticList = new ArrayList<Double>();
	    ticLabels = new StringList();
	    int nsubtics = linearNSubtics(deltax);
	    addSubTics(tics[0] - delta, tics[0], nsubtics, false);
	    for (int i=1; i<ntics; i++) 
	        addSubTics(tics[i-1], tics[i], nsubtics, false);
	    addSubTics(tics[ntics-1], tics[ntics-1] + delta,
	        nsubtics, false);
	    ticValues = ticValues(ticList);	    
	}

	// linear delta for inx
	private double linearDelta(int i) {
	    int r = (i + 3000) % 3;
  	    int p = ((i + 3000) / 3) - 1000;
	    double d = Math.pow(10, p);
	    
	    // return delta
	    switch (r) {
	    case 1: return d*2;
	    case 2: return d*5;
	    default: return d;
	    }
	}

	// linear nsubtics
	private int linearNSubtics(int i) {
	    int r = (i + 3000) % 3;
	    switch (r) {
	    case 1:  return 4;
	    case 2:  return 5;
	    default: return 4;
	    }
	}	    	

	// linear tic values for given min/max/delta
	private double[] linearTicValues(double min, double max, double delta) {
	    int n = linearNtics(min, max, delta);
	    double[] v = new double[n];
	    int minx = (int) Math.ceil(min/delta - 1/THRESH);
	    for (int i=0; i<n; i++) 
		v[i] = delta*(minx+i);
	    return v;
	}

	// # linear tics for given min/max/delta
	private int linearNtics(double min, double max, double delta) {
	    int lox = (int) Math.ceil(min/delta - 1/THRESH); 
	    int hix = (int) Math.floor(max/delta + 1/THRESH);
	    return hix-lox+1;
	}

	// precision required for linear tic rendering min/max/delta
   	private int linearTicPrecision(double min, double max, 
	double delta) {
	    double mintic = Math.abs(delta * Math.ceil(min/delta - 1/THRESH));
	    double maxtic = Math.abs(delta * Math.floor(max/delta + 1/THRESH));
	    double tic = Math.max(mintic, maxtic);
	    int prec = (int) Math.ceil(Math.log(tic/delta)/LOG10);
	    prec++; // make up for PrettyFormat irregularities
	    if (prec < 3) prec = 3; // sanity check
	    return prec;
	}

	//// LOG SECTION

	// log tic calculation
	private void makeLogTics(boolean auto) {

            // ensure axismin < axismax
            if (axismin == axismax) {
	        axismin *= 0.1;
	    	axismax *= 10;
	    }

	    // use linear tics if range less that 2 powers of 10
	    double minLogRange = isContour() ? 1000 : 100;
	    if (axismax/axismin < minLogRange) {
	    	makeLinearTics(auto);
		return;
	    }
	    logTicFormat = true;

	    // autoscale axis range expands min/max to nearby decade
	    //   don't do this for contour plots
	    if (auto && !isContour()) {
		double nmin = Math.pow(10,
		    Math.floor(Math.log10(axismin)));
		if (axismin/nmin < 2)
		    axismin = nmin;
		double nmax = Math.pow(10,
		    Math.ceil(Math.log10(axismax)));
		if (nmax/axismax < 1.2)
		    axismax = nmax;
	    } 

	    // loop creating ticValues until good # of tics
	    //   p is decade reduction factor
	    int minx = (int) Math.ceil(Math.log10(axismin));
	    int maxx = (int) Math.floor(Math.log10(axismax));
	    int ndecs =  maxx-minx+1; // # 10^x tics possible in axis range
	    int p = Math.max(ndecs / 6, 1);  
	    ticPrec = 4; 
	    while (true) {
	    	if (p > 3 && !((p % 5) == 0)) {
		    p++;
		    continue;
		}
	    	ticValues = logTicValues(ndecs, p);
		int ntics = ticValues.length;		
		if (ntics <= 2) break;
		if (ntics <= 6) {
		    if (nchars == 0) break;
		    int tspace = (int) Math.floor(nchars/ticValues.length);
		    int tchars = ticMaxChars(ticValues, ticPrec);
		    if (tchars <= tspace) break;
		}
		p++;    
	    }	

	    // set minDelta, tics w/ no subtics
	    minDelta = Math.pow(10, minx-1);
	    ticValues = logTicValues(ndecs, p);
	    ticLabels = ticLabels(ticValues);
	    if (isContour()) return;
	    
	    // ticValues/ticLables w/ subtics
	    double[] tics = ticValues;
	    int ntics = tics.length;
	    ticList = new ArrayList<Double>();
	    ticLabels = new StringList();
	    boolean sublog = p>1;
	    int nsubtics = 9;
	    if (sublog) {
		switch (p) {
		case 2: nsubtics = 2; break;
		case 3: nsubtics = 3; break;
		default: nsubtics = 5; break;
	 	}
	    }
	    double f = Math.pow(10, p);
	    addSubTics(tics[0]/f, tics[0], nsubtics, sublog);
	    for (int i=1; i<ntics; i++) 
	        addSubTics(tics[i-1], tics[i], nsubtics, sublog);
	    addSubTics(tics[ntics-1], tics[ntics-1]*f,
	        nsubtics, sublog);
	    ticValues = ticValues(ticList);	    
	}

	// create log tic values, every p'th power 10, retaining highest power
	private double[] logTicValues(int ndecs, int p) {
	    int ntics = (int) (Math.ceil((ndecs+0.0)/p));
//System.err.println("  log  #dec=" + ndecs + " #tic=" + ntics);
	    double[] tvs = new double[ntics];
	    int minx = (int) Math.ceil(Math.log10(axismin));
	    double tic = Math.pow(10, minx);
	    int ticx = 0;
	    for (int i=0; i<ndecs; i++) {
		boolean keep = ((i+minx) % p) == 0;
//		System.err.println("   i=" + i  + " tic=" + tic + " keep=" + keep);
	    	if (keep) tvs[ticx++] = tic;
		tic *= 10;
	    }
	    return tvs;
	}

	// create log tic values, every p'th power 10, retaining highest power
	private double[] logTicValuesOLD(int ndecs, int p) {
	    int ntics = (int) (Math.ceil((ndecs+0.0)/p));
//System.err.println("  log  #dec=" + ndecs + " #tic=" + ntics);
	    double[] tvs = new double[ntics];
	    int minx = (int) Math.ceil(Math.log10(axismin));
	    double tic = Math.pow(10, minx);
	    int ticx = 0;
	    for (int i=0; i<ndecs; i++) {
		boolean keep = (((ndecs-1) - i) % p) == 0;
//		System.err.println("   i=" + i  + " tic=" + tic + " keep=" + keep);
	    	if (keep) tvs[ticx++] = tic;
		tic *= 10;
	    }
	    return tvs;
	}

	//// COMMON LINEAR/LOG SECTION

	// constants
	private static final double THRESH = 1e7;
	private static final double LOG10 = Math.log(10);

	// static numeric formatting methods (preserves formatters across calls)
	private static PrettyFormat prettyFormat[] = new PrettyFormat[32];
	private static PrettyFormat prettyFormat(int i) {
	   if (i>=prettyFormat.length) return new PrettyFormat(i);
	   if (prettyFormat[i] == null)
		prettyFormat[i] = new PrettyFormat(i);
	   return prettyFormat[i];
	}
	private static String format(double v, int i) {
	    return prettyFormat(i).format(v);
	}
	private static String logFormat(double v) {
	    int p = (int) Math.round(Math.log10(v));
	    return "1E" + p;
	}
	
	// simple query
	public boolean isContour() { return nchars == 0; }
	public boolean log() { return log; }
	public double axismin() { return axismin; }
	public double axismax() { return axismax; }
	public double axisrange() { return axismax-axismin; }
	public double[] ticValues() { return ticValues; }
	public String[] ticLabels() { return ticLabels.array(); }
	public double minDelta() { return minDelta; }
	public String prettyQuery(double v) { return format(v, ticPrec + 2); }
	public String toString() {
	    String s = "{";
	    if (log) s = s + "log ";
	    s = s + prettyQuery(axismin) +
 		" - " + prettyQuery(axismax);
 	    return s + "} " + ticLabels;
	}

	// max # chars for tic label rendering
	private int ticMaxChars(double[] v, int prec) {
	    int ct = 0; 
	    for (int i=0; i<v.length; i++) {
		String s = format(v[i], prec);
		int j = s.length();
		if (j>ct) ct = j;
	    }
	    return ct+4; // always leave 2 spaces on either side 
	}

	// tic labels for all values in array
	private StringList ticLabels(double[] v) {
	    StringList labs = new StringList(v.length);
	    for (int i=0; i<v.length; i++)
	    	labs.add(format(v[i], ticPrec));
	    return labs;
	}

	// add numtic & tics 
	//    and n subtics from tic1-tic2, sublog if exp spaced
	private void addSubTics(double tic1, double tic2, 
	int n, boolean sublog) {

	    // numeric tic at tic1
	    double locut, hicut;
	    if (log) {
		locut = axismin * .99;
		hicut = axismax * 1.01;
	    } else {
	    	locut = axismin - minDelta/100;
	    	hicut = axismax + minDelta/100;
	    }
	    if (tic1 >= locut && tic1 <= hicut) {
		ticList.add(tic1);
		String s = logTicFormat ?
		    logFormat(tic1) : format(tic1, ticPrec);
		ticLabels.add(s);
	    }

	    // subtics
	    double sdelta = (tic2-tic1) / n;
	    double tic = tic1;
	    double d = 10;
	    if (sublog) 
		d = Math.pow(10, Math.log10(tic2/tic1)/n);
	    for (int i=1; i<n; i++) {
	        if (sublog)
		    tic *= d;
		else
		    tic += sdelta;
		if (tic >= locut && tic <= hicut) {
		    ticList.add(tic);
		    ticLabels.add("");
	        }
	    }
	}	    

	// make double array of list
	private double[] ticValues(ArrayList<Double> list) {
	    double[] arr = new double[list.size()];
	    for (int i=0; i<arr.length; i++)
	    	arr[i] = list.get(i);
	    return arr;
	}

	//// TEST HARNESS
	    
	// test program
	public static void main(String[] args) throws Exception {
            if (args.length != 5) 
		throw new Exception(
                    "Usage: GSTics min max log autoscale nchars");

            int i = 0;
            double min = Util.toDouble(args[i++]);
            double max = Util.toDouble(args[i++]);

            boolean log = false;
            switch (args[i++].charAt(0)) {
            case 't':
            case 'T':        
                log = true;
                break;
            case 'f':
            case 'F':        
                log = false;
                break;
            default:
                throw new Exception(
                        "Parameter \"log\" must be true or false");
            }

            boolean autoscale = true;
            switch (args[i++].charAt(0)) {
            case 't':
            case 'T':        
                autoscale = true;
                break;
            case 'f':
            case 'F':        
                autoscale = false;
                break;
            default:
                throw new Exception(
                        "Parameter \"autoscale\" must be true or false");
            }

            int nchars = Util.toInt(args[i++]);

	    GSTics tics = new GSTics(min, max, log, autoscale, nchars);
	    System.err.println(">> " + tics);
	    double[] tvs = tics.ticValues();
	    //for (i=0; i<tvs.length; i++) 
	    //   System.out.println("" + tvs[i]);
	}
}

	    
