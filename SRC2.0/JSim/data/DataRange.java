/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// data min, minpos & max

package JSim.data;

import JSim.util.*;
import java.io.Serializable;

public class DataRange implements Serializable {
	private boolean isLog;
	private boolean isAutoscale;
	private double min, max;
	
	// constructors
	public DataRange(boolean isLog) {
	    this.isLog = isLog;
	    isAutoscale = true;
	    min = max = Double.NaN;
	}
	public DataRange(boolean isLog, double min, double max) {
	    this(isLog);
	    expand(min, max);
	}
	public DataRange(boolean isLog, Data data) {
	    this(isLog);
	    expand(data);
	}
	public DataRange(boolean isLog, Data[] darr) {
	    this(isLog);
	    expand(darr);
	}
	public DataRange(boolean isLog, Data.List dlist) {
	    this(isLog);
	    expand(dlist);
	}

	// set autoscale off (finalizes range)
	public void setAutoscaleOff() { isAutoscale = false; }

	// expand with single sample
	public void expand(double v) {
	    if (! isAutoscale) return;
	    if (Double.isNaN(v)) return; 
	    if (isLog && v <= 0) return;
	    if (Double.isNaN(min) || v < min) min = v;
	    if (Double.isNaN(max) || v > max) max = v;
	}

	// expand via min/max
	public void expand(double min, double max) {
	    expand(min);
	    expand(max);
	}
	public void expand(double min, double minpos, double max) {
	    expand(min);
	    expand(minpos);
	    expand(max);
	}
	
	// expand via Range
	public void expand(DataRange r) {
	    if (r == null) return;
	    expand(r.min, r.max);
	}

	// expand via Data (assumes calcExtrema completed)
	public void expand(Data data) {
	    if (data == null) return;
	    expand(data.min(), data.minpos(), data.max());
	}
	public void expand(Data[] datas) {
	    if (datas == null) return;
	    for (int i=0; i<datas.length; i++) 
	    	expand(datas[i]);
	}
	public void expand(Data.List dlist) {
	    if (dlist == null) return;
	    for (int i=0; i<dlist.size(); i++) 
	    	expand(dlist.get(i));
	}
	
	// simple query
	public String toString() {
	    return "[" + 
	         (isLog ? "log " : "") +
		 Util.pretty(min) + " " + Util.pretty(max) + "]";
	}
	public double min() { return min; }
	public double max() { return max; }
	public boolean isLog() { return isLog; }
	public boolean isAutoscale() { return isAutoscale; }

	// create array
	public static DataRange[] makeArray(boolean[] isLog) {
	    int n = isLog.length;
	    DataRange[] rs = new DataRange[n];
	    for (int i=0; i<n; i++) 
	        rs[i] = new DataRange(isLog[i]);
	    return rs;
	}

	// test harness
	public static final void main(String[] args) throws Exception {
	    int i=0;
	    boolean log1 = Util.toBoolean(args[i++]);
	    double min1 = Util.toDouble(args[i++]);
	    double max1 = Util.toDouble(args[i++]);
	    boolean log2 = Util.toBoolean(args[i++]);
	    double min2 = Util.toDouble(args[i++]);
	    double max2 = Util.toDouble(args[i++]);
	    DataRange r1 = new DataRange(log1, min1, max1);
	    DataRange r2 = new DataRange(log2, min2, max2);
	    System.out.println("Expand " + r1 + " via " + r2);
	    r1.expand(r2);
	    System.out.println("  result " + r1);
	}    
	   
}
