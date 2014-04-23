/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// native math methods

package JSim.nml.math;

import JSim.util.*;
import JSim.jruntime.RTContext;

public class NatMath {
	static { System.loadLibrary("natmath"); }

	public native static double sin(double a);
	public native static double cos(double a);
	public native static double tan(double a);
	public native static double exp(double a);
	public native static double log(double a);
	public native static double log10(double a);
	public native static double asin(double a);
	public native static double acos(double a);
	public native static double atan2(double y, double x);
	public native static double sinh(double a);
	public native static double cosh(double a);
	public native static double pow(double a, double n);
	public native static double sqrt(double a);

	// fast integer powers
	public static double pow(double a, int n) {
	    switch(n) {
	    case 1: return a;
	    case 2: return a*a;
	    case 3: return a*a*a;
	    case 4: return a*a*a*a;
	    case -1: return 1/a;
	    case -2: return 1/(a*a);
	    case -3: return 1/(a*a*a);
	    case -4: return 1/(a*a*a*a);
	    default: return pow(a, (double) n);
	    }
	}	

	// profiling test
	private static long tstart, tnative, tjava, n;
	private static double tot, totNative, totJava;
	public static void main(String[] args) throws Xcept {
	    if (args.length != 1) throw new Xcept(
	    	"Usage: NatMath ncalls");
	    n = Util.toInt(args[0]);
	    System.out.println("NatMath profile ");
	    System.out.println("ncalls=" + n);
	    System.out.println("\tnative\tjava\t%perf\taccuracy");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += sin(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.sin(i/(n+0.5));
	    print("sin");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += cos(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.cos(i/(n+0.5));
	    print("cos");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += tan(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.tan(i/(n+0.5));
	    print("tan");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += exp(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.exp(i/(n+0.5));
	    print("exp");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += log(1+i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.log(1+i/(n+0.5));
	    print("log");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += log10(1+i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += UtilMath.log10(1+i/(n+0.5));
	    print("log10");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += asin(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.asin(i/(n+0.5));
	    print("asin");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += acos(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.acos(i/(n+0.5));
	    print("acos");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += sinh(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += UtilMath.sinh(i/(n+0.5));
	    print("sinh");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += cosh(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += UtilMath.cosh(i/(n+0.5));
	    print("cosh");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += sqrt(i/(n+0.5));
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.sqrt(i/(n+0.5));
	    print("sqrt");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += atan2(i/(n+0.5), 1);
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.atan2(i/(n+0.5), 1);
	    print("atan2");
	    
	    startNative();
	    for (int i=0; i<n; i++)
	    	tot += pow(i/(n+0.5),1.3);
	    startJava();
	    for (int i=0; i<n; i++)
	    	tot += Math.pow(i/(n+0.5),1.3);
	    print("pow");
	}
	
	// start Native profile
	private static void startNative() {
	    tstart = System.currentTimeMillis();
	    tot = 0;
	}
	
	// start Java profile
	private static void startJava() {
	    tnative = System.currentTimeMillis() - tstart;
	    totNative = tot;
	    tstart = System.currentTimeMillis();
	    tot = 0;
	}
	
	// print profile
	private static void print(String name) {
	    tjava = System.currentTimeMillis() - tstart;
	    int pctDiff = (int) ((100.0 * (tjava-tnative)) / tjava);
	    double totJava = tot;
	    double acc = (totNative-totJava)/totJava;
	    System.out.println(name + "\t" + tnative + 
	    	"\t" + tjava + "\t" + pctDiff + 
		"\t" + Util.pretty(acc));
	} 
	
}

