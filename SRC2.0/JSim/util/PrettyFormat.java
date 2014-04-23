/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// pretty number format

package JSim.util;

import java.text.*;
import java.util.Locale;

public class PrettyFormat {
	// instance fields
	private int precision;
	private DecimalFormat floatFmt, fixedFmt;
	private double zeroThresh; // small numbers format->0

	// static format array
	private static final int NFMTS = Util.DOUBLE_PRECISION+1; 
	private static final PrettyFormat[] fmts = 
	    new PrettyFormat[NFMTS];

	// constructor for given decimal precision p
	public PrettyFormat(int p) {
	    if (p<1) p=7; // bulletproof without Exception
	    precision = p;

	    // create floating format
	    String hash = "";
	    for (int i=1; i<p; i++)
		hash = hash + "#";
	    floatFmt = new DecimalFormat("0." + hash + "E0");
	    floatFmt.setMaximumFractionDigits(p-1);
	    floatFmt.setMinimumIntegerDigits(1);
	    setSymbols(floatFmt);

	    // create fixed decimal format
	    fixedFmt = new DecimalFormat(hash + "." + hash);
	    fixedFmt.setMaximumFractionDigits(p);
	    setSymbols(fixedFmt);
	}

	// set symbols for format
	private void setSymbols(DecimalFormat fmt) {
	    DecimalFormatSymbols s = fmt.getDecimalFormatSymbols();
	    s.setNaN("NaN");
	    s.setInfinity("Infinity");
	    s.setDecimalSeparator('.'); // needed for some Locales
	    fmt.setDecimalFormatSymbols(s);
	}

	// set properties
	public void setZeroThresh(double z) { zeroThresh = z; }

	// format a number
	public String format(double v) {
	    if (Math.abs(v) < zeroThresh) return "0";
	    String s1 = floatFmt.format(v);

	    // remove/replace annoying E0, E1, E2
	    int s1len = s1.length();
	    if (s1len > 2 && s1.charAt(s1len-2) == 'E') {
	    	switch (s1.charAt(s1len-1)) {
		case '0': 
		    s1 = s1.substring(0, s1len-2); 
		    break;
	    	case '1': 
		    if (s1.indexOf('.') < 0)
		    	s1 = s1.substring(0, s1len-2) + "0"; 
		    break;
	    	case '2': 
		    if (s1.indexOf('.') < 0)
		    	s1 = s1.substring(0, s1len-2) + "00"; 
		    break;
		}
	    }

	    // if default Double format doesn't need exponent, 
	    //    replace s1 with fixed format, if it's shorter	    
	    String s0 = Double.toString(v);
	    if (s0.indexOf('E')<0) {
	    	String s2 = fixedFmt.format(v);
		if (s2.charAt(0) == '0' && s2.length() > 1)
		    s2 = s2.substring(1);
		if (s2.length() <= s1.length()) 
		    s1 = s2;
	    }

	    // return s1
	    return s1;
	}

	// query
	public int precision() { return precision; }

	// static query
	public static String sformat(float f) { 
	    return sformat(f, Util.SINGLE_PRECISION);
	}
	public static String sformat(double d) { 
	    return sformat(d, Util.SINGLE_PRECISION);
	}
	public static String sformat(double d, int p) {
	    if (p >= NFMTS) p = NFMTS;
	    if (fmts[p] == null)
	    	fmts[p] = new PrettyFormat(p);
	    return fmts[p].format(d);
	}
	
	// test program
	public static void main(String[] args) throws Exception {
	    double v = Util.toDouble(args[0]);
	    for (int i=1; i<args.length; i++) {
	    	int p = Util.toInt(args[i]);
		String vs = sformat(v, p);
		System.out.println("p=" + p + " v=" + vs);
	    }
	}

	public static void oldMain(String[] args) throws Xcept {
	    Util.setDefaultLocale(args[0]);
	    int p = Util.toInt(args[1]);
	    double v = Util.toDouble(args[2]);

	    PrettyFormat pretty = new PrettyFormat(p);

	    System.err.println("double=" + Double.toString(v));
	    System.err.println("float=" + pretty.floatFmt.format(v));
	    System.err.println("fixed=" + pretty.fixedFmt.format(v));
	    System.err.println("pretty=" + pretty.format(v));
	}
}


