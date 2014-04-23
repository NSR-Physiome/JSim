/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test numeric format
//   generate random numeric strings, reformat, check error
//   show if error exceeds unacceptable threshhold

package JSim.tests;

import java.text.*;
import JSim.util.*;

public class FormatTest {
	
	// constructor
	public FormatTest(int ct) throws Xcept {
	    for (int i=0; i<ct; i++) {
	        int prec = 3 + (int) (Math.random() * 5);
	 	String s1 = randomNumberString(prec + 3);
	    	double d1 = Util.toDouble(s1);
		String s2 = PrettyFormat.sformat(d1);
		double d2 = Util.toDouble(s2);
		double frac = (d2-d1)/d1;
		double thresh = 5 * Math.pow(10.0, -6);
		boolean bad = (frac > thresh) || 
		    Double.isNaN(frac) || Double.isInfinite(frac);
		if (d1 == d2) bad = false;
		if (!bad) continue;
		System.err.println("\t" + prec + "\t" + s1 +
		    "\t" + s2 + "\t" + frac);
	    }
	}
	
	// random double string
	public String randomNumberString(int maxDigits) {
	    int ldigits = 1 + (int) (Math.random() * maxDigits);
	    int rdigits = 1 + (int) (Math.random() * maxDigits);
	    String s = randomIntString(ldigits);
	    if (Math.random() < .5) 
	    	s = "-" + s;
	    if (Math.random() < .5) {
	    	s = s + "." + randomIntString(rdigits);
		if (Math.random() < .5)
		    s = s + "000001";
		else if (Math.random() < .5)
		    s = s + "999999";
	    }
	    if (Math.random() < .5) {
	    	s = s + "E";
		if (Math.random() < .5)
		    s = s + "-";
		s = s + randomIntString(2);
	    }
	    return s;
	}
	
	// random int string
	public String randomIntString(int maxDigits) {
	    int digits = 1 + (int) (Math.random() * maxDigits);
	    String s = "";
	    for (int i=0; i<digits; i++) {
	    	char c = (char) ('0' + (int) (Math.random() * 10));
		s = s + c;
	    }
	    return s;
	}
	
	// mainline
	public static void main(String[] args) throws Xcept {
	    int ct = Util.toInt(args[0]);
	    new FormatTest(ct);

/*	    DecimalFormat fmt = new DecimalFormat("0.########E0");
	    for (int i=0; i<ct; i++) {
	    	double d = 1 + 2*Math.random();
		String s = fmt.format(d);
		if (! s.endsWith("E0")) continue;
		System.err.println("d=" + d + " s=" + s);
	    }			
*/
	}

}

	
