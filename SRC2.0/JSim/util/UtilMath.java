/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.util;

// runtime java math functions
public class UtilMath {
	private static final double LOG10 = Math.log(10);

	public static double log10(double a) {
	    return Math.log(a) / LOG10;
	}

	public static double sinh(double a) {
	    return (Math.exp(a) - Math.exp(-a)) * 0.5;
	}

	public static double cosh(double a) {
	    return (Math.exp(a) + Math.exp(-a)) * 0.5;
	}

	public static double tanh(double a) {
	    double eplus = Math.exp(a);
	    double eminus = Math.exp(-a);
	    return (eplus - eminus) / (eplus + eminus);
	}

	public static double asinh(double a) {
	    return Math.log(a + Math.sqrt(a*a + 1));
	}

	public static double acosh(double a) {
	    return Math.log(a + Math.sqrt(a*a - 1));
	}

	public static double atanh(double a) {
	    return 0.5 * Math.log((1+a)/(1-a));
	}
	public static double rem(double a, double b) {
	    return a % b;
	}
	public static double besselJn(double a, double b) {
	    try {
	        int ia = (int)Math.round(a);
	        if(ia>=0) {
	            return cern.jet.math.Bessel.jn(ia,b);
	        } else {
	            ia=-ia; 
	            return -1.*cern.jet.math.Bessel.jn(ia,b);
	        }
	    } catch (Exception e) {
	        return Double.NaN;
	    }
	}
	public static double besselKn(double a, double b) {
	    try {
	        int ia = (int)Math.round(a);
	        return cern.jet.math.Bessel.kn(ia,b);
	    } catch (Exception e) {
	        return Double.NaN;
	    }
	}
	public static double besselYn(double a, double b) {
	    try {
	        int ia = (int)Math.round(a);
	        if(ia>=0) { 
	            return cern.jet.math.Bessel.yn(ia,b);
	        } else {
	            ia=-ia; 
	            return -1*cern.jet.math.Bessel.yn(ia,b);
	        }
	    } catch (Exception e) {
	        return Double.NaN;
	    }
	}

	public static double besselI0(double a) {
	    return cern.jet.math.Bessel.i0(a);
	}

	public static double besselI1(double a) {
	    return cern.jet.math.Bessel.i1(a);
	}

	public static double erf(double a) {
	    return cern.jet.stat.Probability.errorFunction(a);
	}

	public static double erfc(double a) {
	    return cern.jet.stat.Probability.errorFunctionComplemented(a);
	}

	public static double gamma(double a) throws Xcept {
	    throw new Xcept("gamma() function not implemented");
	}

}
