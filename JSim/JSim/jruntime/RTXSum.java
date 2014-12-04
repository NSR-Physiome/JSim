/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Summation function

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

public class RTXSum extends RTXFunc {
 
	// constructor
    	public RTXSum(RTModel m, String n) 
	    throws Xcept { super(m, n); }

	// return planning info
	public static Info getInfo(Expr.List args) throws Xcept {
	    if (args.size() != 1 && args.size() != 3)
		throw new Xcept("sum() function requires 1 or 3 arguments");
	    Info info = new Info();
	    info.ninputs = args.size();
	    info.dataType = Expr.REAL;
	    return info;
	}

	// return real value
	public double realCalculate(RealNData[] args) throws Xcept {
	    RealNData u = args[0];
	    if (args.length == 1) 
	    	return  u.sum();
	    if (args.length == 3) {
	    	RealNData xmin = args[1];
	    	RealNData xmax = args[2];
	    	return u.sum(xmin.realVal(), xmax.realVal());
	    } 
	    throw new Xcept("sum() function requires 1 or 3 arguments");
       }
}		

