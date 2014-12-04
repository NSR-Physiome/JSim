/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// generate RTML stress test 

package JSim.gui.rtml;

import JSim.util.*;

public class GRTStress {
	public static String AZ = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";


	// generate random names module
	public GRTStress(int nin, int nout, int imax, int omax, 
	boolean units) {
	    String[] inames = new String[nin];
	    String[] onames = new String[nout];

	    // generate names
	    for (int i=0; i<nin; i++) 
	    	inames[i] = newName(imax);
	    for (int i=0; i<nout; i++) 
	    	onames[i] = newName(omax);	

	    // write MML
	    log("import nsrunit;");
	    log("unit conversion off;");
	    log("math main {");
	    log("  realDomain t; t.min=0; t.max=10; t.delta=1;");
	    log("  realDomain x; x.min=0; x.max=1; x.delta=.1;");
	    
	    // write inputs
	    for (int i=0; i<nin; i++) {
	        String s = "  real " + inames[i] + " = 1";
		if (units) s = s + " meter/second";
		log(s + ";");
	    }
	    
	    // write output
	    for (int i=0; i<nout; i++)
	    	log("  real " + onames[i] + " = " + 
		inames[0] + " + " + i + ";");

	    log ("}");
	}
	
	// random name
	private String newName(int ct) {
	    ct = (int) (ct * (Math.random()*0.5+0.5));
	    String s = "";
	    for (int i=0; i<ct; i++) {
	    	int j = (int) (Math.random()*AZ.length());
	    	s = s + AZ.charAt(j%AZ.length());
	    }
	    return s;
	}

	// log to stdout
	private void log(String s) {
	    System.out.println(s);
	}

	// mainline
	public static final void main(String[] args) 
	throws Exception {
	    if (args.length != 5) throw new Exception(
	    	"Usage: GRTStress nin nout imax omax units?");
	    int nin = Util.toInt(args[0]);
	    int nout = Util.toInt(args[1]);
	    int imax = Util.toInt(args[2]);
	    int omax = Util.toInt(args[3]);
	    boolean units = Util.toBoolean(args[4]);
	    new GRTStress(nin, nout, imax, omax, units);
	}
}
