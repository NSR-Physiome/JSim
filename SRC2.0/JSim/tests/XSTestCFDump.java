/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
  
// XSim tests
 
package JSim.tests; import JSim.xsim.*;
 
import java.io.*;
import JSim.util.*;
import JSim.data.*;
import JSim.mml.*;
import JSim.project.*;
import org.w3c.dom.Document;

public class XSTestCFDump {
	public final static void main(String[] args) 
	throws Exception {
	    CF cf = new CF();
	    CFIvar time = new CFIvar(cf, "time");
	    time.set("loc", "130");
	    time.pstart.set("init", "0");
	    time.pstop.set("init", "30");
	    time.pincr.set("init", "0.1");
	    CFPar V = new CFPar(cf, CFPar.REAL, "V");
	    V.set("loc", "1");
	    V.set("init", "0.5");
	    CFPar Cout = new CFPar(cf, CFPar.REAL, "Cout");
	    Cout.set("loc", "201");
	    Cout.set("output");
	    Cout.set("dynamic");
	    cf.writeMML(System.out);
	}
}
