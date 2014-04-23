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

public class XSTestRead {		// read test
	final public static void main(String args[]) 
	throws Exception {
	    String n = Util.jsimHome() + File.separator + 
		"common" + File.separator + "nsrunit.mod";
	    Model unitModel = new ModelReader(new File(n));
	    Util.verbose = true;
	    File f = new File(args[0]);
	    CF cf = new CFReader(f, unitModel);
	    cf.writeMML(System.out);
	}
}

