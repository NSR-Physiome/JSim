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

public class XSTestParDump {
	public final static void main(String[] args) 
	throws Exception {
	    String fname = args[0];
	    File f = new File(fname);
	    XSParFile xsparf = new XSParFile(f);
	    XSParFile.Par.NList pars = xsparf.mpars();
	    for (int i=0; i<pars.size(); i++) {
		XSParFile.Par par = pars.par(i);
		System.err.println(par.name() + "=" +
		    par.value());
	    }
	}
}
