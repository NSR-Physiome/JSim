/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// CellML test 

package JSim.tests; import JSim.cellml.*;

import java.io.*;
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import org.w3c.dom.Document; 

public class CMLTest {

	// mainline
	public static void main(String[] args) throws Exception {
	    Util.verbose = false;
	    boolean unitConv = true;
	
	    int i=0;
	    while (args[i].charAt(0) == '-') {
	    	if (args[i].equals("-v")) {
		    Util.verbose = true;
		    i++;
//		} else if (args[i].equals("-off")) {
//		    unitConv = false;
//		    i++;
		} else 
		    usage();
	    }
	    if (i >= args.length) usage();
	    File f = new File(args[i]);
	    Document doc = UtilXML.parse(f);

	    // load CellML/system units files
	    String cellmlUnits = "cellmlunit.mod";
	    ModelReader umodel = new ModelReader(new File(cellmlUnits));
	    UnitNList cunits = umodel.units;
	    String jsimHome = System.getProperty("jsim.home");
	    String nsrUnits = jsimHome + "/common/nsrunit.mod";
	    umodel = new ModelReader(new File(nsrUnits));
	    UnitNList sunits = umodel.units;

	    CMLDoc cmlDoc = new CMLDoc(doc, cunits, sunits);
	    Writer wrt = new OutputStreamWriter(System.out);
	    cmlDoc.writeMML(wrt);
	}

	private static void usage() throws Exception {
	    throw new Exception("Usage: CMLDoc [-v] file-name");
	}
}
