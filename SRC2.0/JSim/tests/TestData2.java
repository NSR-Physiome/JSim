/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test data IO from project

package JSim.tests; import JSim.project.*;

import java.io.*;
import JSim.util.*;
import JSim.data.*;

public class TestData2 {

	// mainline
	public static void main(String[] args) throws Exception {
	    if (args.length < 2) throw new Xcept(
		"Usage: TestData2 infile outformat");
	    File infile = new File(args[0]);
	    String outfmtName = args[1];

	    PApplication appl = new PApplication();
	    DataFormat.List fmts = appl.dataFormats();
	    
	    String sfx = UtilIO.fileSuffix(infile);
	    DataFormat infmt = fmts.forSuffix(sfx);
	    if (infmt == null) throw new Xcept(
		"Unknown file suffix " + sfx);
	    DataReader rdr = infmt.createReader(infile);
	    Data.List data = rdr.readData();

	    DataFormat outfmt = fmts.format(outfmtName);
	    DataWriter wrt = outfmt.createWriter();
	    wrt.writeData(System.out, data);
	}

}
