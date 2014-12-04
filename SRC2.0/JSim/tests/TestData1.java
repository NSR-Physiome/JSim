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

public class TestData1 {

	// mainline
	public static void main(String[] args) throws Exception {
	    if (args.length < 3) throw new Xcept(
		"Usage: TestData1 project dataset format");
	    PApplication appl = new PApplication();
	    JSReadable file = new JSReadable(args[0]);
	    Project proj = new Project("proj1", appl);
	    proj.importXML(file);
	    Data.List data = getList(proj, args[1]);
	    DataFormat.List fmts = appl.dataFormats();
	    DataFormat fmt = fmts.format(args[2]);
	    DataWriter wrt = fmt.createWriter();

	    wrt.writeData(System.out, data);
	}

	// get data list from data set
	public static Data.List getList(
	Project proj, String name) throws Exception {
	    PDataSet dset = (PDataSet) proj.child(name);
	    return dset.dataList();
	}
}

