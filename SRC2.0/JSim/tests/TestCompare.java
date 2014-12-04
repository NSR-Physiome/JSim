/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// quick test for DataCompare

package JSim.tests; import JSim.project.*;

import java.io.*;
import JSim.util.*;
import JSim.data.*;

public class TestCompare {

	// mainline
	public static void main(String[] args) throws Exception {
	    if (args.length < 3) throw new Xcept(
		"Usage: TestResid project-file dset1 dset2 [dset3]");
	    PApplication appl = new PApplication();
	    JSReadable file = new JSReadable(args[0]);
	    Project proj = new Project("proj1", appl);
	    proj.importXML(file);
	    Data.List data = getList(proj, args[1]);
	    Data.List refs = getList(proj, args[2]);
	    DataCompare comp = new DataCompare(refs);
	    comp.setData(data);
	    if (args.length > 3) {
		Data.List pwgts = getList(proj, args[3]);
		comp.setPointWgts(pwgts);
	    }
	    double err = comp.rmsError();
	    System.out.println("rmsError=" + err);
	}

	// get data list from data set
	public static Data.List getList(
	Project proj, String name) throws Exception {
	    PDataSet dset = (PDataSet) proj.child(name);
	    return dset.dataList();
	}
}
