/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// interpolation value test

package JSim.tests; import JSim.data.*;

import JSim.util.*;
import java.io.*;

public class InterpTest {

	// constructor
	public InterpTest(String[] args) throws Xcept {

	    // parse command line
	    if (args.length < 4) throw new Xcept(
	    	"Usage: InterpTest data-file curve interp-alg qval[0] ...");
	    File f = new File(args[0]);
	    String uname = args[1];
	    int ialg = Util.toInt(args[2]);    
	    int n = args.length-3;
	    double[] qvals = new double[n];
	    for (int i=0; i<n; i++) 
	    	qvals[i] = Util.toDouble(args[3+i]);

	    // read data file
	    JSReadable r = new JSReadable(f);
	    String text = r.readText();
	    String sfx = r.fileSuffix();
	    DataFormat.List fmts = new DataFormat.List();
	    DataFormat fmt = fmts.forSuffix(sfx);
	    DataReader rdr = fmt.createReader(text);
	    Data.List dataList = rdr.readData();

	    // find data u
	    RealNData udata = null;
	    for (int i=0; i<dataList.size(); i++) {
	    	Data data = dataList.get(i);
		if (data.desc().equals(uname)
		|| data.name().equals(uname))
		    udata = (RealNData) data;
	    }
	    if (udata == null) throw new Xcept(
	    	"No curve <" + uname + "> in " + f);
	    
	    // query 
	    udata.setInterp(ialg);
	    double v = udata.realVal(qvals);
	    System.out.println(uname + Util.pretty(qvals) + "=" + v);
	}
	    
	// f(grids)
	// mainline
	public static void main(String[] args) throws Xcept {
	    new InterpTest(args);
	}
}
