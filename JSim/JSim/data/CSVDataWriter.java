/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataWriter for CSVDataFormat

package JSim.data;

import JSim.util.*;
import java.io.*;
import java.net.URL;

public class CSVDataWriter extends DataWriter {

	// private state
	private PrintWriter writer;
	
	// constructor
	public CSVDataWriter(CSVDataFormat f) { 
	    super(f);
	}

	// write data
	public void writeData(Writer wrt, Data.List list) throws Xcept {
	    warning = null;

	    // check support
	    GridData grid0 = null;
	    Data.List dlist = new Data.List(list.size());
	    int skipCt = 0;
	    for (int i=0; i<list.size(); i++) {
		Data data = list.data(i);
		switch (data.ndim()) {
		case 0:
		    dlist.add(data);
		    break;
		case 1:
		    if (grid0 == null) grid0 = data.grid(0);
		    GridData grid = data.grid(0);
		    if (! grid.sameSamplesAs(grid0)) throw new Xcept(this,
		        data, "mismatched domains not supported");
		    dlist.add(data);
		    break;
		default:
		    skipCt++;
		    break;
		}
	    }
	    if (grid0 == null || dlist.size() < 1) throw new Xcept(this,
		"CSV data format only supports 1D curves.");
	    if (skipCt > 0) warning = 
		"" + skipCt + " constants/curves excluded from output. " +
		"CSV data format supports only 1D curves.";

	    // write header line
	    writer = new PrintWriter(wrt);
	    String s = grid0.name(); 
	    if (Util.isBlank(s)) s = grid0.desc();
	    if (Util.isBlank(s)) s = "x";
	    s = s.replace('"', '_');
	    writer.print(s);
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		s = data.glegend().replace('"', '_');
		writer.print("," + s);
	    }
	    writer.println("");

	    // write data lines
	    for (int j=0; j<grid0.ct(); j++) {
		double f = grid0.realVal(j);
		writer.print(pretty(f) + ",");
		for (int i=0; i<dlist.size(); i++) {
		    Data data = dlist.data(i);
		    int k = (data.ndim() == 0) ? 0 : j;
		    double d = data.realVal(k);
		    if (i>0) writer.print(",");
		    writer.print(pretty(d));
		}
		writer.println("");
	    }
	}
}
