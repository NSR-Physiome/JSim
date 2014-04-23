/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataWriter for LineDataFormat

package JSim.data;

import JSim.util.*;
import JSim.expr.*;
import java.io.*;
import java.net.URL;

public class LineDataWriter extends DataWriter {

	// private state
	private PrintWriter writer;

	// constructor
	public LineDataWriter(LineDataFormat f) { 
	    super(f);
	}

	// write data
	public void writeData(Writer wrt, Data.List dlist) throws Xcept {
	    writer = new PrintWriter(wrt);
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		writer.print(data.legend() + data.legendGrids());
		if (data.group() != null)
		    writer.print(": " + data.group());
	    	writer.print(" = {");
		if (data instanceof GridData) {
		    GridData grid = (GridData) data;
		    for (int j=0; j<grid.ct(); j++) 
			writer.print(" " + pretty(grid.realVal(j)));
		} else if (data instanceof RealNData) {
		    for (int j=0; j<data.nsamples(); j++) 
			writer.print(" " + dataStr((RealNData) data, j));
		}
		Unit u = data.unit;
		if (u==null) u = Unit.scalar();
	    	writer.println(" } " + u.pubName());
	    }
	}

	// RealNData data string
	private String dataStr(RealNData data, int inx) 
	throws Xcept {
	    if (data.ndim() == 0)
		return pretty(data.realVal(inx));
	    String s = "(";
	    int[] gpos = data.gridPos(inx);
	    for (int i=0; i<data.ndim(); i++) {
		GridData grid = data.grid(i);
		double val = grid.realVal(gpos[i]);
		s=s + pretty(val) + ",";
	    }
	    double val = data.realVal(inx);
	    s = s + pretty(val);
	    return s + ")";
	}
}
