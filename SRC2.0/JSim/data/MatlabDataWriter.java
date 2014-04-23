/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataWriter for MatlabDataFormat

package JSim.data;

import JSim.util.*;
import java.io.*;
import java.net.URL;

public class MatlabDataWriter extends DataWriter {

	// private state
	private PrintWriter writer;
	
	// constructor
	public MatlabDataWriter(DataFormat f) { 
	    super(f);
	}

	// write data
	public void writeData(Writer wrt, Data.List dlist) throws Xcept {
	    writer = new PrintWriter(wrt);

	    // write each
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);

		// clean name
		String name = data.glegend();
		name = matlabName(name);

		// header line
	    	String doms = "";
		if (!(data instanceof GridData) && data.ndim() > 0) {
		    doms = "(";
	    	    for (int j=0; j<data.ndim(); j++) {
		    	if (j>0) doms = doms + ",";
			String d = data.grid(j).desc;
			if (Util.isBlank(d)) d = data.grid(j).name;
			if (Util.isBlank(d)) d = "?";
		    	doms = doms + d;
	    	    }
		    doms = doms + ")";
		}
		String group = (data.group() == null) ? 
		    "" : ( ": " + data.group());
		String unit = (data.unit() == null) ? 
		    "" : (" units=" + data.unit().pubName());
		writer.println("% JSim data: " + data.legend() + 
		    doms + group + unit + ";");

		// data
	    	if (data instanceof RealNData)
		    writeNData(name, (RealNData) data);
	    	else if (data instanceof GridData)
		    writeGrid(name, (GridData) data);
	    	else throw new Xcept(data,
		    "Unsupported Data subclass in MatlabDataWriter");
		writer.println("");
	    }
	}

	// write N-dimensional data
	private void writeNData(String name, RealNData data) throws Xcept {
	    writeSamp(name, data);
	    for (int i=0; i<data.ndim(); i++) 
		writeGrid(name + "__" + (i+1), data.grid(i));
	}

	// write GridData
	private void writeGrid(String name, GridData grid) throws Xcept {
	    if (! (grid instanceof RegularGridData)) {
		writeSamp(name, grid);
		return;
	    }

	    RegularGridData rgrid = (RegularGridData) grid;
	    writer.println(name + " = " + pretty(rgrid.min()) + ":" +
		pretty(rgrid.delta()) + ":" + pretty(rgrid.max()) +
		";");
	}

	// write samples
	private void writeSamp(String name, Data data) throws Xcept {

	    // 0D and 1D
	    if (data.ndim() < 2) {
	    	writer.print(name + " = [");
	    	for (int i=0; i<data.nsamples(); i++) 
		    writer.print(" " + pretty(data.realVal(i)));
	    	writer.println(" ];");
		return;
	    }

	    // N-dim samples
	    int N = data.ndim();
	    int[] gpos = new int[N];
	    int[] cts = new int[N];
	    for (int i=0; i<N; i++)
		cts[i] = data.grid(i).ct();
	    int lct = data.nsamples()/cts[N-1];

	    // one line varying last dimension
	    for (int i=0; i<lct; i++) {
		writer.print(name + "(");
		for (int j=0; j<N-1; j++) {
		    if (j != 0) writer.print(",");
		    writer.print("" + (gpos[j]+1));
		}
		writer.print(",:)=[");
		for (int j=0; j<cts[N-1]; j++) {
		    gpos[N-1] = j;
		    writer.print(" " + 
			pretty(((RealNData) data).realVal(gpos)));
		}
		writer.println(" ];");
		writer.flush();
		if (i<lct-1) nextLine(gpos, cts);
	    }
	}

	// update gpos for next line
	private void nextLine(int[] gpos, int[] cts) {
	    int i=0;
	    while (true) {
		gpos[i]++;
	        if (gpos[i] < cts[i]) 
		    break;
		gpos[i]=0;
		i++;
	    }
	}

	// matlab data name
	private String matlabName(String s) {
	    StringBuffer buf = new StringBuffer(s);
	    for (int i=0; i<buf.length(); i++) {
		char c = buf.charAt(i); 
		if (! Character.isLetterOrDigit(c))
		    buf.setCharAt(i, '_');
	    }
	    while (buf.length() > 1 &&
	    buf.charAt(buf.length()-1) == '_')
		buf.deleteCharAt(buf.length()-1);
	    if (Character.isDigit(buf.charAt(0)))
	    	buf.insert(0, 'X');
	    return buf.toString();
	}
		 
}
