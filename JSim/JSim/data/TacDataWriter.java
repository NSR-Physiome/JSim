/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataWriter for TacDataFormat

package JSim.data;

import java.util.*;
import java.io.*;

import JSim.util.*;

public class TacDataWriter extends DataWriter {

	// private state
	private PrintWriter out;
	private TacData tacData;

	// constructor
	public TacDataWriter(TacDataFormat f) { 
	    super(f);
	}

	// write to file
	public void writeData(Writer wrt, Data.List dlist) throws Xcept {
	    warning = null;

	    // create tacData
	    int skipCt = 0;;
	    if (dlist instanceof TacData) 
		tacData = (TacData) dlist;
	    else {
		tacData = new TacData();
		tacData.addRun();
		for (int i=0; i<dlist.size(); i++) {
		    Data data = dlist.data(i);
		    if (data.ndim() == 1) 
		    	tacData.addCurve(data, 0, TacData.SAMPLE);
		    else
			skipCt++;
		}
	    }
	    if (tacData.size() < 1) throw new Xcept(this,
		"TAC format only supports 1D curves.");
	    if (skipCt > 0) warning = 
		"" + skipCt + " constants/curves excluded from output. " +
		"TAC data format supports only 1D curves.";
	    
	    // write file headers
	    out = new PrintWriter(wrt);
 	    String filename = fileName();
	    Date date = new Date(System.currentTimeMillis());
	    out.println(
		"'TAC file name  (Ver. 2.0):'\t'" + 
		filename + "'\t'CREATED: " +
		date + "'");
	    out.println("'experiment type:'\t\t'JSim 1.6'");
	    out.println("'experiment description:'\t' '");
	    out.println("'experiment name:'\t\t' '");
	    out.println("'# of auxiliary data:'\t\t0");

	    // write runs
	    out.println("'# of runs:'\t\t\t" + tacData.runs.size());
	    for (int i=0; i<tacData.runs.size(); i++) 
		write(tacData.run(i));
	}

	// write entire run output
	private void write(TacData.Run run) throws Xcept {
	    out.println("'run description:'\t\t' '");
	    out.println("'# of auxiliary data:'\t\t0");
	    write(run, TacData.PHYSIOL);
	    write(run, TacData.INPUT);
	    write(run, TacData.SAMPLE);
	}

	// write out one section
	private void write(TacData.Run run, int type) throws Xcept {

	    // count curves
	    int ct = 0;
	    String typedesc = null;
	    switch (type) {
	    case TacData.PHYSIOL: 
		ct = run.nphysiol; 
		typedesc = "physiol"; 
		break;		    
	    case TacData.INPUT: 
		ct = run.ninput; 
		typedesc = "input function"; 
		break;		    
	    case TacData.SAMPLE: 
		ct = run.nsample; 
		typedesc = "sample"; 
		break;		    
	    }

	    // collect curves
	    Data.List curves = new Data.List(4);
	    for (int i=0; i<ct; i++) {
		String n = tacData.curveName(run.inx, type, i);
		Data curve = tacData.curve(n);
		if (curve == null) throw new Xcept(this, 
		    "Internal index error " + n);
		curves.add(curve);
	    }

	    // headers
	    out.println("'# of " + typedesc + 
		" datasets:'\t" + curves.size());

	    // output curves
	    for (int i=0; i<ct; i++) {
		RealNData curve = (RealNData) curves.data(i);
		GridData grid = curve.grid(0);
		out.println("'" + typedesc + 
		    " dataset description:'\t' '");
		out.println("'# of lines:'\t" + grid.ct());
		out.println("'# of fields:'\t2");
		out.println("'" + label(grid) + "'\t'" + 
		    label(curve) + "'");
		int[] inxarr = new int[1];
		for (int j=0; j<grid.ct(); j++) {
		    inxarr[0] = j;
		    out.println(pretty(grid.realVal(j))
			+ "\t" + pretty(curve.realVal(inxarr)));
		}
	    }	
	}

	// column header label
	private String label(Data data) {
	    String l = data.desc();
	    if (l == null) l = "";
	    l = l.replace('"', '_');
	    l = l.replace("'".charAt(0), '_');
	    return l;
	}

}

