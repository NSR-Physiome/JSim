/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// NSR Tac (time-activity curve) Data

package JSim.data;
import java.util.ArrayList;
import java.io.*;
import JSim.util.*;

public class TacData extends Data.List implements DiagInfo {

	// static constants
	public static final int AUX = 1; 
	public static final int PHYSIOL = 2;
	public static final int INPUT = 3;
	public static final int SAMPLE = 4;

	// state
	protected ArrayList<Run> runs; // arraylist for each run

	// empty constructor
	public TacData() {
	    super(4);
	    runs = new ArrayList<Run>(4);
	}

	// query
	public String diagInfo() { return "NSR TAC file"; }
	public Run run(int i) { return (Run) runs.get(i); }

	// add run to tac file
	public void addRun() {
	    Run run = new Run();
	    run.inx = runs.size();
	    runs.add(run);
	}
	
	// add aux value
	public void addAuxValue(String name, double value)
	throws Xcept {
	    Data data = new RealNData(name, null, null,
	    	new double[] { value });
	    data.setName(name);
	    add(data);
	}

	// add curve of type to run
	public void addCurve(Data data, int r, int type)
	throws Xcept {
	    if (data instanceof GridData) 
		data = new RealNData((GridData) data);
	    if (data.ndim() != 1) throw new Xcept(data,
		"TacData may contain only 1-dimensional data"); 
	    if (! (data instanceof RealNData)) throw new Xcept(data,
		"TacData does not support class " + data.getClass());
	    Run run = run(r);
	    int seq = 0;
	    switch (type) {
	    case PHYSIOL: seq = run.nphysiol++; break;
	    case INPUT: seq = run.ninput++; break;
	    case SAMPLE: seq = run.nsample++; break;
	    }

	    // make clean copy of data so name/desc to perturbed
	    data = data.copy();
	    if (Util.isBlank(data.desc) && !isTACName(data.name))
		data.setDesc(data.name);
	    data.setName(curveName(r, type, seq));
	    add(data);
	}

	// is String a TAC curve name? (somewhat imperfect)
	private boolean isTACName(String n) {
	    if (n == null) return false;
	    if (n.length() < 4) return false;
	    if (n.charAt(0) != 'R') return false;
	    if (! Character.isDigit(n.charAt(1))) return false;
	    for (int i=2; i<n.length(); i++) {
		char c = n.charAt(i);
		if (Character.isDigit(c)) continue;
		if (c == 'i') continue;
		if (c == 's') continue;
		if (c == 'p') continue;
	        return false;
	    }
	    if (! Character.isDigit(n.charAt(n.length()-1))) 
		return false;
	    return true;
	}

	// curve name
	public String curveName(int r, int type, int seq) 
	throws Xcept {
	    String s = "R" + (r+1);
	    switch (type) {
	    case PHYSIOL: 
		s = s + "p";
	 	break;
	    case INPUT: 
		s = s + "i";
		break;
	    case SAMPLE: 
		s = s + "s";
		break;
	    default: 
		throw new Xcept("Invalid TAC curve type: " + type);
	    }
	    return s + (seq+1);
	}

	// query by name
	public Data curve(String n) {
	    for (int i=0; i<size(); i++) {
		Data data = data(i); 
		if (n.equals(data.name()))
		    return data;
	    }
	    return null;
	}

	// Run datatype
	protected class Run {
	    public int inx;	// run number (from 0)
	    public int nphysiol; // # physiol curves in run
	    public int ninput;	// # input curves in run
	    public int nsample;	// # sample curves in run

	    // constructor
	    public Run() {
		nphysiol = ninput = nsample = 0;
	    }	
	}
}

