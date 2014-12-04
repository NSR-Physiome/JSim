/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one node in Sequencer graph

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import java.util.*;

public class SeqNode {
	private String name;
	private String fft;  // feedforward time domain or null
	private LinkedHashSet<String> loops;
	
	// constructor
	public SeqNode(String name) {
	    this.name = name;
	    loops = new LinkedHashSet<String>();
	}
	
	// set stufft
	protected void setLoops(LinkedHashSet<String> loops) {
	    this.loops = loops;
	}
	protected void setFFT(String x) throws Xcept { 
	    if (! loops.contains(x)) throw new Xcept(
	    	"Illegal feedfwd: " + x + " for node " + this + loops);
	    if (fft != null && ! fft.equals(x))
	        throw new Xcept("Ambigous feedfwd for node " + this
		+ ": " + fft + " vs " + x);
	    fft = x;
	}

	// query
	public String fft() { return fft; }
	public String toString() { return name; }
	public String name() { return name; }
	public boolean hasLoop(String x) {
	    return loops.contains(x);
	}
	public int hashCode() { return name.hashCode(); }
	public boolean equals(Object o) {
	    if (! (o instanceof SeqNode)) return false;
	    SeqNode n = (SeqNode) o;
	    return name.equals(n.name) && loops.equals(n.loops);
	}
	public LinkedHashSet<String> loops() { return loops; }
}
