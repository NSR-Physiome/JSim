/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one edge in Sequencer graph

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 

public class SeqEdge {
	private SeqNode src, dest; // source & destination nodes
	
	// constructor
	public SeqEdge(SeqNode src, SeqNode dest)
	throws Xcept {
	    this.src = src;
	    this.dest = dest;
	    if (src.equals(dest)) throw new Xcept(
	    	"Illegal SeqEdge (src=dest): " + this);
	}
	
	// query
	public SeqNode dest() { return dest; }
	public SeqNode src() { return src; }
	public int hashCode() { return 2 * src.hashCode() - dest.hashCode(); }
	public boolean equals(Object o) {
	    if (! (o instanceof SeqEdge)) return false;
	    SeqEdge e = (SeqEdge) o;
	    if (! src.equals(e.src)) return false;
	    if (! dest.equals(e.dest)) return false;
	    return true;
	}
	public String toString() {
	    return "" + src + "->" + dest;
	}
}
