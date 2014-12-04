/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// relation <, <=, >, >=, ~=

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class TRelation extends TEqn implements SeqItem {
	public DomainSet seqLoops; // sequence/calculation loops
	
	// constructor
	public TRelation(TModel model, Eqn eqn) throws Xcept {
	    super(model, eqn);
	    seqLoops = usages().seqLoops();
	}
	
	// query
	public Domain t() { return null; }
	public DomainSet seqLoops() { return seqLoops; }
	public VarUsages vreqs() { return usages(); } 
	public String nodeString() { return toString(); }
	public int op() { return cexpr().op(); }

}
