/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// any item sequenced for calculation

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public interface SeqItem {
	Domain t();        // time domain
	VarUsages vreqs();  // vus reqd for sequencing
	DomainSet seqLoops(); // loops over which is calc'ed
	String nodeString(); // for SeqGraph node name
}
