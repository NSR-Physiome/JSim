/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// membrane between two regions

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class Membrane extends MFComp {
	Region region1, region2;
	Transport.List transp;  // attached transports

	// constructor
	public Membrane(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e,2);
	    region1 = (Region) getParm(e,0,Region.class);
	    region2 = (Region) getParm(e,1,Region.class);
	    region1.attach(this);
	    region2.attach(this);
	    transp = new Transport.List(1);
 	}

	// attach a transport
	protected void attach(Transport t) {
	    transp.add(t);
	}

	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }

        // Membrane.List
        public static class List extends Comp.List {
          public List(int n) {
            super(n);
          }
        }
}
