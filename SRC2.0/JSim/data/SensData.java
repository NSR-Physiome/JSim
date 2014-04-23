/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// N-dimensional sensitivity data

package JSim.data;
import JSim.util.*;
import JSim.expr.*;

public class SensData {

	// create sensitivity data
	public static RealNData makeData(String desc, 
	Data F1, Data F0, double P, double dP, boolean dFdP) 
	throws Xcept {

	    // check agreement
	    int nsamples = F0.nsamples();
	    if (nsamples != F1.nsamples()) throw new Xcept(
		F1, F0, "Sensitivity data point count mismatch");

	    // create empty S (sensitivity result)
	    GridData[] grids = new GridData[F1.ndim()];
	    for (int i=0; i<grids.length; i++) 
		grids[i] = F1.grid(i);
	    RealNData S = new RealNData(desc, 
		Unit.scalar(), grids);

	    // subset calculations
	    int ct = nsamples;
	    if (F1.subset != null 
	    && F1.subset.gridInx >= 0 
	    && F1.ndim() == 1) {
		ct = F1.subset.hix;
		S.subset = new Data.Subset();
		S.subset.gridInx = F1.subset.gridInx;
		S.subset.hix = F1.subset.hix;
	    }

	    // fill in results
	    for (int i=0; i<ct; i++) {
		double F = F0.realVal(i);
		double dF = F1.realVal(i)-F;
		double s = dFdP ? 
		    dF/dP :
		    dF*P / (dP*F); //(dF/F)/(dP/P), but faster
		S.set(i, s);
	    }
	
	    return S;
	}
}


