/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Fast-Fourier transform 

package JSim.data;
import JSim.util.*;
import JSim.expr.*;

public class FFTData {
	private RealNData uff;
	private RealNData f;

	// constructor
	public FFTData(RealNData u) throws Xcept {
	    if (u.ndim() != 1) throw new Xcept(u,
		"JSim FFT currently only support 1D data");
	    if (! (u.grid(0) instanceof RegularGridData))
		throw new Xcept(u,
		"JSim FFT doesn't yet support irregular grids");
	    RegularGridData t = (RegularGridData) u.grid(0);

	    // create data
	    Unit uffUnit = null;
	    Unit fUnit = null;
	    if (u.unit() != null) 
		uffUnit = u.unit().mult(u.unit());
	    if (t.unit() != null)
		fUnit = Unit.scalar().div(t.unit());
	    uff = new RealNData(u.name() + " power spectrum", 
		uffUnit, new GridData[] { t });
	    f = new RealNData(u.name() + " frequency", 
		fUnit, new GridData[] { t });

	    // Gary does his thing
	    // t.delta() t.ct() t.min() t.max()
	    // uff.set(i, 5.5);
	}

	// query
	public RealNData uff() { return uff; }
	public RealNData f() { return f; }
}
