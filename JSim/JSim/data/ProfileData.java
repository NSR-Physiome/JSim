/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// run-time profiling info for one job

package JSim.data;

import java.io.Serializable;


public class ProfileData implements Serializable {
	public String desc; // job description
	public long startTime; // time job started
	public long stopTime; // time job started
	public Problem[] problems; // problem profiles for job
	public boolean advanced; // advanced profiling available?
	
	// constructor
	public ProfileData() { }
	
	// ProfileData.Problem: profile for one RTProblem
	public static class Problem implements Serializable {
	    public String name; // problem name
	    public int type;    // see types below
	    public int nstate;  // # state vars in problem
	    public long ncalls; // # problem calls
	    public long ncallbacks; // total # callbacks
	    public long[] npdeCallbacks; // PDE cbs by type (if PDE)

	    // constructors
	    public Problem() { }
	}

	// type constants
	public static final int ODE = 1;
	public static final int PDE = 2;
	public static final int FZERO1 = 3;
	public static final int FZERO2 = 4;

	// PDE callback indices
	public static final int LSFEA_tstep = 0;
	public static final int LSFEA_xstep = 1;
	public static final int MacCormack_State = 2;
	public static final int Toms731_State = 3;
	public static final int common_LHB = 4;
	public static final int common_RHB = 5;
	public static final int Toms731_LHB = 6;
	public static final int Toms731_RHB = 7;
	public static final int NPDECALLBACKS = 8;
}

