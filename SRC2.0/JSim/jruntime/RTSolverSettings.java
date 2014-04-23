/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Solver default settings

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

public class RTSolverSettings {
	private static NamedVal.NList defVals;  // default vals
	private static String pfx = "solver.";

	static {
	    defVals = new NamedVal.NList();

	    // ODE controls
	    defVals.add(NamedVal.create(pfx + "ode_which", 0));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_reltol", 1e-7));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_abstol", 1e-7));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_nstep", 100000));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_stiff", 1000));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_round", 2.3e-16));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_safety", 0.9));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_loselect", 0.2));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_hiselect", 10.0));
	    defVals.add(NamedVal.create(pfx + "ode_Dopri5_beta", 0.04));
	    defVals.add(NamedVal.create(pfx + "ode_Euler_nstep", 2));
	    defVals.add(NamedVal.create(pfx + "ode_RK2_nstep", 2));
	    defVals.add(NamedVal.create(pfx + "ode_Fehlberg_minstep", 1e-4));
	    defVals.add(NamedVal.create(pfx + "ode_Fehlberg_maxstep", 0.1));
	    defVals.add(NamedVal.create(pfx + "ode_Fehlberg_tol", 1e-6));
	    defVals.add(NamedVal.create(pfx + "ode_KM_minstep", 1e-4));
	    defVals.add(NamedVal.create(pfx + "ode_KM_maxstep", 0.1));
	    defVals.add(NamedVal.create(pfx + "ode_KM_tol", 1e-6));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_reltol", 1e-4));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_abstol", 1e-7));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_nstep", 100000));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_round", 1e-16));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_safety", 0.9));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_minord", 3));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_maxord", 7));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_initord", 3));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_newton", 7));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_jacob", 0.001));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_losize", 1.0));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_hisize", 1.2));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_loselect", 0.2));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_hiselect", 8.0));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_locontract", 0.002));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_hicontract", 0.8));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_hiorder", 1.2));
	    defVals.add(NamedVal.create(pfx + "ode_Radau_loorder", 0.8));
	    defVals.add(NamedVal.create(pfx + "ode_RK4_nstep", 2));
	    defVals.add(NamedVal.create(pfx + "ode_CVode_reltol", 1e-7));
	    defVals.add(NamedVal.create(pfx + "ode_CVode_abstol", 1e-8));
	    defVals.add(NamedVal.create(pfx + "ode_CVode_maxsteps", 100000));
	    defVals.add(NamedVal.create(pfx + "ode_CVode_stiff", false));

	    // PDE controls
	    defVals.add(NamedVal.create(pfx + "pde_which", 1));
	    defVals.add(NamedVal.create(pfx + "pde_MacCormack_FCT", false));

	    // Fzero controls
	    defVals.add(NamedVal.create(pfx + "fzero_unbound", "ggopt"));
	    defVals.add(NamedVal.create(pfx + "fzero_bound", "simplex"));
	    defVals.add(NamedVal.create(pfx + "fzero_errtol", 1e-6));
	    defVals.add(NamedVal.create(pfx + "fzero_maxcalls", 500));
	    defVals.add(NamedVal.create(pfx + "fzero_maxiters", 100));
	    defVals.add(NamedVal.create(pfx + "fzero_eps", 1e-6));
	    defVals.add(NamedVal.create(pfx + "fzero_istep", 0.01));
	    defVals.add(NamedVal.create(pfx + "fzero_npoints", 5));
	    defVals.add(NamedVal.create(pfx + "fzero_randseed", 0));
	    defVals.add(NamedVal.create(pfx + "fzero_inittemp", 100));
	    defVals.add(NamedVal.create(pfx + "fzero_populationsize", 50));
	    defVals.add(NamedVal.create(pfx + "fzero_mutationrate", 0.1));
	    defVals.add(NamedVal.create(pfx + "fzero_crossoverrate", 0.5));
	    defVals.add(NamedVal.create(pfx + "fzero_mutationstep", 0.05));
	    defVals.add(NamedVal.create(pfx + "fzero_elitecutoff", 0.5));
	    defVals.add(NamedVal.create(pfx + "fzero_selectmethod", 1));
	    // Random number controls
	    defVals.add(NamedVal.create(pfx + "random_seed", 0));
	}

	// setting variable, if any
	public static RTSettingVar getSettingVar(RTModel model, String n) {
	    NamedVal nval = defVals.namedVal(n);
	    if (nval == null) return null;
	    if (nval.dataType() == Expr.UNDEFINED) return null;
	    return new RTSettingVar(model, n, nval.dataType());
	}

	// simple query
	public static NamedVal.NList getDefaults() { return defVals; }
}


