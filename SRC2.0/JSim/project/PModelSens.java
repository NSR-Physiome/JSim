/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Sensitivity configuration for a model

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;

public class PModelSens extends PNamed {
	public IntControl npars;
	private PNamed.List pars;

	// constructor
	public PModelSens(PModel p, String n) throws Xcept {
	    super(p, n);
	    pars = new PNamed.List(4);
	    npars = new IntControl(this, "npars", 1) {
		public void updateOther() throws Xcept {
		    reconfig();
		}
	    };
	    reconfig();		
	}

	// change npars
	public void reconfig() throws Xcept {
	    int ct = npars.val();
	    if (ct<1) ct=1;
	    int ct0 = pars.size();

	    // add missing children
	    for (int i=ct0; i<ct; i++) 
		pars.add(new SensPar(this, "senspar" + i));

	    // remove extra children
	    for (int i=ct0-1; i>=ct; i++) {
		SensPar par = senspar(i);
		int inx = pars.indexOf(par);
		if (inx <= 0) throw new Xcept(
		    "Internal error removing senspar " + i);
		pars.remove(par);
		remove(par);
	    }
	}

	// get sensitivty job info
	protected ASInfo.Sens makeJobInfo() throws Xcept {
	    SensPar.List runpars = new SensPar.List(4);
	    for (int i=0; i<npars.val(); i++) {
		SensPar senspar = senspar(i);
		if (senspar.isBlank()) continue;
		if (! senspar.enabled()) continue;
		if (! senspar.valid()) throw new Xcept(
		    senspar.par, senspar.validMsg());
		runpars.add(senspar);
	    }
	    int n = runpars.size();
	    if (n < 1) throw new Xcept(pmodel(),
		"No parameters specified for sensitivity analysis");
	    ASInfo.Sens jinfo = new ASInfo.Sens(n);
	    jinfo.baseVals = pmodel().makeJobInfo();
	    for (int i=0; i<n; i++) {
	    	SensPar senspar = (SensPar) runpars.pnamed(i);
		jinfo.parNames[i] = senspar.par.stringVal();
		jinfo.deltas[i] = senspar.delta.realVal();
	    }
	    return jinfo;
	}

	// query
	public String diagInfo() { return "ModelSens " + name(); }
	public String xmlLabel() { return "sens"; }
	public SensPar senspar(int i) { 
	    if (i<0 || i>=pars.size()) return null;
	    return (SensPar) pars.get(i);
	}

	// sensitivity par class
	public static class SensPar extends PValidGroup {
	    public ModelParControl par;
	    public RealControl delta;
	    public BooleanControl enabled;
	
	    // constructor
	    public SensPar(PNamed p, String n)
	    throws Xcept {
		super(p, n);
		par = new ModelParControl(this, "par", pmodel());
		delta = new RealControl(this, "delta", 0.01);
		enabled = new BooleanControl(this, "enabled", true);
	    }

	    // initial value
	    public double initVal() throws Xcept {
		Control c = par.control();
		if (c == null) throw new Xcept(par,
		    "null control() for sensitivity analysis");
		double ival = Double.NaN;
		if (c instanceof AssignControl) 
		     ival = ((AssignControl) c).realVal();
		else if (c instanceof RealControl)
		     ival = ((RealControl) c).realVal();
		else throw new Xcept(c,
		    "Sensitivity analysis does not support this control");
		if (Double.isNaN(ival)) throw new Xcept(c,
		    "Sensitivity parameter starting value must be real number");
		return ival;
	    }

	    // perturbed value
	    public double perturbVal() throws Xcept {
		double pval = delta.realVal();
		if (pval == 0) throw new Xcept(par,
		    "sensitivity delta must be non-zero");
		return initVal() + pval;
	    }

	    // simple query
	    public String diagInfo() { return "senspar " + name(); }
	    public String xmlLabel() { return "senspar"; }
	    public BooleanControl enableControl() {
		return enabled;
	    }
	    public boolean isBlank() {
		return par.isBlank();
	    }
	    public String validMsg() {
		if (par.isBlank()) return "parmeter is blank";
		if (! par.valid()) return par.validMsg();
		return null;
	    }
	}
		    
	// contains any info for ParSet store
	public boolean isEmpty() {
	    for (int i=0; i<npars.val(); i++) {
	        if (senspar(i) == null) continue;
	    	if (! senspar(i).par.isBlank()) return false;
	    }
	    return true;
	}

	// control changed: update parset
	public void childChanged(PNamed c) throws Xcept {
	    super.childChanged(c);
	    if (! isEmpty() || pmodel().isEmptyControl(c))
	    	pmodel().setParsModified(true);
	}

}
