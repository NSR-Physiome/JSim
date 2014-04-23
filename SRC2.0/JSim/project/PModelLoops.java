/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Loop configuration for a model

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;

public class PModelLoops extends PNamed {
	// constants for Loop.mode.val()
	public static final int OFF = 0;
	public static final int MANUAL = 1;
	public static final int AUTO = 2;

	protected Loop outer;
	protected Loop inner;

	// constructor
	public PModelLoops(PModel p, String n) throws Xcept {
	    super(p, n);
	    outer = new Loop(this, "outer");
	    inner = new Loop(this, "inner");
	}

  	// create job info ASInfo.Loops 
	public ASInfo.Loops makeJobInfo() throws Xcept {
	    int oct = outer.runPrep();
	    int ict = inner.runPrep();
	    ASInfo.Loops info = new ASInfo.Loops();
	    info.counts = new int[] { oct, ict };
	    info.baseVals = pmodel().makeJobInfo();
	    info.runNames = new String[oct*ict];
	    info.nvals = new NamedVal.NList[oct*ict];
	    int k=0;
	    for (int o=0; o<oct; o++) {
		for (int i=0; i<ict; i++) {
	    	    NamedVal.NList nvals = new NamedVal.NList();
		    String label = outer.add(nvals, o);
		    String ilabel = inner.add(nvals, i);
		    if (label == null) {
		    	label = ilabel;
			ilabel = null;
		    }
		    if (ilabel != null)
		    	label = label + " " + ilabel;
		    if (label == null)
		    	label = "loop " + (k+1);
		    info.runNames[k] = label;
		    info.nvals[k] = nvals;
		    k++;
		}
	    }
	    return info;
	}

	// query
	public String diagInfo() { return "ModelLoops " + name(); }
	public String xmlLabel() { return "loops"; }
	public Loop inner() { return inner; }
	public Loop outer() { return outer; }

	// single loop class
	public static class Loop extends PNamed {
	    public ChoiceControl mode;
	    public IntControl ntimes;
	    public IntControl npars;
	    private LoopPar.List pars;
	    private int rct; // # loops in actual run

	    // constructor
	    public Loop(PModelLoops p, String n) throws Xcept {
		super(p, n);
		pars = new LoopPar.List(2);
		mode = new ChoiceControl(this,"mode", 2, 
		     new String[] { "off", "manual", "auto" });
		ntimes = new IntControl(this, "ntimes", 3);
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
		    pars.add(new LoopPar(this, "looppar" + i));

	    	// remove extra children
	    	for (int i=ct0-1; i>=ct; i++) {
		    LoopPar par = looppar(i);
		    int inx = pars.indexOf(par);
		    if (inx <= 0) 
//		        throw new Xcept(
//		    	"Internal error removing looppar " + i);
		 	return;
		    pars.remove(par);
		    remove(par);
	    	}

	    }

	    // run-time prepass
	    public int runPrep() throws Xcept { 
		int rct = nloops();
		for (int i=0; i<npars(); i++) {
		    LoopPar looppar = looppar(i);
		    looppar.vals.runPrep(rct);
		}
		return rct;
	    }
	
	    // add NamedVals for one loop iteration, return label
	    public String add(NamedVal.NList nvals, int i) throws Xcept {
		String label = null;
	    	for (int j=0; j<npars(); j++) {
		    LoopPar looppar = looppar(j);
		    if (looppar == null) continue;
		    if (!looppar.valid()) continue;
		    if (!looppar.enabled()) continue;
		    String n = looppar.par.stringVal();
		    String assn = looppar.vals.runVal(i);
		    if (assn == null) continue;
		    double val = Util.toDouble(assn);
		    nvals.add(NamedVal.create(n, val));
		    if (label == null) 
		    	label = n + "=" + assn;
		}
		return label;
	    }

	    // n loops specified
	    public int nloops() {
		if (mode.val() == OFF) return 1;
		int ct = maxListSize();
		if (mode.val() == MANUAL) ct = ntimes.val();
		if (ct == 0 && hasPar()) ct = ntimes.val();
		if (ct<1) ct = 1;
		return ct;
	    }

	    // does loop have varying parameter
	    public boolean hasPar() {
		for (int i=0; i<npars(); i++) 
		    if (!looppar(i).par.isBlank()) 
			return true;
		return false;
	    }

	    // maximum list size
	    public int maxListSize() {
		int ct = 0;
		for (int i=0; i<npars(); i++) {
		    LoopPar looppar = looppar(i);
		    if (!looppar.valid()) continue;
		    if (!looppar.enabled()) continue;
		    int j = looppar.vals.listSize();
		    if (j>ct) ct=j;
		}
		return ct;
	    }

	    // query
	    public String diagInfo() { return name() + " loop"; }
	    public String xmlLabel() { return "loop"; }
	    public int npars() { return pars.size(); }
	    public LoopPar looppar(int i) { 
		if (i<0 || i>=pars.size()) return null;
		return (LoopPar) pars.get(i);
	    }
	    public boolean ntimesRequired() {
		switch (mode.val()) {
		case OFF: return false;
	 	case MANUAL: return true;
		default: return hasPar() && maxListSize()==0;
		}
	    }

	    // contains info for ParSet storage
	    public boolean isEmpty() {
		for (int i=0; i<npars(); i++) 
		    if (! ((LoopPar) pars.get(i)).par.isBlank()) 
		        return false;
		return true;
	    }
	}

	// single loop par class
	public static class LoopPar extends PValidGroup {
	    public ModelParControl par;
	    public LoopValsControl vals;
	    public BooleanControl enabled;

	    // constructor
	    public LoopPar(Loop p, String n) throws Xcept {
		super(p, n);
		par = new ModelParControl(this, "par", pmodel());
		vals = new LoopValsControl(this, "values", par);
		enabled = new BooleanControl(this, "enabled", true);
	    }

	    // query
	    public String diagInfo() { return "looppar " + name(); }
	    public String xmlLabel() { return "looppar"; }
	    public BooleanControl enableControl() {
		return enabled;
	    }
	    public boolean isBlank() {
		return par.isBlank() && vals.isBlank();
	    }
	    public String validMsg() {
		if (par.isBlank()) return "parameter is missing";
		if (!par.valid()) return par.validMsg();
		if (vals.isBlank()) return "loop values are missing";
		return null;
	    }
	}

	// contains info for ParSet store?
	public boolean isEmpty() {
	    if (outer == null || inner == null)
	        return true;
	    return outer.isEmpty() && inner.isEmpty();
	}

	// control changed: update parset
	public void childChanged(PNamed c) throws Xcept {
	    super.childChanged(c);
	    if (! isEmpty() || pmodel().isEmptyControl(c))
	    	pmodel().setParsModified(true);
	}
}

