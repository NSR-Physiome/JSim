/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// selection from other functions

package JSim.project; import JSim.aserver.*;
import JSim.util.*;
import JSim.data.*;
import JSim.fgen.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class FuncGen extends PNamed {

	// controls
	public ChoiceControl which;

	// state
	private FuncGenSlave[] slaveTbl;
	private Control.List domains; // domain controls
	private FuncGenPreview previewer; // calculates preview data
	private RealNData previewData; // latest preview data

	private String[] slaveNames = new String[] { 
	    "Pulse1", "Pulse2", "Pulse3",
	    "ExtendedPulse3", "Ramp", "SquareWaveTrain",
	    "SawtoothTrain", "SineTrain", "Exponential",
	    "Gaussian", "LagNormal", "Longtail",
	    "GammaVar", "Poisson", "RandomWalk",
	    "DataCurve"
	};
	private String[] slaveDescs = new String[] { 
	    "Single pulse", "Double pulse", "Triple pulse", 
	    "Extended Triple pulse", "Ramp", "Square-wave Train",
	    "Sawtooth Train", "Sine Train", "Exponential",
	    "Gaussian", "Lagged Normal Density", "Longtail",
	    "Gamma Variate", "Poisson-like", "Random Walk",
	    "Data" };
	    
	// constants
	private static final int FGENDATA = 15;

	// constructor
	public FuncGen(PModelVars p, String n) throws Xcept {
	    super(p,n);
	    addDescCntl();
	    which = new ChoiceControl(this, "which", 0, slaveNames);
	    slaveTbl = new FuncGenSlave[slaveNames.length];
	    for (int i=0; i<slaveTbl.length; i++) 
	    	slaveTbl[i] = (i != FGENDATA) ?
		    new FuncGenSlave(this, slaveNames[i], slaveDescs[i]) :
		    new FuncGenData(this, slaveNames[i], slaveDescs[i]);

	    domains = new Control.List(4);
	    domains.add(new DomainControl(this, "domain0", true));
	    domains.add(new DomainControl(this, "domain1", false));
	    domains.add(new DomainControl(this, "domain2", false));

	    // update ASModel func gens list
	    pmodel().vars().updateFuncGenNames();	    
// line below fails at GUI Func Gen create because
// assignment calculation done before funcgens updated
//	    pmodel().vars().funcGenNamesChanged = true;

	    // set domain0, if available
//	    ASVar t = pmodel().firstDomain();
//	    if (t != null) domains.control(0).setVal(t.name());
	}

	// simple query
	public String diagInfo() { return "FuncGen " + name(); }
	public String xmlLabel() { return "function"; }
	public DomainControl domain(int i) {
	    return (DomainControl) domains.get(i);
	}
	public PNamed func(int i) { return slaveTbl[i]; }
	public FuncGenData funcData() { return (FuncGenData) func(FGENDATA); }

	// childChanged erases previewData
	protected void childChanged(PNamed child) throws Xcept {
	    super.childChanged(child);
	    previewData = null;
	}

	// get/set source data
	public Data getSourceData() { 
	    if (which.val() != FGENDATA) return null;
	    return funcData().getSourceData();
	}
	public void setSourceData(Data data) {
	    funcData().setSourceData(data);
	}

	// preview data
	public RealNData previewData() throws Xcept {
	    if (previewData != null) return previewData;
	    if (previewer == null) 
	    	previewer = new	FuncGenPreview(this);
	    previewData = previewer.calcData();
	    return previewData;
	}
}

