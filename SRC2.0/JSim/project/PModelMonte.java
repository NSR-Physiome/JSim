/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Monte-Carlo configuration for model

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;

import java.util.*;
import org.w3c.dom.*;

public class PModelMonte extends PNamed {

	// run-time controls
	public IntControl noptims;  // # optimizations
	public IntControl randomSeed; // random seed (0=randomized)
	public ChoiceControl dist; // random distribution PDF
	public RealControl magnitude; // magnitude of randomization
	public ChoiceControl addMethod; // add, multiply, other?

	// graph controls
	public ChoiceControl graphView; // which graph?
	public ModelParControl graphPar1;  // 1st parameter (hist/scatter)
	public ModelParControl graphPar2;  // 2nd parameter (scatter)
	public IntControl    graphOptNo;  // optim run #
	public BooleanControl graphLog;  // log scale on optim graphs?
	public IntControl    graphBins;  // # bins for histogram

	// graph constants
	public static final String[] GRAPH_NAMES = new String[] {
	    "histogram", "scatter",
	    "optimPars", "optimNormPars", "optimRmsError", 
	    "optimDataToMatch",
	    "optimUnwgtResid", "optimWgtResid", "optimPointWgts"
	};
	public static final int GRAPH_HISTOGRAM = 0;
	public static final int GRAPH_SCATTER = 1;
	public static final int GRAPH_OPTIM = 2;

	// run info
	private Hashtable<String, String> args; // control vals
	private ASInfo.Mopt jobInfo; // job info for run
	private Data[][] noisyData; // generated data

	// constructor
	public PModelMonte(PModel p, String n) throws Xcept {
	    super(p, n);
	    noptims = new IntControl(this, "noptims", 50);
	    randomSeed = new IntControl(this, "randomSeed", 0);
	    dist = new ChoiceControl(this, "dist", 
	        NoiseMaker.GAUSSIAN,
	        new String[] { "uniform", "gaussian" });
	    magnitude = new RealControl(this, "magnitude", 0.1);
	    addMethod = new ChoiceControl(this, "addMethod", 
	        NoiseMaker.PROPORTIONAL,
	        new String[] { "proportional", "additive" });
	    addMethod.addLegacyLabel("scaled", "proportional");
	    addMethod.addLegacyLabel("fixed", "additive");
	    graphView = new ChoiceControl(this, "graphView", 0,
	    	GRAPH_NAMES);
	    graphPar1 = new ModelParControl(this, "graphPar1", pmodel());
	    graphPar2 = new ModelParControl(this, "graphPar2", pmodel());
	    graphOptNo = new IntControl(this, "graphOptNo", 1);
	    graphLog = new BooleanControl(this, "graphLog", false);
	    graphBins = new IntControl(this, "graphBins", 10);
	}

	// create job
	public PModelMoptJob createJob() throws Xcept {
	    if (noptims.val() < 2) throw new Xcept(
	    	"# optimization must be at least 2");
	    args = new Hashtable<String, String>();
	    addArg(noptims);
	    addArg(randomSeed);
	    addArg(dist);
	    addArg(magnitude);
	    addArg(addMethod);
	    ASInfo.Mopt jobInfo = makeJobInfo();
	    return new PModelMoptJob(pmodel(), jobInfo);
	}

	// add control val to args
	private void addArg(Control c) throws Xcept {
	    args.put(c.name(), c.stringVal());
	}
	
	// create job info
	protected ASInfo.Mopt makeJobInfo() throws Xcept {
	    jobInfo = new ASInfo.Mopt();
	    jobInfo.optim = pmodel().optim().makeJobInfo();
	    jobInfo.nvals = null; // not yet implemented
	    makeNoisyData(jobInfo.optim.refData);
	    jobInfo.refData = noisyData;
	    jobInfo.saveOptimResults = true;
	    jobInfo.noabort = false;
	    return jobInfo;
	}

	// create noisy data from refData
	private void makeNoisyData(Data[] refData) throws Xcept {
	    NoiseMaker maker = new NoiseMaker();
	    maker.setDist(dist.val());
	    maker.setMagnitude(magnitude.val());
	    maker.setAddMethod(addMethod.val());
	    maker.setRandomSeed(randomSeed.val());

	    int nseg = noptims.val();
	    int nmatch = refData.length;
	    noisyData = new Data[nseg][nmatch];
	    for (int i=0; i<nseg; i++) {
	    	for (int j=0; j<nmatch; j++) {
		    Data base = refData[j];
		    Data noisy = maker.makeNoisy(base);
		    noisy.setName(noisy.name() + "_" + (i+1));
		    noisyData[i][j] = noisy;
		}
	    }
	}

	// simple query
	public String diagInfo() { return "ModelMonte " + name(); }
	public String xmlLabel() { return "montecarlo"; }
	public OptimArgs optimArgs() { 
	    if (jobInfo == null) return null;
	    return jobInfo.optim.args;
	}
	public Hashtable<String, String> args() { return args; }
	public Data[][] getNoisyData() { return noisyData; }

	// control changed: update parset
/*	public void childChanged(PNamed c) throws Xcept {
	    super.childChanged(c);
	    if (! isEmpty() || pmodel().isEmptyControl(c))
	    	pmodel().setParsModified(true);
	}
*/
}

