/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim functional image generator (used by jsfim/QPP system)

package JSim.project;

import java.io.*;
import java.util.StringTokenizer;
import java.net.URL;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 

public class PModelFim implements DiagInfo {
	private Project proj;
	private PModel pmodel;
	private ASInfo.Optim optimInfo;
	private PDataSet[] matchDataSets;
	private Data.List[] matchData;
	private int nsegments;
	private int nmatches;

	// constructor
	public PModelFim(Project p) throws Xcept {
	    proj = p;
	}
	
	// select single model within project
	public void selectModel() throws Xcept {
	    PNamed.List pmodels = proj.children(PModel.class);
	    if (pmodels.size() != 1) throw new Xcept(this,
	    	"Project must contain exactly one model, contains " +
		pmodels.size());
	    pmodel = pmodels.pmodel(0);
	    compileModel();
	}	

	// select model by name
	public void selectModel(String name) throws Xcept {
	    PNamed n = proj.child(name);
	    if (! (n instanceof PModel)) throw new Xcept(this,
	    	"Model " + n + " not found in project");
	    pmodel = (PModel) n;
	    compileModel();
	}
	
	// compile pmodel, if needed
	private void compileModel() throws Xcept {
	    if (pmodel.rt().isBuilt()) return;
	    Util.verbose("Compiling model " + pmodel.name() + " ...");
	    boolean verbose = Util.verbose;
	    Util.verbose = false;
	    PJob pjob = new PModelBuildJob(pmodel);
	    pjob.setStackTrace(true);
	    pjob.simpleRun();
	    fudgeMatchDataSet();
	    proj.revalidate();
	    clearMatchData();
	} 

	// assign optim data sets if unspecified
	private void fudgeMatchDataSet() throws Xcept {
	    PNamed.List dataSets = proj.children(PDataSet.class);
	    if (dataSets.size() != 1) return;
	    String dsname = dataSets.pnamed(0).name();
	    PModelOptim optim = pmodel.optim();
	    for (int i=0; i<optim.nmatches.val(); i++) {
	    	PModelOptim.Match match = optim.match(i);
		if (! match.valid()) continue;
		if (Util.isBlank(match.dataSrc.val()))
		    match.dataSrc.setVal(dsname);
	    }
	}

	// clear match data
	private void clearMatchData() throws Xcept {
	    optimInfo = pmodel.optim().makeJobInfo();
	    nmatches = optimInfo.nmatches();
	    matchDataSets = new PDataSet[nmatches];
	    matchData = new Data.List[nmatches];
	    for (int i=0; i<nmatches; i++) 
	    	matchData[i] = new Data.List(20);
	}

	// assign fgen to data curve
	public void setFuncGen(String varName,
	PDataSet dataSet, String curveName) throws Xcept {
	    modelCheck();
	    Control vcntl = pmodel.vars().control(varName);
	    if (! (vcntl instanceof AssignControl)) throw new Xcept(this,
	    	"-fgen variable " + varName + " not an input variable");


	    // find/create func gen
	    FuncGen fgen = null;
	    PNamed p = pmodel.vars().child(vcntl.stringVal());
	    if (p instanceof FuncGen) {
		fgen = (FuncGen) p;
	    } else {	    
	    	String fgenName = pmodel.vars().newChildName(
		    "fgen", true);
	    	fgen = new FuncGen(pmodel.vars(), fgenName);
		pmodel.vars().updateFuncGenNames();
		vcntl.setVal(fgenName);
	    }
	    Util.verbose("Assigning variable " + vcntl.name() + 
	    	" to FuncGen " + fgen.name() + 
		" using data curve " + curveName);

	    // find data for curveName
	    Data data = dataSet.data(curveName);
	    if (data == null) data = dataSet.dataForDesc(curveName);
	    if (data == null) throw new Xcept(dataSet,
	    	"Can't find fgen data curve \"" + curveName 
		+ "\" in data set");

	    // set fgen to selected data curve
	    fgen.which.setVal("DataCurve");
	    Control cntl = (Control) 
	    	fgen.nestedChild("DataCurve.dataSet");
	    cntl.setVal(dataSet.name());
	    cntl = (Control) fgen.nestedChild("DataCurve.name");
	    cntl.setVal(data.name());
	    proj.revalidate();
	} 

	// set match data - QPP legacy form (no excludes)
	public void setMatchData(int inx, PDataSet dataSet,
	String pfx) throws Xcept {
	    setMatchData(inx, dataSet, pfx, null);
	}

	// set match data
	public void setMatchData(int inx, PDataSet dataSet,
	String pfx, StringList excludes) throws Xcept {
	    modelCheck();
	    if (inx >= matchData.length) throw new Xcept(this,
	    	"setMatchData index out of bounds");
	    matchDataSets[inx] = dataSet;
	    Data.List list = dataSet.dataList();
	    for (int i=0; i<list.size(); i++) {
	    	Data data = list.data(i);
		boolean add = (pfx == null);
		if (!add && data.name().startsWith(pfx))
		    add = true;
		if (!add && data.desc() != null 
		&& data.desc().startsWith(pfx))
		    add = true;
		if (excludes != null && excludes.contains(data.name()))
		    add = false;
		if (excludes != null && excludes.contains(data.desc()))
		    add = false;
		if (add) matchData[inx].add(data);
	    }
	    if (matchData[inx].size() == 0) throw new Xcept(dataSet,
	    	"Specified match data set is empty");
	}

	// create Job
	public PModelMoptJob createJob() throws Xcept {
	    return new PModelMoptJob(pmodel(), makeJobInfo());
	}

	// create job info
	public ASInfo.Mopt makeJobInfo() throws Xcept {
	    modelCheck();
	    matchCheck();
	    ASInfo.Mopt jobInfo = new ASInfo.Mopt();
	    jobInfo.optim = pmodel.optim().makeJobInfo();
	    jobInfo.nvals = null; // not yet implemented
	    Data.List[] refData = matchData();
	    int nsegments = refData[0].size();
	    int nmatches = refData.length;
	    jobInfo.refData = new Data[nsegments][nmatches];
	    for (int s=0; s<nsegments; s++) 
	    	for (int m=0; m<nmatches; m++) 
		    jobInfo.refData[s][m] = refData[m].data(s);
	    jobInfo.saveOptimResults = true;
	    jobInfo.noabort = false;
	    return jobInfo;
	}

	// check model is set
	protected void modelCheck() throws Xcept {
	    if (proj == null) throw new Xcept(this,
	    	"No project specified");
	}

	// check match data is set & coherent
	protected void matchCheck() throws Xcept {
	    if (matchData == null) throw new Xcept(this,
	    	"No match data specified");
	    nsegments = matchData[0].size();
	    for (int i=1; i<nmatches; i++) 
	    	if (matchData[i].size() != nsegments)
		    throw new Xcept(this,
		    	"Match data is inconsistent");
	}

	// max match data "time" (1st grid)
	public double matchMaxTime() throws Xcept {
	    matchCheck();
	    double tmax = 0;
	    for (int s=0; s<nsegments; s++) {
	    	for (int m=0; m<nmatches; m++) {
		    Data data = matchData(s, m);
		    if (data.ndim() != 1) throw new Xcept(data,
		    	"matchMaxTime() match data not 1D");	    
		    GridData grid = data.grid(0);
		    tmax = Math.max(tmax, grid.max());
		}
	    }    
	    return tmax;
	}

	// simple query
	public String diagInfo() { return "FuncImage " + proj; }
	public Project project() { return proj; }
	public PModel pmodel() { return pmodel; }
	public MoptData moptData() throws Xcept { 
	    return pmodel.rt().getMoptData(); 
	}
	public Data matchData(int segment, int match)
	throws Xcept {
	    return matchData[match].data(segment);
	}
	protected Data.List[] matchData() { return matchData; }
	
	//// I4 Bullseye methods

	// check if I4 compatible
	public void checkI4Bull() throws Xcept {
	    modelCheck();
	    matchCheck();
	    if (intVal("z.ct") < 2) throw new Xcept(
	    	matchDataSets[0], "z.ct must be >= 2");
	    intVal("z.offset");
	    if (intVal("s.ct") < 1) throw new Xcept(
	    	matchDataSets[0], "s.ct must be >= 2");
	    for (int i=0; i<nsegments; i++) 
	    	if (i4ring(i) < 0 || i4sector(i) < 0) 
		    throw new Xcept(matchDataSets[0],
		    	"Curve " + i4name(i) + " not I4Bull compatible");
	}

	// get integer parm value
	private int intVal(String name) throws Xcept {
	    Data data = matchDataSets[0].data(name);
	    if (data == null) throw new Xcept(matchDataSets[0],
	    	"Parameter " + name + " not found");
	    if (data.ndim() != 0) throw new Xcept(matchDataSets[0],
	    	"Parameter " + name + " is not constant");
	    return (int) Math.round(data.realVal(0));
	}

	// i4name (rs#_#) for segment, null if not defined
	private String i4name(int seg) throws Xcept {
	    matchCheck();
	    String s = i4name(seg, 0);
	    if (s == null) return null;
	    for (int m=1; m<nmatches; m++) {
		String s1 = i4name(seg, m);
		if (s1 == null) return null;
		if (! s.equals(s1)) return null;
	    }
	    return s;
	}	        
	private String i4name(int seg, int match) {
	    Data data = matchData[match].data(seg);
	    String s = data.name();
	    if (! s.startsWith("rs")) s = data.desc();
	    if (! s.startsWith("rs")) s = null;
	    return s;
	}

	// I4 ring/sector for segment, -1 if not defined
	private int i4ring(int seg) throws Xcept {
	    String s = i4name(seg);
	    if (s == null) return -1;
	    int inx = s.indexOf('_');
	    try {
	    	return Integer.parseInt(s.substring(2,inx))-1;
	    } catch (Exception e) {
	    	return -1;
	    }
	}
	private int i4sector(int seg) throws Xcept {
	    String s = i4name(seg);
	    if (s == null) return -1;
	    int inx = s.indexOf('_');
	    try {
	    	return Integer.parseInt(s.substring(inx+1))-1;
	    } catch (Exception e) {
	    	return -1;
	    }
	}

	// create I4 fit dataset (fit curves labeled rs#_#)
	public Data.List createI4FitData() throws Xcept {
	    checkI4Bull();
	    Data.List list = moptData().fitData();
	    for (int i=0; i<list.size(); i++) {
		Data data = list.data(i);
	    	int s = i % nsegments;
		String name=i4name(s);
		if (data != null)
		    data.setName(name);
	    }
	    return list;
	}

	// create I4 bullseye dataset
	public Data.List createI4BullData() throws Xcept {
	    checkI4Bull();
	    int nrings = intVal("z.ct");
	    int nsectors = intVal("s.ct");
	    int zoffset = intVal("z.offset");
	    GridData sgrid = new RegularGridData(
	    	"s", null, 1, nsectors,	nsectors);
	    GridData zgrid = new RegularGridData(
	    	"z", null, zoffset+1, zoffset+nrings, nrings);
	    GridData[] grids = new GridData[] { sgrid, zgrid };
	    MoptData moptData = moptData();
	    if (moptData == null) throw new Xcept(this,
	    	"Multiple-optim data not available");
	    int npars = moptData.npars();
	    Data.List list = new Data.List(npars);
	    for (int p=0; p<npars; p++) {
		String name = optimInfo.args.xname[p];
		Data data = new RealNData(name, null, grids);
		list.add(data);
	    }
	    int[] rs = new int[2];
	    for (int i=0; i<nsegments; i++) {
	    	rs[0] = i4sector(i);
		rs[1] = i4ring(i);
		for (int p=0; p<npars; p++) {
		    Data data = moptData.parData(p);
		    double value = data.realVal(i);
		    RealNData i4data = (RealNData) list.data(p);
		    i4data.set(rs, value);
		}
	    }	    
//	    list.add(matchDataSets[0].data("z.ct"));
//	    list.add(matchDataSets[0].data("z.offset"));
//	    list.add(matchDataSets[0].data("s.ct"));
	    return list;
	}
}
