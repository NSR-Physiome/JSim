/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// static server status info

package JSim.rclient;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 

public class RCInfo implements Serializable {

	// static constants
	public final static int DEFAULT_PORT = 1099;

	// Client login request
	public static class Login implements Serializable {
	    public String login; // requested login ID
	    public String password; // requested login password

	    // info from client system
	    public String version;  // JSim version
	    public byte[] hostAddr; // host address
	    public String hostName; // host name
	    public String userName; // user.name property
	    public String osArch;   // os.arch property
	    public String osName;   // os.name property
	    public String osVersion;   // os.version property
	}

	// Client connection info
	public static class Connect implements Serializable {
	    public String id;	// client ID
	    public int port; // client port# 
	    public OptimAlg.Info[] optimAlgsInfo; // optimizers
	    public long pingFreq; // client must ping this
		// often (msec) to avert server disconnect
	}

	// Model info
	public static class Model implements Serializable {
	    public String id;			// model ID
	    public ModelVar[] vars;		// vars info
	    public UnitNList.Info units;	// unit list
	    public ASModel.Flags flags;	// unit/solver flags

	    // constructor 
	    public Model(String idx, ASModel rt) throws Xcept {
		id = idx;
		ASVar.List asvars = rt.getASVars();
		vars = new RCInfo.ModelVar[asvars.size()];
		for (int i=0; i<vars.length; i++) 
		    vars[i] = new ModelVar(asvars.asvar(i));
		if (rt.units() != null) 
		    units = new UnitNList.Info(rt.units());
		flags = rt.getFlags();
	    }

	    public String toString() {
		StringBuffer s = new StringBuffer();
		s.append("Model " + id + ":\n");
		for (int i=0; i<vars.length; i++) 
		    s.append("\t" + vars[i] + "\n");
		return s.toString();
	    }
	}

	// Model variable info
	public static class ModelVar implements Serializable {
	    public String name;		// variable name
	    public int dataType;	// dataType
	    public boolean isInput;	// input?
	    public boolean isDomain;	// domain?
	    public String[] domains;	// domains,  if NVar
	    public String defAssign;	// default assign, as String
	    public Unit.Info unit;	// unit
	    public String[] labels;	// choice labels or null
	    public String[] labelValues; // choice values or null

	    // constructor
	    public ModelVar(ASVar asvar) throws Xcept {
		name = asvar.name();
		dataType = asvar.dataType();
		isInput = asvar.isInput();
		isDomain = asvar.isDomain();
		domains = new String[asvar.ndim()];
		for (int i=0; i<domains.length; i++)
		    domains[i] = asvar.domain(i).name();
		defAssign = asvar.getAssign(); // QDefault???
		Unit u = asvar.unit();
		if (u != null)
		    unit = new Unit.Info(u);
		labels = asvar.labels();
		labelValues = asvar.labelValues();
	    }

	    public String toString() {
		String stype = (dataType == Expr.REAL) ?
		    "real" : "int";
		if (isDomain) stype = stype + "Domain";
		String doms = "";
		if (!isDomain && domains != null) {
		    for (int i=0; i<domains.length; i++) {
			if (i>0) doms = doms + ",";
			doms = doms + domains[i];
		    }
		    doms = "(" + doms + ")";
		}
	        String sunit = "";
		if (unit != null) sunit = unit.name;
		return stype + " " + name + doms + " " + sunit;
	    }
	}

	// run-time query metadata
  	public static class Query implements Serializable {
	    public String unitString;
	    public int ndim;
	    public boolean isConst;
	    public double constRealVal;
	    public StringList domainNames;
	    
	    // constructor
	    public Query(ASQuery q) {
	        unitString = q.unitString();
		ndim = q.ndim();
		isConst = q.isConst();
		domainNames = q.getDomainNames();
		try {
		    constRealVal = q.constRealVal();
		} catch (Xcept e) {
		    constRealVal = Double.NaN;
		}
	    }
	}

	// build or run job status
	public static class JobStatus implements Serializable {
	    public String id;	// job ID
	    public String desc; // job description
	    public boolean done;  // is job done?
	    public int termStat; // termination status (below) 
	    public String termMessage; // termination message
	    public long startTime; // time job started
	    public long doneTime; // time job completed
	    public ASInfo.Status jstat; // remote job status
	    public Serializable jobInfo; // job-specific status info
	}

	// JobStatus.termStat definitions
	public static final int RUNNING = 0;
	public static final int NORMAL = 1; // normal completion
	public static final int XCEPT = 2; // term by Xcept
	public static final int NOMEMORY = 3; // term by OutOfMemoryError
	public static final int OTHER = 4; // term by Exception

	// input info for single model run
	public static class RunInput implements Serializable {
	    public NamedVal[] assigns; // assignments
	    public NamedVal[] runVals; // user overrides

	    // constructor
	    public RunInput(ASModel model, NamedVal.NList runVals) 
	    throws Xcept {
	        ASVar.List asvars = model.getASVars();
		NamedVal.List aslist = new NamedVal.List();
		for (int i=0; i<asvars.size(); i++) {
		    ASVar v = asvars.asvar(i);
		    if (! v.isInput()) continue;
		    String s = v.getAssign();
		    aslist.add(NamedVal.create(v.name(), s));
		}
		assigns = aslist.info();
	    	this.runVals = runVals.info();
	    }
	}

	// input info for model loops run
	public static class LoopsInput implements Serializable {
	    public RunInput runInput; // info for single run
	    public ASInfo.LoopsInfo loopsInfo; // info for loops parm
	    
	    // constructor
	    public LoopsInput(ASModel model, ASInfo.Loops loops) 
	    throws Xcept {
	    	runInput = new RunInput(model, loops.baseVals);
		loopsInfo = new ASInfo.LoopsInfo(loops);
	    }
	}

	// input info for model sensitivitiy run
	public static class SensInput implements Serializable {
	    public RunInput runInput; // info for single run
	    public ASInfo.SensInfo sens; // info for sens parms/deltas
	    
	    // constructor
	    public SensInput(ASModel model, ASInfo.Sens s) 
	    throws Xcept {
	    	runInput = new RunInput(model, s.baseVals);
		sens = new ASInfo.SensInfo(s);
	    }
	}
	

	// input info for model optimization run
	public static class OptimInput implements Serializable {
	    public RunInput runInput; // info for best single run
	    public ASInfo.OptimInfo optimInfo; // info for optimiz
	    
	    // constructor
	    public OptimInput(ASModel model, ASInfo.Optim optim) 
	    throws Xcept {
	    	runInput = new RunInput(model, optim.baseVals);
		optimInfo = new ASInfo.OptimInfo(optim);
	    }
	}

	// input info for model multiple-optimization run
	public static class MoptInput implements Serializable {
	    public RunInput runInput; // info for best single run
	    public ASInfo.MoptInfo moptInfo; // info for mopt
	    
	    // constructor
	    public MoptInput(ASModel model, ASInfo.Mopt mopt) 
	    throws Xcept {
	    	runInput = new RunInput(model, mopt.optim.baseVals);
		moptInfo = new ASInfo.MoptInfo(mopt);
	    }
	}

	// info returned immediately from model run
	public static class RunOutput implements Serializable {
	    public int storeInx; // output for this store
	    public double[] finalVals; // final variable values

	    // constructors
	    public RunOutput(int inx, ASModel rtmodel) throws Xcept {
		storeInx = inx;
		ASVar.List asvars = rtmodel.getASVars();
		finalVals = new double[asvars.size()];
		for (int i=0; i<finalVals.length; i++) {
		    finalVals[i] = Double.NaN;
		    ASVar v = asvars.asvar(i);
		    try {
			finalVals[i] = v.finalRealVal();
		    } catch (Xcept e) {
			// nothing to do
		    }
		}
	    }
	}

	// data cache update, sent for live update var/exprs
	public static class CacheDelta implements Serializable {
	    public String expr;
	    public int storeInx;
	    public DataInfo data;
	    public String toString() {
	    	return "delta " + expr + "#" + storeInx + 
		    " subset=" + data.subset;
	    }
	}

	// info returned from model optimization
	public static class OptimOutput implements Serializable {
	    public RunOutput runOutput; // optimized run values
	    public OptimResults.Info orinfo;  // optimizer info

	}

	// info returned from model multi-optimization
	public static class MoptOutput implements Serializable {
	    public RunOutput runOutput; // single run output
	    public MoptData.Info morinfo;  // Mopt data info
	}

}

