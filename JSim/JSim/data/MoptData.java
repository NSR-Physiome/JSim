/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// multiple optimization data

package JSim.data;

import JSim.util.*;

import java.io.Serializable;

public class MoptData implements DiagInfo {
	private int nsegments, npars, nmatches;
	private boolean[] done;
	private String[] parNames;
	private RealNData[] parData;
	private RealNData rmsData;
	private RealNData[][] fitData;
	private OptimResults[] optimResults;

	// constructor
	public MoptData(String[] parNames, int nsegments, int nmatches) 
	throws Xcept {
	    this.parNames = parNames;
	    this.npars = parNames.length;
	    this.nsegments = nsegments;
	    this.nmatches = nmatches;
	    done = new boolean[nsegments];
	    parData = new RealNData[npars];
	    fitData = new RealNData[nmatches][nsegments];
	    optimResults = new OptimResults[nsegments];
	    int ns = (nsegments == 1) ? 2 : nsegments;
	    GridData grid = new RegularGridData("segment", null,
	    	1, ns, ns);
	    for (int p=0; p<npars; p++) 
	    	parData[p] = new RealNData(parNames[p],
		    null, new GridData[] { grid });
	    rmsData = new RealNData("RMS_error", 
	    	null, new GridData[] { grid });
	}

	// set data
	public void setDone(int segment) throws Xcept {
	    done[segment] = true;
	}
	public void setParData(int par, int segment, double value)
	throws Xcept {
	    parData[par].set(segment, value);
	}
	public void setRMSData(int segment, double value) 
	throws Xcept {
	    rmsData.set(segment, value);
	}
	public void setFitData(int segment, int match, Data data)
	throws Xcept {
//OLD	    data.setGroup("segment_" + (segment+1));
	    fitData[match][segment] = (RealNData) data;
	}
	public void setOptimResults(int segment, OptimResults rold)
	throws Xcept {
	    OptimArgs args = new OptimArgs(rold.args);
	    OptimResults rnew = new OptimResults(args);
	    OptimResults.Info info = new OptimResults.Info(rold);
	    rnew.importInfo(info);
	    optimResults[segment] = rnew;
	}
	    
	// query
	public int nsegments() { return nsegments; }
	public int npars() { return npars; }
	public int nmatches() { return nmatches; }
	public String[] parNames() { return parNames; }
	public int nsegmentsDone() { 
	    int ct = 0;
	    for (int i=0; i<nsegments; i++)
	    	if (done[i]) ct++;
	    return ct;
	}
	public boolean segmentDone(int segment) {
	    if (segment >= nsegments) return false;
	    return done[segment];
	}
	public boolean[] segmentsDone() { return done; }
	public Data.List parData() { 
	    return new Data.List(parData); 
	}
	public Data rmsData() { return rmsData; }
	public Data.List fitData() { 
	    Data.List list = new Data.List(nmatches*nsegments);
	    for (int s=0; s<nsegments; s++) 
	        for (int m=0; m<nmatches; m++)
		    list.add(fitData(s, m));
	    return list; 
	}
	public RealNData parData(int par) {
	    if (par >= npars()) return null;
	    return parData[par];
	}
	public RealNData parData(String name) throws Xcept {
	    for (int i=0; i<npars(); i++)
	    	if (parNames[i].equals(name))
		    return parData[i];
	    throw new Xcept(this, "No such data: " + name);
	}
	public RealNData fitData(int segment, int match) {
	    if (segment >= nsegments() || match >= nmatches)
	        return null;
	    return fitData[match][segment];
	}
	public OptimResults optimResults(int segment) {
	    if (segment >= nsegments()) return null;
	    return optimResults[segment];
	}
	public String diagInfo() {
	    return "MoptData " + new StringList(parNames);
	}

	// import serializable info
	public void importInfo(Info info) throws Xcept {
	    done = info.done;
	    for (int p=0; p<npars; p++) 
		if (info.parData[p] != null)
		    parData[p] = (RealNData) Data.makeData(info.parData[p]);
	    rmsData = (RealNData) Data.makeData(info.rmsData);
	    for (int m=0; m<nmatches; m++) {
		for (int s=0; s<nsegments; s++) {
		    DataInfo dinfo = info.fitData[m][s];
		    if (dinfo == null) continue;
		    fitData[m][s] = (RealNData) Data.makeData(dinfo);
		}
	    }
	    for (int s=0; s<nsegments; s++) {
		OptimResults.Info rinfo = info.optimResults[s];
		if (rinfo == null) continue;
		optimResults[s] = new OptimResults(rinfo);
	    }
	}

	// serializable info (complete or partial)
	public static class Info implements Serializable {
	    private int nsegments, npars, nmatches;
	    private boolean[] done;
	    private String[] parNames;
	    private DataInfo[] parData;
	    private DataInfo rmsData;
	    private DataInfo[][] fitData;
	    private OptimResults.Info[] optimResults;

	    // constructors
	    public Info(MoptData mopt) throws Xcept {
		this(mopt, null);
	    }
	    public Info(MoptData mopt, boolean[] prevDone) 
	    throws Xcept {
		nsegments = mopt.nsegments;
		npars = mopt.npars;
		nmatches = mopt.nmatches;
		done = mopt.done;
		parNames = mopt.parNames;
		parData = new DataInfo[npars];
		if (prevDone == null)
		    prevDone = new boolean[nsegments];
		for (int p=0; p<npars; p++) 
		    if (mopt.parData[p] != null)
			parData[p] = mopt.parData[p].info();
		if (mopt.rmsData != null)
		    rmsData = mopt.rmsData.info();
		fitData = new DataInfo[nmatches][nsegments];
		for (int s=0; s<nsegments; s++) {
		    if (prevDone[s]) continue;
		    for (int m=0; m<nmatches; m++) {
		    	Data data = mopt.fitData[m][s];
			if (data == null) continue;
			fitData[m][s] = data.info();
		    }
		}
		optimResults = new OptimResults.Info[nsegments];
		for (int s=0; s<nsegments; s++) {
		    if (prevDone[s]) continue;
		    OptimResults r = mopt.optimResults[s];
		    if (r == null) continue;			
		    optimResults[s] = new OptimResults.Info(r);
		}
	    }

	    // create MoptData
	    public MoptData mopt() throws Xcept {
	    	MoptData mopt = new MoptData(parNames, nsegments, nmatches);
		mopt.importInfo(this);
		return mopt;
	    }

	    // query
	    public int nresults() {
		if (optimResults == null) return 0;
	    	int ct = 0;
		for (int i=0; i<optimResults.length; i++)
		    if (optimResults[i] != null)
		    	ct++;
		return ct;
	    }
	}
	
}

