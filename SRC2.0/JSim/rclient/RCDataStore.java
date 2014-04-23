/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one data-store for run-time model

package JSim.rclient;

import java.util.ArrayList;
import java.io.*;
import java.rmi.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 

public class RCDataStore extends NamedList {
	private RCModel model; // for this model
	private int storeInx; // index of this store
	private String storeName; // store name
	private double[] finalVals;   // final run vals

	// constructor
	public RCDataStore(RCModel m, int i, String n) {
	    model = m;
	    storeInx = i;
	    storeName = n;
	}

	// set final vals
	protected void setFinalVals(double[] vals) {
	    finalVals = vals;
	}

	// query
	public String name() { return storeName; }
	public CData cdata(String n) { return (CData) getByName(n); }

	// get query data from cache or remote model
	public Data getData(ASQuery query) throws Xcept {
	    String n = query.toString();
	    CData cdata = cdata(n);

	    // missing 0D data from vars0 
	    if (cdata == null 
	    && finalVals != null
	    && (query instanceof RCModelVar) 
	    && ((RCModelVar) query).ndim() == 0) {
		preLoad();
		cdata = cdata(n);
	    }

	    // query rclient if no local data
	    if (cdata == null) try {
		model.request();
	    	DataInfo info = model.rclient().getData(
		    model.id(), storeInx, n);
		if (info == null) return null;
	        Data data = Data.makeData(info);
		cdata = new CData(n, data);
		add(cdata);
	    } catch (RemoteException e) {
		cdata = new CData(n, e.getMessage());
		// cacheing Xcept causes problem with webmodel 0134
		// 2nd loop query gets Xcept 1st time, never recovers
		// add(cdata); 
	    }		

	    // return
	    if (cdata.errMsg != null) 
		throw new Xcept(cdata.errMsg);
	    return cdata.data;
	}

	// preload scalar data
	public void preLoad() throws Xcept {
	    if (finalVals == null) return;
	    ASVar.List asvars = model.getASVars();
	    for (int i=0; i<asvars.size(); i++) {
		RCModelVar v = (RCModelVar) asvars.asvar(i);
		if (v.ndim() != 0) continue;
		double val = finalVals[i];
		String n = v.name();
		if (storeName != null)
		    n = n + ": " + storeName;
		Data data = new RealNData(n, v.unit(), null,
       		    new double[] { val });
	    	CData cdata = new CData(n, data);
	    	add(cdata);
	    }	
	    finalVals = null;  // no longer needed
	}

	// update live cache delta
	public void updateDelta(RCInfo.CacheDelta delta) throws Xcept {
	    CData cdata = cdata(delta.expr);
	    if (cdata == null) return;
	    if (! (cdata.data instanceof RealNData)) return;
	    if (! (delta.data instanceof RealNData.Info)) return;
	    RealNData ndata = (RealNData) cdata.data;
	    RealNData.Info ninfo = (RealNData.Info) delta.data;
	    ndata.set(ninfo.subset, ninfo.samples);
	    if (ninfo.subset != null) {
		if (ndata.subset == null) {
		    ndata.subset = new Data.Subset(ninfo.subset);
		    ndata.subset.lox = 0; // hack
		}
		ndata.subset.hix = ninfo.subset.hix;
	    }
	}

	// CData class wraps data with better name
	public static class CData implements Named {
	    public String name;
	    public Data data;
	    public String errMsg;
	
	    // constructors
	    public CData(String n, Data d) {
		name = n;
		data = d;
	    }
	    public CData(String n, String s) {
		name = n;
		errMsg = s;
	    }

	    // query
	    public String name() { return name; }
	    public String diagInfo() { return "CData " + name; }
	}

}
