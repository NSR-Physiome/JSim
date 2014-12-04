/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1 server side model

package JSim.rserver;

import java.io.*;
import java.util.ArrayList;
import java.rmi.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.rclient.*;

public class RSModel implements Named {
	private RSClient rclient; // for this client
	private RSInfo.Model info; // static info
	private ASModel rt; // model runtime
	private RSDataStore[] stores; // data stores by inx

	// constructor
	public RSModel(RSClient r, ASModel asmodel, String id) {
	    rclient = r;
	    rt = asmodel;
	    info = new RSInfo.Model();
	    info.id = id;
	    info.buildTime = System.currentTimeMillis();
	    clearStores(0);
	}		

	// clear old stores, create n new ones
	protected void clearStores(int n) {
	    stores = new RSDataStore[n];
	    for (int i=0; i<n; i++)
	        stores[i] = new RSDataStore(this, i);
	}

	// get model build  warnings
	public String[] getBuildAlerts() throws Xcept {
	    request();
	    return rt.getBuildAlerts();
	}

	// get model text
	public String getText(int type, String variant) throws Xcept {
	    request();
	    return rt.getText(type, variant);
	}

	// get model text warnings
	public String[] getTextWarnings(int type, String variant) throws Xcept {
	    request();
	    return rt.getTextWarnings(type, variant);
	}

	// set func gen names
	public void setFuncGenNames(String[] names) throws Xcept {
	    request();
	    rt.setFuncGenNames(names);
	}

	// set assign
	public void setAssign(String vname, String expr) throws Xcept {
	    request();
	    ASVar v = getASVar(vname);
	    v.setAssign(expr);
	}

	// parse query
	public RCInfo.Query parseQuery(String s) throws Xcept {
	    request();
	    ASQuery q = rt.parseQuery(s);
	    return new RCInfo.Query(q);
	}

	// get model data
	public DataInfo getData(int i, String s) throws Xcept {
	    request();
	    ASQuery query = rt.parseQuery(s);
	    Data data = rt.getData(i, query);
	    if (data == null) return null;
	    if (data.allNaN()) return null;
	    if (data.subset != null && i<stores.length) 
		stores[i].add(query, data);
	    return data.info();
	}

	// get profile
	public ProfileData getProfile() throws Xcept {
	    request();
	    return rt.getProfile();
	}

	// update request time
	public void request() { 
	    info.requestTime = System.currentTimeMillis();
	}

	// run completed
	public void runDone() { 
	    info.runTime = System.currentTimeMillis();
	}	

	// load RunInput info
	protected void loadRunInput(RCInfo.RunInput rinfo)
	throws Xcept {
	    for (int i=0; i<rinfo.assigns.length; i++) {
	        NamedVal assign = rinfo.assigns[i];
	    	ASVar v = getASVar(assign.name());
		v.setAssign(assign.stringVal());
	    }
	}
	
	// get deltas for stores
	public RCInfo.CacheDelta[] getDeltas() {
	    ArrayList<RCInfo.CacheDelta> list = 
	    	new ArrayList<RCInfo.CacheDelta>();
	    for (int i=0; i<stores.length; i++) 
	    	stores[i].addDeltas(list);
	    if (list.size() == 0) return null;
	    RCInfo.CacheDelta[] deltas = 
	    	new RCInfo.CacheDelta[list.size()];
	    for (int i=0; i<list.size(); i++)
	    	deltas[i] = (RCInfo.CacheDelta) list.get(i);
	    return deltas;
	} 

	// log
	protected void log(String msg, Exception e) { 
	    rclient.log(msg, e); 
	}

	// query
	public String id() { return info.id; }
	public String name() { return id(); }
	public String diagInfo() { return "RSModel " + id(); }
	public ASModel rt() { return rt; }
	public ASVar getASVar(String n) throws Xcept { 
	    return rt.getASVar(n);
	}

	// get info
	public RSInfo.Model getInfo() { return info; }

	// named list of Models
	public static class NList extends NamedList {
	    public NList(int n) { super(n); }
	    public RSModel model(int n) { return (RSModel) get(n); }
 	    public RSModel model(String n) { return (RSModel) getByName(n); }
	}
}

