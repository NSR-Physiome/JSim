/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one data store for an RSModel

package JSim.rserver;

import java.util.ArrayList;

import JSim.aserver.*;
import JSim.util.*;
import JSim.data.*;
import JSim.rclient.*;

public class RSDataStore extends NamedList {
	private RSModel rsmodel;
	private int storeInx;
	private NamedList entries;

	// constructor for one cache item
	public RSDataStore(RSModel m, int i) {
	    rsmodel = m;
	    storeInx = i;
	    entries = new NamedList();
	}

	// add item to cache
	public void add(ASQuery query, Data data) {
	    String name = query.toString();
	    if (entries.getByName(name) != null) return;
	    entries.add(new Entry(query, data));
	}
	    
	// delta info
	public RCInfo.CacheDelta[] deltas() {
	    RCInfo.CacheDelta[] deltas = 
		new RCInfo.CacheDelta[entries.size()];
	    for (int i=0; i<deltas.length; i++) 
		deltas[i] = entry(i).delta();
	    return deltas;
	}

	// add deltas to ArrayList
	public void addDeltas(ArrayList<RCInfo.CacheDelta> list) {
	    for (int i=0; i<entries.size(); i++) {
	        RCInfo.CacheDelta delta = entry(i).delta();
		if (delta != null) list.add(delta);
	    }
	}

	// query
	public Entry entry(int i) { 
	    return (Entry) entries.get(i);
	}

	// cache entry for one var/expr
	public class Entry implements Named {
	    private ASQuery query;
	    private Data data;
	    private Data.Subset lastSubset;
	    private boolean done;

	    // constructor
	    public Entry(ASQuery q, Data d) {
		query = q;
		data = d;
		if (data.subset != null)
		    lastSubset = new Data.Subset(data.subset);
	    }

	    // query
	    public String name() { return query.toString(); }
	    public String diagInfo() { 
		return "RSDataStore entry " + name();
	    }

	    // serializable delta
	    public RCInfo.CacheDelta delta() {
	        if (done) return null;
		RCInfo.CacheDelta delta = new RCInfo.CacheDelta();
		delta.storeInx = storeInx;
		delta.expr = name();
		try {
		    data = rsmodel.rt().getData(storeInx, query);
		    if (data == null || data.subset == null) {
			lastSubset = null;
		    } else if (lastSubset == null) {
			lastSubset = new Data.Subset(data.subset);
		    } else {
			lastSubset.lox = lastSubset.hix;
			lastSubset.hix = data.subset.hix;
		    }

		    delta.data = (data == null) ?
		    	null : data.info(lastSubset);
		    done = data != null && data.subset == null;
		} catch (Xcept e) {
		    rsmodel.log("delta: ", e);
		}

		return delta;
	    }
	}
}

