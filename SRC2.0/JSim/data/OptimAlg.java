/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// optimizer algorithm

package JSim.data;

import java.io.Serializable;
import JSim.util.*;

public class OptimAlg implements Named {
	private Info info;

	// listing constants
	public static final int ALL = 0;
	public static final int BOUND_FZERO = 1;
	public static final int UNBOUND_FZERO = 2;
	public static final int FZERO = 3;
	
	// constructor
	public OptimAlg(Info info) {
	    this.info = info;
	}
	
	// query
	public String name() { return info.name; }
	public String toString() { return name(); }
	public String diagInfo() { return "OptimAlg " + name(); }
	public boolean boundsNeeded() { return info.boundsNeeded; }
	public boolean sensMatNeeded() { return info.sensMatNeeded; }
	public String optimClassName() { return info.optimClassName; }
	public int nparsNeeded() { 
	    return (info.parsNeeded == null) ? 0 : info.parsNeeded.length;
	}
	public String parNeeded(int i) {
	    if (i<0 || i>=nparsNeeded()) return null;
	    return info.parsNeeded[i];
	}
	public boolean parNeeded(String n) {
	    for (int i=0; i<nparsNeeded(); i++)
	    	if (n.equals(info.parsNeeded[i])) return true;
	    return false;
	}
	
	// OptimAlg.Info class
	public static class Info implements Serializable {
	    public String name;
	    public boolean boundsNeeded;
	    public boolean sensMatNeeded;
	    public String[] parsNeeded;
	    public String optimClassName;
	}

	// OptimAlg.NList class
	public static class NList extends NamedList {
	    public NList() { super(); }
	    public NList(Info[] infos) {
	    	super();
		for (int i=0; i<infos.length; i++)
		    add(new OptimAlg(infos[i]));
	    }
	    public OptimAlg alg(String n) { 
	    	return (OptimAlg) getByName(n);
	    }
	    public OptimAlg alg(int i) {
	    	return (OptimAlg) get(i);
	    }
	    
	    // names list
	    public StringList getNames(int which) {
	    	StringList list = new StringList();
		for (int i=0; i<size(); i++) {
		    OptimAlg alg = alg(i);
		    boolean add = false;
		    switch (which) {
		    case ALL: 
		    	add = true; 
			break;
		    case BOUND_FZERO: 
		    	add = !alg.sensMatNeeded() && alg.boundsNeeded(); 
			break;
		    case UNBOUND_FZERO: 
		    	add = !alg.sensMatNeeded() && !alg.boundsNeeded(); 
			break;
		    case FZERO: 
		    	add = !alg.sensMatNeeded(); 
			break;
		    }
		    if (add) list.add(alg.name());
		}
		return list;
	    }
	}
}
