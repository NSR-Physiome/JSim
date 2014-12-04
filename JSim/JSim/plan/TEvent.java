/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// event

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class TEvent implements SeqItem {
	public TModel model;
	public Domain t;  // time domain
	public TExpr trigger; // boolean expr
	public ArrayList<TAction> actions; // eqns used as basis for tool
	public VarUsages vacts; // vars modified by actions
	public VarUsages vreqs;  // vu's needed for sequencing
	public DomainSet seqLoops; // sequence/calculation loops
	public String nodeString; // for SeqNode
	
	// constructor
	public TEvent(TModel model, Event e) throws Xcept {
	    this.model = model;
	    t = model.domain(e.t());
	    trigger = new TExpr(model, e.trigger());
	    actions = new ArrayList<TAction>();
	    vacts = new VarUsages(model);
	    vreqs = new VarUsages(model);
	    vreqs.add(trigger.usages());
	    for (int i=0; i<e.nactions(); i++) {
	    	TAction act = new TAction(model, e.v(i), e.vexpr(i));
		actions.add(act);
		Var vact = act.v;
		vacts.add(vact);
		vreqs.add(vact);
		vreqs.add(act.vexpr.usages());
		DomainSet vdoms = model.domSets.get(vact);
		if (i == 0) 
		    seqLoops = vdoms;
		else if (! seqLoops.equals(vdoms))
		    throw new AbortXcept(
		    "Action vars " + vacts.get(0) +
		    " and " + vact + 
		    " have incompatible domains for event: " + e);
	    }
	    nodeString = e.id() + vacts;
	    if (vacts.size() == 0) throw new Xcept(
	    	"Event contains no actions: " + e);
	}

	// query
	public Logger logger() { return model.logger(); }
	public DomainSet seqLoops() { return seqLoops; }
	public Domain t() { return t; }
	public VarUsages vreqs() { return vreqs; } 
	public String toString() {
	    return "event(" + trigger + ") " + actions;
	}
	public String nodeString() { return nodeString; }

	// TAction subclass
	public static class TAction {
	    public TModel model;
	    public Var v; // variable afftected
	    public TExpr vexpr; // calculation for v
	    public TAction(TModel model, Var mmlVar, Expr expr) 
	    throws Xcept {
	    	this.model = model;
		v = model.var(mmlVar);
		vexpr = new TExpr(model, expr);
	    }
	    public String toString() { 
	        return v.toString() + ":=" + vexpr;
	    }
	}
}
