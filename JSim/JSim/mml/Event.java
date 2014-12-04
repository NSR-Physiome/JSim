/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MML event

package JSim.mml; 

import JSim.util.*;
import JSim.expr.*;
import JSim.aserver.*;

import java.util.ArrayList;

public class Event implements DiagInfo {
	private Comp parent;	// parent event
	private Expr trigger; 	// event triggered
	private Domain t;	// progress domain
	private Var.List vstate; // state variables
	private Var.List vaction; // vars for each action
	private Expr.List vexpr; // exprs for each action
	private String id; // for XMML, SeqGraph's
	
	// constructor
	public Event(Comp p, Expr trig) throws Xcept {
	    parent = p;
	    trigger = trig.simplify();
	    if (trigger.dataType() != Expr.BOOLEAN) 
		throw new Xcept(this,
		    "event trigger must be boolean expression");
	    vstate = new Var.List(4);
	    vaction = new Var.List(4);
	    vexpr = new Expr.List(4);
	    Domain.List doms = new Domain.List(4);
	    trigger.addDomains(doms);
	    if (doms.size() > 1) throw new Xcept(
	    	"Event trigger (" + trigger + ") may not be multi-dimensional");
	    t = doms.domain(0);
	    parent.registerEvent(this);
	    id = "event-" + parent.events.size();
	}

	// add eqn to event
	public void addEqn(Expr eqn) throws Xcept {

	    // parse eqn
	    CompareExpr ce = (eqn instanceof CompareExpr) ?
		((CompareExpr) eqn) : null;
	    if (ce == null || ce.op() != ce.EQ) 
		throw new Xcept(eqn, 
  		   "event action must be of form variable = expression");
	    Var v = (ce.arg(0) instanceof Var) ?
		((Var) ce.arg(0)) : null;
	    if (v == null)
		throw new Xcept(eqn, "event action LHS missing");
	    
	    // check domains
	    Domain.List vdoms = new Domain.List(v.ndim());
	    v.addDomains(vdoms);
	    Domain.List edoms = new Domain.List(v.ndim());
	    ce.arg(1).addDomains(edoms);
	    if (! vdoms.containSet(edoms)) throw new Xcept(eqn, 
		"event action RHS has domains missing from LHS");

	    // update internal lists
	    vstate.addUniq(v);
	    vaction.add(v);
	    vexpr.add(ce.arg(1));
	}

	// expand derivs in Event - in place
	// don't simplify because bolyxs unit correction for RealConst
	protected void expandDeriv() throws Xcept {
	    trigger = trigger.expandDeriv();
	    Expr.List nexpr = new Expr.List(vexpr.size());
	    for (int i=0; i<nactions(); i++) {
		Var v = v(i);
		Expr rhs = vexpr(i).expandDeriv();
	        nexpr.add(rhs);
	    }
	    vexpr = nexpr;
	}

	// unit correction - in place
	public void unitCorrect() throws Xcept {
	    trigger = trigger.unitCorrect();
	    Expr.List nexpr = new Expr.List(vexpr.size());
	    for (int i=0; i<nactions(); i++) {
		Var v = v(i);
		Expr rhs = vexpr(i).unitCorrect();
		Unit lu = v.unit();
		Unit ru = rhs.unit();
	    	if (! Unit.compatible(lu, ru)) throw new UnitXcept(
		    (IExpr) v.eq(rhs), v, rhs, lu, ru);
	    	rhs = rhs.multUnit(lu, ru).simplify();
		nexpr.add(rhs);		
	    }
	    vexpr = nexpr;
	}

	// simplify in place
	public void simplify() throws Xcept {
	    trigger = trigger.simplify();
	    Expr.List nexpr = new Expr.List(vexpr.size());
	    for (int i=0; i<nactions(); i++) {
		Var v = v(i);
		Expr rhs = vexpr(i).simplify();
	        nexpr.add(rhs);
	    }
	    vexpr = nexpr;
	}    

	// query
	public String toString() { 
	    StringBuffer buf = new StringBuffer(
		"if (" + trigger + ") {");
	    for (int i=0; i<nactions(); i++) 
		buf.append(" " + v(i) + "=" + vexpr(i));
	    return buf.toString() + " }";
	}
	public Var.List vstate() { return vstate; }
	public Expr trigger() { return trigger; }
	public int nactions() { return vaction.size(); }
	public Var v(int i) { return vaction.var(i); }
	public Expr vexpr(int i) { return vexpr.expr(i); }
	public String diagInfo() { return toString(); }
	public Domain t() { return t; }
	public String id() { return id; }

	// Event.List
	public static class List extends ArrayList<Event> {
	    public List(int n) { super(n); }
	    public Event event(int i) { 
		return (Event) get(i);
	    }
	    public Var.List vstate() {
		Var.List list = new Var.List(4);
		for (int i=0; i<size(); i++) {
		    Event e = event(i);
		    list.addUniq(e.vstate());
		}
		return list;
	    }
	}
}

