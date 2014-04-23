/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// live data query during model run

package JSim.jruntime;

import java.util.ArrayList;
import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.nml.*;

public class RTLiveQuery implements DiagInfo {
	private RTModel model; // for this model
	private Expr oexpr; // original expr to be queried
	private Expr uexpr; // unit-corrected expression
	private Expr.List domains; // domains in expr
	private int phase; // live update during this runPhase
	private RTRealDomain liveDomain; // which domain is live
	private Data data; // data cache
	private boolean complete; // data complete

	// constructor
	protected RTLiveQuery(RTModel m, Expr e) throws Xcept {
	    model = m;
	    oexpr = e;
	    uexpr = (model.unitCorrect) ? 
		oexpr.unitCorrect() : oexpr;
	    domains = new NamedExpr.List(2);
	    oexpr.addDomains(domains);
	    Expr.List vars = new NamedExpr.List(2);
	    oexpr.addNamedExpr(vars);
	    phase = 0;
	    for (int i=0; i<vars.size(); i++) {
		Expr v = vars.expr(i);
		if (! (v instanceof RTVar)) continue;
		int p = ((RTVar) v).phase();
		if (p>phase) phase = p;
	    }
	}

	// get data after live Phase
	protected Data getAllData(RTContext ctxt) throws Xcept {
	    // already complete?
	    if (complete) 
		return data;

	    // never created live data?
	    if (data == null) {
		data = model.getAllData(ctxt, oexpr, null, null);
		complete = true;
		return data;
	    }

	    // partially completed live data
	    update(ctxt, liveDomain, ctxt.dct(liveDomain));
	    return data;
	}

	// get data during live Phase
	protected void update(RTContext ctxt, RTRealDomain x, int hix) 
	throws Xcept {
	    if (x == null) throw new Xcept(
		"RTLiveQuery update domain is null");
	    if (liveDomain == null) liveDomain = x;
//	    System.err.println("LiveQuery update " + oexpr + 
//		" phase #" + phase + " " + x.name + "=" + hix);
	    if (x != liveDomain) throw new Xcept(x, liveDomain,
		"RTLiveQuery domain conflict");
	    if (data == null) data = makeData(ctxt);

	    // calculate new data only for non RTVars exprs
	    if (! (oexpr instanceof RTVar)) 
		for (int i=data.subset.hix; i<hix; i++) 
		    calcData(ctxt, i);

	    // store last calc index, check complete
	    data.subset.hix = hix;
	    if (hix == ctxt.dct(liveDomain)) complete = true;
	}

	// make data, first time
	private Data makeData(RTContext ctxt) throws Xcept {

	    // RTVars,  just use internal data array
	    if (uexpr instanceof RTRealNVar) {
		data = ctxt.ndata((RTRealNVar) uexpr);
	    } else if (uexpr instanceof RTRealDomain) {
		data = ctxt.grid((RTRealDomain) uexpr);

	    // others create working data array
	    } else {
		String desc = oexpr.toString();
		Unit u = uexpr.unit();
		GridData[] grids = new GridData[domains.size()];
		int j=0;
		for (int i=0; i<domains.size(); i++) {
		    RTRealDomain x = (RTRealDomain) 
			domains.get(i);
		    grids[j++] = ctxt.grid(x);
		}
		data = new RealNData(desc, u, grids);
	    }

	    // create data subset and return
	    data.subset = new Data.Subset();
	    data.subset.gridInx = domains.indexOf(liveDomain);
	    if (data.subset.gridInx < 0) throw new Xcept(uexpr,
		liveDomain, "RTLiveQuery domain missing");
	    return data;
	}

	// calculate data point(s) for expr data
	private void calcData(RTContext ctxt, int xinx)
	throws Xcept {
	    // 1D data
	    if (data.ndim() == 1) {
		ctxt.setDInx(liveDomain, xinx);
	    	double d = uexpr.realVal(ctxt);
	    	((RealNData) data).set(xinx, d);
		return;
	    }
	    if (data.ndim() != 2) return;

	    // 2D data
	    RTRealDomain x = (RTRealDomain) domains.expr(0);
	    RTRealDomain y = (RTRealDomain) domains.expr(1);
	    boolean xlive = (x == liveDomain);
	    RTRealDomain deadDomain = xlive ? y : x;
	    int deadInx = xlive ? 1 : 0;
	    int[] gpos = new int[] { xinx, xinx };
	    ctxt.setDInx(liveDomain, xinx);
	    for (int i=0; i<ctxt.dct(deadDomain); i++) {
		ctxt.setDInx(deadDomain, i);
	    	double d = uexpr.realVal(ctxt);
		gpos[deadInx] = i;
		int inx = data.inx(gpos);
//System.err.println("calcData inx=" + inx + " d=" + d);
	    	((RealNData) data).set(inx, d);
	    }
	}

	// simple query
	public int phase() { return phase; }
	public Data data() { return data; }
	public String diagInfo() { 
	    return "Live Query " + oexpr; 
	}

	// RTLiveQuery.List
	public static class List extends ArrayList<RTLiveQuery> {
	    public List() { super(); }
	    public RTLiveQuery query(int i) { 
		return (RTLiveQuery) get(i);
	    }
	    public RTLiveQuery query(Expr e) {
		for (int i=0; i<size(); i++) 
		    if (e.sameAs(query(i).oexpr))
			return query(i);
		return null;
	    }
	} 
}


