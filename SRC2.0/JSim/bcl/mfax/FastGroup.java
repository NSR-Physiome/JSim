/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Group of fast reactions

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import java.util.ArrayList;

class FastGroup implements DiagInfo {
	MFSys sys;		// system attached
	Region region;		// reacting in this region
	Reaction.List reac;	// reactions in group
	Chem.List chem;		// chems in group
	boolean[] clamp;	// inx by chem,  true if conc or conc:t clamped
	Var.List concF;		// single species conc:t for each reaction
	Var[] concZ;		// zero concentration test variable, if reqd
	Var[] concDTX;		// external conc:t for each reaction, if non-zero
	Var concDir;		// conc to solve directly, to increase
				//    numeric accuracy
	boolean ENABLE_DIRECT = true; 
	    // true: to solve concDir directly for entire
	    // false: solve concDir directly for IC only,  DE for entire
	    //   set true for best accuracy,  false for rough debuggin

	// constructor
	public FastGroup(Region reg, ReactionFast r) throws Xcept {
	    region = reg;
	    sys = region.sys;
	    region.fastGroups.add(this);
	    region.fastChem.addUniq(r.eqn.chem);
	    reac = new Reaction.List(4);
	    chem = new Chem.List(4);
	    add(r);
	}

	// add reaction to group
	public void add(ReactionFast r) throws Xcept {
	    reac.add(r);
	    chem.addUniq(r.eqn.chem);
	}

	// solve concentrations in this group
	public void solve2() throws Xcept {
	    Util.verbose("== Solving FastGroup " + chem);

	    // create concF vars for each reactions
	    concF = new Var.List(reac.size());
	    concZ = new Var[reac.size()];
	    for (int i=0; i<reac.size(); i++) {
		ReactionFast r = (ReactionFast) reac.reac(i);
		RealNVar F = new RealNVar(r, "concF", sys.tlist());
		F.setAccess(Comp.PRIVATE);
		Unit u = chem.chem(0).unit().div(r.timeUnit());
		F.setUnit(u);
		concF.add(F);

		// Zero Concentration test variable concZ,  create
		// if reaction potentially singular (!=1 mol on both sides)
		// if test met, concF for reaction will be 0 
		// this is a hack, but exact solution has not yet been found 
		if (r.eqn.rtot() == 1 && r.eqn.rchem.size() == 1) continue;
		if (r.eqn.ltot() == 1 && r.eqn.lchem.size() == 1) continue;
		concZ[i] = new RealNVar(region, r.name() + "concZ", 
		    sys.tlist());
	    	concZ[i].setAccess(Comp.PRIVATE);
	    	Expr ztest = Expr.truex;
	    	for (int j=0; j<r.eqn.chem.size(); j++) {
		    Chem c = r.eqn.chem.chem(j);
		    ztest = new LogicalExpr(IExpr.AND, 
		        ztest, region.conc(c).eq(Expr.zero));
	    	}
	    	Expr zrhs = new IfExpr(ztest, Expr.cons(1e-30), Expr.zero);
	    	region.setVar(concZ[i], zrhs, true);
	    }

	    // create & set concDTX, Chem conc
	    concDTX = new Var[chem.size()];
	    clamp = new boolean[chem.size()];
	    int deCount = 0;
	    int icCount = 0;
	    int clampCount = 0;
	    for (int i=0; i<chem.size(); i++) {
		Chem c = chem.chem(i);
		Var v = region.conc(c);
		Var vt = v.deriv(sys.t);

		// tally clamp and DE status		
		boolean vclamp = region.varSet(SubDom.entire(), v);
		boolean vtclamp = region.varSet(SubDom.entire(), vt);
		boolean vicclamp = region.varSet(sys.t.lhbc(), v);
		clamp[i] = vclamp || vtclamp;
		boolean concDE;
		if (vclamp) 
		    concDE = false;
		else if (vtclamp || vicclamp)
		    concDE = true;
		else if (concDir == null) {
		    concDE = !ENABLE_DIRECT;
		    concDir = v;
		} else
		    concDE = true;
		if (vclamp || vtclamp) clampCount++;
		if (vicclamp) icCount++;
		if (concDE) deCount++;
		Util.verbose("Status flags " + v + " " + 
		    vclamp + " " + vtclamp + " " + vicclamp + " " +
		    concDE + " " + icCount + " " + deCount);

		// if expr for concDTX non-zero,  create variable and eqn
		Expr e = clamp[i] ? ((Expr) vt) : region.slowDelta(c);
		if (! e.sameAs(Expr.zero)) {
		    RealNVar vtx = new RealNVar(region, c.name() + "concDTX",
		        sys.tlist());
		    vtx.setAccess(Comp.PRIVATE);
		    Unit u = c.unit().div(c.timeUnit());
		    vtx.setUnit(u);
		    concDTX[i] = vtx;
		    region.setVar(vtx, e, true);
		}
		
		// DEs for conc(c)
		if (concDE) {
		    if (v != concDir)
		        region.setVar(sys.t.lhbc(), v, Expr.zero, false);
		    e = concDTExpr(c);
		    region.setVar(v.deriv(sys.t), e, false);
		}

		// solve concDir directly
		if (v == concDir) {
		    Expr sd = concDE ? sys.t.lhbExpr() : Expr.truex;
		    Expr vx = concDirEqn(c);
		    Eqn.create(sys, sd, vx, null);
		}
	    }

	    // check if overspecified
	    int df = chem.size() - reac.size();
	    boolean err = (df <= 0);
	    df -= clampCount;
	    err = err || (df < 0);
	    String dfmsg = "Reaction group overspecified: " +
		"#reactions=" + reac.size() +
		"  #forced conc=" + clampCount; 
	    if (err) throw new Xcept(this, dfmsg);
	    Util.verbose(dfmsg);

	    // check #ICs
	    int ct = deCount;
	    if (!ENABLE_DIRECT) ct--;
	    if (icCount > 0 && icCount != ct) throw new Xcept(this,
		"Must have 0 or " + ct + " ICs, " 
		+ icCount + " found");

	    // linear eqns solving concF's
	    for (int i=0; i<reac.size(); i++) {
		ReactionFast r = (ReactionFast) reac.reac(i);
		Expr lhs = r.k.mult(linX(r.eqn, true));
		Expr rhs = linX(r.eqn, false);
		if (concZ[i] != null) 
		    rhs = rhs.add(concZ[i].mult(concF.var(i)));
		Eqn eqn = new Eqn(sys, Expr.truex, lhs, IExpr.EQ, rhs);
	    }
	}

	// direct solution eqn for chem 
	public Expr concDirEqn(Chem c) throws Xcept {
	    Var v = region.conc(c);
	    ReactionFast r = null;
	    for (int i=0; i<reac.size(); i++) {
		if (reac.reac(i).eqn.factor(c) == 0) continue;
		r = (ReactionFast) reac.reac(i);
		break;
	    }
	    if (r == null) throw new Xcept(this,
		"JSIM internal error - fast reac for chem not found");
	    double nc = r.eqn.factor(c);
	    Expr lhs = r.k.pow(1/nc);
	    Expr rhs = Expr.one;		    
	    for (int i=0; i<r.eqn.chem.size(); i++) {
		Chem c1 = r.eqn.chem.chem(i);
		Var v1 = region.conc(c1);
		double n = r.eqn.factor(c1);
		rhs = rhs.mult(v1.pow(n/nc));
	    }
	    return lhs.eq(rhs).simplify();
	}

	// t.delta term from  { product_i (C_i + C_i:t * t.delta)^n_i }
	public Expr linX(ChemEqn eqn, boolean left) throws Xcept {
	    Chem.List xchem = left ? eqn.lchem :eqn.rchem;
	    double[] coef = left ? eqn.lcoef : eqn.rcoef;
	    Expr etot = Expr.zero;
	    for (int i=0; i<xchem.size(); i++) {
		Chem c = xchem.chem(i);
		double ni = coef[i];
		Expr e = Expr.cons(ni).mult(region.conc(c).pow(ni-1));
		e = e.mult(concDTExpr(c));
		Expr ep = Expr.one;
		for (int j=0; j<xchem.size(); j++) {
		    if (j==i) continue;
		    Chem cj = xchem.chem(j);
		    double nj = coef[j];
		    ep = ep.mult(region.conc(c).pow(nj));
		}
		e = e.mult(ep);
		etot = etot.add(e);
	    }
	    return etot.simplify();
	}
    
	// expr for conc(c):t
	public Expr concDTExpr(Chem c) throws Xcept {
	    int inx = chem.indexOf(c);
	    Expr e = concDTX[inx];
	    if (e == null) e = Expr.zero;
	    if (clamp[inx]) return e;

	    // add fast reaction DT terms
	    for (int i=0; i<reac.size(); i++) {
		double n = reac.reac(i).eqn.factor(c);
		e = e.add(Expr.cons(n).mult(concF.var(i)));
	    }
	    return e.simplify();
	}

	// diagnostics
	public String diagInfo() { 
	    return "Fast reaction group " + chem + 
		" compartment " + region;
	}

	// FastGroup.List
	public static class List extends ArrayList<FastGroup> {
	    public List(int n) { super(n); }
	    public FastGroup group(int i) { return (FastGroup) get(i); }
	}	

}

