/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1D PDE factorization for a DETool
// factor PDE to form v:t = D*v:x:x + P  (Toms731)
//                and v:t = D*v:x:x - B*v:x + S (LSFEA, MacCormack)
// factor BCs to form f1*v + f2*u:x = f3

package JSim.plan;

import java.util.*;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import JSim.aserver.*;

public class PDE1Factors extends PDEFactors {
	private Domain  x; // space
	private Var v, vt, vx, vxx;  // state var & derivs
	private DECon lhbc, rhbc; // BCs
	
	// PDE/BC factors
	private Expr coefD, coefDx, coefP, coefB, coefS; // state var coefs
	private Expr[] coefF1, coefF2, coefF3; // standard BC coefs [left,right]
	private Expr toms731_C, toms731_Q, toms731_R; // toms PDE coefs
	private Expr[] toms731_Beta, toms731_Gamma; // toms BC coefs [left,right]

	// solver support messages
	private String factorMsg; // basic PDE factoring error, if any
	private String advecMsg;  // advection factoring error, if any
	private String[] solverMsgs; // non-support reasons

	// constructor
	public PDE1Factors(DETool detool) throws Xcept {
	    super(detool);
	    if (detool.xs.size() != 1) throw new Xcept(
	    	"Wrong PDE dimension for PDE1Factors");
	    x = detool.xs.first();
	    v = detool.v;
	    vt = v.deriv(t);
	    vx = v.deriv(x);
	    vxx = vx.deriv(x);	    
	    lhbc = detool.bc(model().lhbcs.get(x));
	    rhbc = detool.bc(model().rhbcs.get(x));
	    coefF1 = new Expr[2];
	    coefF2 = new Expr[2];
	    coefF3 = new Expr[2];
	    toms731_Beta = new Expr[2];
	    toms731_Gamma = new Expr[2];

	    factorMsg = factorState();
	    if (factorMsg != null) return;
	    advecMsg = factorAdvection(); 
	    log("PDE(" + v + ") factors: " + 
	        " D=" + coefD +  
	        " P=" + coefP +  
	        " B=" + coefB +  
	        " S=" + coefS);  
	    factorMsg = factorBC(true);
	    if (factorMsg != null) return;
	    factorMsg = factorBC(false);
	    if (factorMsg != null) return;

	    solverMsgs = new String[3];
	    solverMsgs[ASModel.PDE_LSFEA] = lsfeaMsg();
	    solverMsgs[ASModel.PDE_MacCormack] = macCormackMsg();
	    solverMsgs[ASModel.PDE_Toms731] = toms731Msg();

	    for (int i=0; i<solverMsgs.length; i++) 
		if (solverMsgs[i] == null)
		    log("  Solver " + ASModel.PDE_Solvers[i] 
		    	+ " supported");
		else
		    log("  Solver " + ASModel.PDE_Solvers[i] 
		    	+ " unsupported: " + solverMsgs[i]);
	}
	
	// factor state eqn
	private String factorState() throws Xcept {
	    Expr e0 = zeroExpr(detool.state());
	    Expr coefVT = e0.linearFactor(vt, true).simplify();
	    e0 = e0.linearFactor(vt, false).mult(Expr.negone).simplify();
	    coefD = e0.linearFactor(vxx, true).div(coefVT);
	    coefD = coefD.simplify();
	    coefP = e0.linearFactor(vxx, false).div(coefVT).simplify();

	    // check factorization
	    VarUsages vus = new VarUsages(model(), 
	    	new Expr[] { coefD, coefP });
	    if (vus.hasVar(vt) || vus.hasVar(vxx))
		return "PDE " + detool.state() + 
		  " could not be factored to ideal form: " 
		  + vt + " = D*" + vxx + " + P";
	    if (containsDerivs(coefD))
	    	return "Diffusion term (" + coefD + ") may not contain derivatives";
	    if (realVal(coefD) < 0) return
	    	"PDE has negative diffusion term: " + coefD;
	    return null;
	}
	
	// create separate advection (coefB) if possible
	private String factorAdvection() throws Xcept {
	    coefB = coefP.linearFactor(vx, true).mult(Expr.negone).simplify();
	    coefS = coefP.linearFactor(vx, false).simplify();
	    String msg = factorAdvection(coefD, "Diffusion term " + coefD);
	    if (msg == null)
	        msg = factorAdvection(coefB, "Advection term " + coefB);
	    if (msg == null) 
	        msg = factorAdvection(coefS, "Source term " + coefS);
	    if (msg != null)
	    	coefB = coefS = null;
	    return msg;
	}
	
	// check 1 advection term for restrictions
	private String factorAdvection(Expr coef, String name) throws Xcept {
	    Expr.List list = new Expr.List(1);
	    VarUsages vus = new VarUsages(model(), coef);
	    if (vus.hasVar(vx)) // factorization, sure
		return name + " contains " + vx;
	    else if (containsDerivs(coef))
	        return name + " contains derivatives";
	    return null;
	}

	// factor BC to [f1,f2,f3]: f1*v + f2*u:x = f3
	private String factorBC(boolean left) throws Xcept {
	    DECon con = left ? lhbc : rhbc;
	    Expr[] fs = new Expr[3];
	    Expr bcz = zeroExpr(con);
	    fs[0] = bcz.linearFactor(v, true).simplify();
	    Expr rem = bcz.linearFactor(v, false).simplify();
	    fs[1] = rem.linearFactor(vx, true).simplify();
	    Expr f3 = rem.linearFactor(vx, false).mult(Expr.negone);
	    fs[2] = f3.simplify();
	    VarUsages vus = new VarUsages(model(), fs);
	    if (vus.hasVar(vt) || vus.hasVar(vx) || vus.hasVar(vxx)
	    || containsDerivs(fs)) throw new AbortXcept(
	        "PDE BC " + con + " cannot be factored to ideal form: " +
		"f1*" + v + " + f2*" + vx + " = f3");
	    coefF1[bcinx(left)] = fs[0];
	    coefF2[bcinx(left)] = fs[1];
	    coefF3[bcinx(left)] = fs[2];
	    log("  " + (left ? "LHBC" : "RHBC") + 
	    	" F1=" + coefF1(left) +
	    	" F2=" + coefF2(left) +
	    	" F3=" + coefF3(left));
	    return null;
	}

	// LSFEA unsupported msg (null if OK) 
	public String lsfeaMsg() throws Xcept {
	    if (advecMsg != null) 
	    	return advecMsg;
	    if (coefB == null) 
	        return "PDE state eqn not factorable";

	    // B,D spatially independence
	    if (hasXDependence(coefB)) return 
	    	"Advection term " + coefB + " has spatial dependency";
	    if (hasXDependence(coefD)) return
	    	"Diffusion term " + coefD + " has spatial dependency";
	    
	    // plan-time sign checks (NaNs indicate insuff. info)
	    if (realVal(coefD) < 0) 
	        return "Negative diffusion term: " + coefD;
	    double Bval = realVal(coefB);
	    if (Bval == 0 &&
	    	(realValNonZero(coefF1(true)) 
		|| realValNonZero(coefF1(false))
		|| realValNonZero(coefF3(true))
		|| realValNonZero(coefF3(false)))) return
		"Zero advection requires zero f1, f3, g1 and g3";
	    if (Bval > 0 && realVal(coefF1(true)) == 0) return
		"Positive advection requires non-zero f1";
	    if (Bval < 0 && realVal(coefF1(false)) == 0) return
		"Negative advection requires non-zero g1";
	    if (realValNonZero(coefF1(true)) 
	    && realValNonZero(coefF1(false))) return
		"At least one of f1,g1 must be zero";

	    // check below may be redundant, given above
	    if (realVal(coefF2(true)) == 0 
	    && realVal(coefF2(false)) == 0) return
		"At least one of f2,g2 must be non-zero";	    

	    // BC checks done in finishFactors
	    return null;
	}

	// lsfea BC's msg depend upon block vars
	protected void setBlockVars(LinkedHashSet<Var> vblock) 
	throws Xcept {
	    if (solverMsgs == null) return;
	    String s = solverMsgs[ASModel.PDE_LSFEA];
	    if (s == null) 
	    	s = lsfeaBCMsg(vblock, true);
	    if (s == null) 
	    	s = lsfeaBCMsg(vblock, false);
	    solverMsgs[ASModel.PDE_LSFEA] = s;
	}
	
	// check lsfeaBC: f1-g3, throw Xcept if not OK
	private String lsfeaBCMsg(LinkedHashSet<Var> vblock, boolean left) throws Xcept {
	    VarUsages vus = new VarUsages(model(), 
	        new Expr[] { coefF1(left), coefF2(left), coefF3(left) });
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (vblock.contains(vu.v())) return
		    "BC " + bc(left) + 
		    " depends on PDE block variable " + vu;
	    }
	    return null;
	}

	// MacCormack support message
	private String macCormackMsg() throws Xcept {
	    if (advecMsg != null) 
	    	return advecMsg;
	    if (coefB==null || coefS==null) return
	        "PDE state equation not factorable";
	    VarUsages vus = new VarUsages(model(), 
	    	new Expr[] { coefD, coefB, coefS });
	    if (vus.hasVar(vx)) return
	        "Unsupported " + vx +  " in PDE state eqn";
	    if (realVal(coefD) < 0) return
	    	"Negative diffusion term: " + coefD;
	    return null;
	}

	// Toms731 support message, excluding coefDx which is done later
	private String toms731Msg() throws Xcept {
	    if (coefD == null) 
	        return "PDE state eqn not factorable";
	    if (realVal(coefD) < 0) return
	    	"Negative diffusion term: " + coefD;
	    return null;
	}

	// set Toms731 coefDx
	protected void setCoefDx(Expr coefDx) throws Xcept { 
	    this.coefDx = coefDx; 
	    if (coefDx == null) 
	    	solverMsgs[ASModel.PDE_Toms731] = 
		    "Diffusion term not symbolically differentiable";
	    if (solverMsg(ASModel.PDE_Toms731) == null) {
	        factorToms731();
		log("  Tom731 " + 
		    " C=" + toms731_C +
		    " Q=" + toms731_Q +
		    " R=" + toms731_R);
		log("  Toms731 LHBC: " +
		    " Beta=" + toms731_Beta(true) +
		    " Gamma=" + toms731_Beta(true));
		log("  Toms731 RHBC: " +
		    " Beta=" + toms731_Beta(false) +
		    " Gamma=" + toms731_Beta(false));
	    }
	}

	// Toms731 factorization
	private void factorToms731() throws Xcept { 
	    toms731_C = Expr.one; 
	    toms731_Q = coefDx.mult(vx).sub(coefP).simplify();
	    toms731_R = coefD.mult(vx).simplify();
	    toms731_Beta = new Expr[2];
	    toms731_Gamma = new Expr[2];
	    factorToms731BC(true);
	    factorToms731BC(false);
	}

	// Toms731 BC factorization
	private void factorToms731BC(boolean left) throws Xcept {
	    Expr f1 = coefF1(left);
	    Expr f2 = coefF2(left);
	    Expr beta = new IfExpr(f2.eq(Expr.zero), Expr.zero, Expr.one);
	    toms731_Beta[bcinx(left)] = beta.simplify();

	    DECon con = left ? lhbc : rhbc;
	    Expr bcz = zeroExpr(con);
	    Expr GB0 = new IfExpr(f1.eq(Expr.zero), bcz, bcz.div(f1));
	    Expr GB1 = vx.sub(bcz.div(f2)).mult(coefD).simplify();
	    Expr gamma = new IfExpr(f2.eq(Expr.zero), GB0, GB1);
	    toms731_Gamma[bcinx(left)] = gamma.simplify();
	}

	//// Expr processing Utils

	// DECon expr = 0
	private Expr zeroExpr(DECon con) throws Xcept {
	    TEqn eqn = con.eqn();
	    if (eqn != null) {
	        CompareExpr cexpr = (CompareExpr) eqn.expr().expr();
	    	return cexpr.arg(0).sub(cexpr.arg(1)).simplify();
	    }
	    Tool tool = con.tool();
	    if (! (tool instanceof ExprTool)) throw new AbortXcept(
	    	"Can't factor PDE " +
		(con.isState() ? "state eqn" : "BC") + ": " +
		tool);
	    ExprTool etool = (ExprTool) tool;
	    Var v = etool.vu.v();
	    Expr expr = etool.expr.expr();
	    return v.sub(expr).simplify();
	}
	
	// does Expr have x dependence?
	private boolean hasXDependence(Expr expr) throws Xcept {
	    Expr.List doms = new Expr.List();
	    expr.addDomains(doms);
	    return doms.containSame(x);
	}

	// does Expr contains derivs
	private boolean containsDerivs(Expr expr) throws Xcept {
	    return containsDerivs(new Expr[] { expr });
	}    
	private boolean containsDerivs(Expr[] exprs) throws Xcept {
	    VarUsages vus = new VarUsages(model(), exprs);
	    for (int i=0; i<vus.size(); i++) 
	    	if (vus.get(i).v().isDeriv()) return true;
	    return false; 
	}
    
	// check real values, NaN indicate unknowns
	private double realVal(Expr expr) throws Xcept {
	    return expr.isConst() ? expr.constRealVal() : Double.NaN;
	}

	// is real value non-zero?
	private boolean realValNonZero(Expr expr) throws Xcept {
	    double d = realVal(expr);
	    if (Double.isNaN(d)) return false;
	    return d != 0;
	}

	// simple private query
	private TModel model() { return detool.model; }
	private int bcinx(boolean left) { return left ? 0 : 1; }
	private DECon bc(boolean left) { return left ? lhbc : rhbc; }

	// public query
	public Expr coefD() { return coefD; }
	public Expr coefDx() { return coefDx; }
	public Expr coefP() { return coefP; }
	public Expr coefB() { return coefB; }
	public Expr coefS() { return coefS; }
	public Expr coefF1(boolean left) { return coefF1[bcinx(left)]; }
	public Expr coefF2(boolean left) { return coefF2[bcinx(left)]; }
	public Expr coefF3(boolean left) { return coefF3[bcinx(left)]; }
	public Expr toms731_C() { return toms731_C; }
	public Expr toms731_Q() { return toms731_Q; }
	public Expr toms731_R() { return toms731_R; }
	public Expr toms731_Beta(boolean left) { return toms731_Beta[bcinx(left)]; }
	public Expr toms731_Gamma(boolean left) { return toms731_Gamma[bcinx(left)]; }
	public String factorMsg() { return factorMsg; }
	public String solverMsg(int id) throws Xcept {
	    if (factorMsg != null) return factorMsg;
	    if (id < 0 || id >= solverMsgs.length) throw new Xcept(
	    	"Unknown PDE 1D solver id=" + id);
	    return solverMsgs[id];
	}

	// logging
	protected void log(String msg) { model().log(msg); }
}
	
