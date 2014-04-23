/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// CML Math equation

package JSim.cellml;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class CMLEqn implements DiagInfo {
    private CMLComp comp;
    private CompareExpr eqn;
    protected CMLVar derivVar; // Set by CMLComp if this is of the form dx:dy=**
    protected CMLVarGroup solvedVarGroup; //Will be something unless overdetermined--figured out by CMLEqnSorter.
    private CMLVar.List allVars;
    protected boolean unitConversionOK; // true=passes unit correction

    // constructor
    protected CMLEqn(CMLComp c, Expr e) throws Xcept {
        comp = c;
        allVars = new CMLVar.List();
        if (! (e instanceof CompareExpr)) 
            throw new Xcept(comp, eqn,
                "Equation or relation expected but found:\n   '" + e.toString() 
		+ "'\n If this is a partial differential equation model, JSim cannot yet translate these models.");
        eqn = (CompareExpr) e;
        Expr.List allEqs = new Expr.List();

        Expr lhs = eqn.arg(0);
        lhs.addNamedExpr(allEqs);
        
        Expr rhs = eqn.arg(1);
        rhs.addNamedExpr(allEqs);

        for (int v=0; v<allEqs.size(); v++) {
            if (! (allEqs.expr(v) instanceof CMLVar))
                continue;
            allVars.add( (CMLVar)(allEqs.expr(v)) );
        }
    }

    // add any domains from the list of allVars to the solved variable's domains.
    //    return whether any domains added
    protected boolean expandSolvedDomains() {
        // //Util.verbose("expandSolvedDomains begin: " + eqn);
        boolean addany = false;
        CMLVarGroup solvedVG = solvedVarGroup;
        if (solvedVG == null) return addany;
        for (int i=0; i<allVars.size(); i++) {
            CMLVar depvar = allVars.var(i);
            CMLVarGroup depVG = allVars.var(i).varGroup;
            boolean add1 = 
                solvedVG.domains.addAll(depVG.domains);
            if (depVG.isDomain && !solvedVG.domains.contains(depVG)) {
                solvedVG.domains.add(depVG);
                add1 = true;
            }
            addany = addany || add1;
        }
        if (addany) {
            // //Util.verbose("expandSolvedDomains: " + eqn);
        }
        // //Util.verbose("expandSolvedDomains end: " + eqn);
        return addany;
    }

    // MML output
    protected void writeMML(PrintWriter out) throws Xcept {
        out.println("\t" + eqn.toString(comp.doc.ctxt) + ";");
    }

    // query
    public String toString() { return eqn.toString(); }
    public String diagInfo() { return "CellML equation " + this; }

    //Creates a new list of all variables mentioned in this equation and returns it.
    public CMLVarGroup.List getAllVars() {
        CMLVarGroup.List ret = new CMLVarGroup.List();
        for (int v=0; v<allVars.size(); v++) {
            ret.addUniq(allVars.var(v).varGroup);
        }
        return ret;
    }

    //Looks at the equation and sees whether the variable it solves for exists as
    // a derivative.  Ideally would check both the left- and right-hand side of the
    // equation, but for now, only checks the left.
    public boolean solvesDeriv() {
        if (derivVar==null) return false;
        if (derivVar.varGroup == solvedVarGroup) {
            return true;
        }
        return false;
    }

    // check if eqn is unit correct
    protected void checkUnitConversion() throws Xcept {
        try {
            eqn.unitCorrect();
            unitConversionOK = true;
        } catch (Xcept e) {
            Util.verbose("UNIT ERROR: " +  e.cleanMessage());
            unitConversionOK = false;
            checkPowerVars();
        }
    }

    // try replacing power vars with constants
    private void checkPowerVars() throws Xcept {
 	Expr.List powerVars = new Expr.List();
	addPowerVars(eqn, powerVars);
	if (powerVars.size() == 0) return;
	Util.verbose("  POWER VARS: " + powerVars);
	Expr.List powerValues = new Expr.List();
	for (int i=0; i<powerVars.size(); i++) {
	    CMLVar v = (CMLVar) powerVars.expr(i);
	    Expr value = v.varGroup.substituteValue();
	    if (value == null) return;
	    powerValues.add(value);
	}
	Util.verbose("  POWER VALUES: " + powerValues);
	Expr peqn = eqn.replace(powerVars, powerValues);
	Util.verbose("  POWER EQN: " + peqn);
	try {
	    peqn.unitCorrect();
	    unitConversionOK = true;
	    Util.verbose("  POWER UNITS OK");
	    for (int i=0; i<powerVars.size(); i++) 
	    	comp.doc.updatePowerVars((CMLVar) powerVars.expr(i),
		    powerValues.expr(i));
 	} catch (Xcept e) {
	    unitConversionOK = false;
	    Util.verbose("  POWER UNIT ERROR: " +  e.cleanMessage());
	}
    }

    // substitute global power var/value lists
    protected void substituteValues(Expr.List vars, Expr.List vals) 
    throws Xcept {
	Util.verbose("  OLD EQN: " + eqn);
    	eqn = (CompareExpr) eqn.replace(vars, vals);
	Util.verbose("  NEW EQN: " + eqn);
    }

    // collect power exponent vars
    private void addPowerVars(Expr expr, Expr.List vlist) throws
    Xcept {
        if (! (expr instanceof IExpr)) return;
	IExpr iexpr = (IExpr) expr;
	for (int i=0; i<iexpr.nargs(); i++) {
	    Expr arg = iexpr.arg(i);
	    if (iexpr.op() == IExpr.POW && i==1)
	    	arg.addNamedExpr(vlist);
	    else
	        addPowerVars(arg, vlist);
	}
    }

    // CMLEqn.List
    public static class List extends ArrayList<CMLEqn> {
        public List() { super(); }
        public CMLEqn eqn(int i) { return (CMLEqn) get(i); }
        protected boolean expandSolvedDomains() {
            boolean addany = false;
            for (int i=0; i<size(); i++) {
                boolean add1 = eqn(i).expandSolvedDomains();
                addany = addany || add1;
            }
            return addany;
        }
    }
}
