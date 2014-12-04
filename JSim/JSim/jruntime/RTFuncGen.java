/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// run-time function generator

package JSim.jruntime; 

import JSim.aserver.*;
import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.fgen.*;
import java.util.ArrayList;

public class RTFuncGen extends NamedExpr {
	private RTModel model;
	private String name; 
	private FgenMaster[] fgens; // RT calcs for each thread

	// constructor
	public RTFuncGen(RTModel model, String name) {
	    this.model = model;
	    this.name = name;
	}

	// allocate fgen for each comp thread
	protected void allocThreads(int n) throws Xcept {
	    fgens = new FgenMaster[n];
	}

	// clear fgen for run
	protected void runPrep(RTContext ctxt) throws Xcept {
	    fgens[ctxt.threadInx] = null;
	}

	// real value for context
    	public double realVal(Context ctxt0) throws Xcept {
	    if (! (ctxt0 instanceof FgenContext)) throw new Xcept(this, 
		"realVal() requires FgenContext");
	    RTContext ctxt = (RTContext) ctxt0;
	    int inx = ctxt.threadInx;
	    if (fgens[inx] == null)
	    	fgens[inx] = new FgenMaster(name());
	    return fgens[inx].realVal(ctxt);
	}

	// final val from run
	public double finalRealVal() throws Xcept {
	    return Double.NaN;
	}

	// create SettingVar
	public NamedExpr getVar(String s) {
	    if (name.equals(s)) return this;
	    if (! s.startsWith(name + ".")) return null;
	    String n = s.substring(name.length() + 1);
	    NamedVal nval = defVals.namedVal(n);
	    if (nval == null) return null;
	    int dataType = nval.dataType();
	    if (dataType == Expr.UNDEFINED) return null;
	    return new RTSettingVar(model, s, dataType);
	}

	// query
	public int dataType() { return Expr.REAL; }
	public String toString() { return name; }
	public String toString(Context ctxt) { return name; }
	public String name() { return name; }
	public Unit unit() { return null; }
	public Expr unitCorrect() { return this; }
	public boolean isDomain() { return false; }
	public int ndim() { return 1; } // ??? needs work

	// other Expr stuff
	public Expr takeDomDeriv(NamedExpr x) throws Xcept {
	    throw new Xcept("RTFuncGen.takeDomDeriv not implemented");
	}
	public Expr expandDeriv() { return this; }
	public void addDomains(Expr.List list) {
	    for (int i=0; i<2; i++) {
	    	try { 
		    addDomain(i, list); 
		} catch (Xcept e) {
		    // System.err.println("" + e);  s/b Xcept !!!
		}
	    }
	}
	private void addDomain(int i, Expr.List list) throws Xcept {
	    NamedVal xval = model.namedVal(name + ".domain" + i);
	    if (xval == null) return;
	    String xname = xval.stringVal();
	    if (Util.isBlank(xname)) return;
	    RTVar x = model.getVar(xname);
	    if (x instanceof RTRealDomain)
		list.addUniq(x);
	}
	public boolean sameAs(Expr expr) { return this == expr; }

	// List of fgens
	public static class List extends ArrayList<RTFuncGen> {
	    public RTFuncGen get(String n) {
	    	for (int i=0; i<size(); i++) 
		    if (get(i).name().equals(n)) 
		    	return get(i);
		return null;
	    }

	    // update to match name list, return true if changed
	    public boolean update(RTModel model, String[] names) {
		boolean changed = false;
		for (int i=0; i<names.length; i++) {
		    if (get(names[i]) != null) continue;
		    add(new RTFuncGen(model, names[i]));
		    changed = true;
		}
	 	for (int i=0; i<size(); i++) {
		    String name = get(i).name();
		    boolean found = false;
		    for (int j=0; j<names.length; j++) {
		    	if (! names[j].equals(name)) continue;
			found = true;
			break;
		    }
		    if (found) continue;
		    remove(i);
		    changed = true;
		}
		return changed;
	    }

	}

	private static NamedVal.NList defVals = FgenMaster.getDefaults();
}
