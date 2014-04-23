/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// group of connected variables

package JSim.cellml;

import java.io.*;
import java.util.ArrayList;
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class CMLVarGroup implements DiagInfo {
    private CMLDoc doc;
    protected String mmlName;
    protected CMLVar.List vars;
    protected StringList varNames;
    protected boolean isDomain;
    protected boolean isInput;
    private String initVal;
    private Unit unit;
    protected boolean hasDeriv;
    protected boolean solvedByDeriv;
    private boolean isSolvedByEqn; //Only a property of var groups, not vars.
    protected List domains;
    protected boolean isStatic; // var must be immutable for unit correction

    // constructor
    public CMLVarGroup(CMLDoc d, CMLVar v) throws Xcept {
        doc = d;
        vars = new CMLVar.List();
        varNames = new StringList();
        domains = new List();
        //Util.verbose("new CMLVarGroup");
        isInput = true;
        hasDeriv = false;
        solvedByDeriv = false;
        add(v);
        mmlName = d.createLegalNameFrom(v.fullName());
        addDomainsFrom(v);
    }

    // add connected vars
    private void add(CMLVar v) throws Xcept {
        if (v.varGroup != null) return;
        //Util.verbose("  adding " + v.fullName());
        vars.add(v);
        v.varGroup = this;
        if (v.isDomain) isDomain = true;
        if (! v.isInput) isInput = false;
        if (! Util.isBlank(v.initVal)) initVal = v.initVal;
        if (v.unit != null) unit = v.unit;
        if (v.hasDeriv) hasDeriv = true;
        addDomainsFrom(v);

        // build varNames, sorted by length
        String vname = v.mmlName;
        if (! varNames.containSame(vname)) {
            int i = 0;
            while (i<varNames.size() 
                   && vname.length() > varNames.str(i).length()) {
                i++;
            }
            if (i>=varNames.size() 
                || !vname.equals(varNames.str(i))) {
                varNames.add(vname);
            }
        }

        // add connected vars
        for (int i=0; i<v.connectVars.size(); i++) {
            add(v.connectVars.var(i));
        }
    }

    //Is this variable not solved in some way?
    protected boolean isUnderdetermined() {
        if (isDomain) return false;
        if (!isSolvedByEqn && !solvedByDeriv && Util.isBlank(initVal)) {
            return true;
        }
        if (solvedByDeriv && Util.isBlank(initVal)) {
            return true;
        }
        return false;
    }

    //Is this variable solved in multiple ways?
    protected boolean isOverdetermined() {
        if (isSolvedByEqn && !solvedByDeriv && !Util.isBlank(initVal)) {
            return true;
        }
        return false;
    }

    // write MML declaration
    protected void writeMML(PrintWriter out) throws Xcept {

        String unitname = "";
        if (unit == null) {
            out.println("\t//Warning:  " + mmlName + " has no declared units.");
        }
        else {
            unitname = " " + unit.pubName();
        }
        // realDomain
        if (isDomain) {
            if (Util.isBlank(initVal)) initVal = "0";
            out.println("\trealDomain " + mmlName + 
                        unitname + ";");
            out.println("\t" + mmlName + ".min=" + initVal + ";");
            out.println("\textern " + mmlName + ".max;");
            out.println("\textern " + mmlName + ".delta;");
            return;
        }
	
        // real
        if (isStatic) out.println
            ("//\tVar below replaced by constant in model eqns to satisfy unit correction");
        boolean isExtern = !isSolvedByEqn && !solvedByDeriv && Util.isBlank(initVal);
	String startLine = isStatic ? "//" : "";
        out.println(startLine + 
                    "\t" + (isExtern ? "extern " : "") +
                    "real " + mmlName +  
                    domains.mmlName() + unitname + ";");
  
        // assume IC=0 if solved by a derivative but has no initial value, but warn the user we are doing so.
        if (solvedByDeriv && Util.isBlank(initVal)) {
            initVal="0";
            out.println("\t//Warning:  Assuming zero initial condition; nothing provided in original CellML model.");
        }

	// write cellmlMLNames
	StringBuffer buf = new StringBuffer(startLine + "\t" + 
	    mmlName + ".cellMLNames=\"");
	for (int i=0; i<vars.size(); i++) {
	    CMLVar v = vars.var(i);
	    if (i>0) buf.append(";");
	    buf.append(v.fullName());
	}
	buf.append("\";");
	out.println(buf);

        // write initVal (IC or entire)
        if (Util.isBlank(initVal)) return;
        String SD = "";
        if (solvedByDeriv) {
            String tname = icDomain().mmlName;
            SD = "when(" + tname + "=" + tname + ".min) ";
        }
        String comment = (isSolvedByEqn && !solvedByDeriv) ? "//Warning: CellML initial value suppressed to prevent overdetermining model.  Original initial value: " : "";
        out.println(startLine +
                    "\t" + comment + 
                    SD + mmlName + "=" + initVal + ";");
    }

    // add domains from an added var
    protected void addDomainsFrom(CMLVar v) {
        if (v.domains==null) return; 
        for (int j=0; j<v.domains.size(); j++) {
            CMLVar x = (CMLVar) v.domains.get(j);
            CMLVarGroup xg = x.varGroup;
            domains.addUniq(xg);
        }
    }

    // IC domain
    protected CMLVarGroup icDomain() throws Xcept {
        if (domains.size() < 1) throw new Xcept(this, "Variable missing IC domain");
        return domains.varGroup(0);  // flawed !!!
    }

    // simple query
    public String diagInfo() {
        return "CellML VarGroup " + mmlName;
    }

    // Does this var group have an initial value set?
    public boolean hasInitValue() {
        if (Util.isBlank(initVal)) return false;
        return true;
    }

    public boolean initValueIsString() {
        if (!hasInitValue()) return false;
        try {
            Double.parseDouble(initVal);
            return false;
        }
        catch (NumberFormatException x) {
            return true;
        }
    }

    protected void setIsSolvedByEqn(boolean solved) {
        isSolvedByEqn = solved;
    }

    // power-exponent value substitution
    protected Expr substituteValue() throws Xcept {
        if (domains.size() > 0) return null;
    	if (Util.isBlank(initVal)) return null;
        if (initValueIsString()) return null;
        double value = Util.toDouble(initVal);
        return new RealConst(value, unit);
    }	

    // CMLVarGroup.List
    public static class List extends ArrayList<CMLVarGroup> {
        public List() { super(); }
        public List(int n) { super(n); }
        public CMLVarGroup varGroup(int i) {
            return (CMLVarGroup) get(i);
        }
        public void addUniq(CMLVarGroup v) {
            if (v==null) return;
            if (! contains(v)) add(v);
        }
        public boolean addAll(List list) {
            boolean addany = false;
            for (int i=0; i<list.size(); i++) {
                CMLVarGroup vg = list.varGroup(i);
                if (! contains(vg)) {
                    addany = true;
                    add(vg);
                }
            }
            return addany;
        }
        public String mmlName() {
            if (size() == 0) return "";
            String s = "(";
            for (int i=0; i<size(); i++) {
                CMLVarGroup cvg = varGroup(i);
                if (i>0) s = s + ",";
                s = s + varGroup(i).mmlName;
            }
            return s + ")";
        }
    }
}
