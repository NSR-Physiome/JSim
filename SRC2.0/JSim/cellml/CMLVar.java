/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// CML variable

package JSim.cellml;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class CMLVar extends NamedExpr  {
    protected CMLComp comp;
    protected CMLVar.List connectVars;
    protected CMLVarGroup varGroup;
    private String name;
    protected String mmlName;
    protected boolean isDomain;
    protected boolean isInput;
    protected String initVal;
    protected Unit unit;
    protected boolean hasDeriv;
    protected Expr.List domains;

    // constructor
    protected CMLVar(CMLComp c, Element elem) throws Xcept {
        comp = c;
        connectVars = new CMLVar.List(4);
        name = elem.getAttribute("name");
        mmlName = CMLDoc.createLegalNameFrom(name);
        //Util.verbose("  processing variable " + name);

        // set isInput
        String kpub = elem.getAttribute("public_interface");
        if (Util.isBlank(kpub)) kpub = "none";
        String kpri = elem.getAttribute("private_interface");
        if (Util.isBlank(kpri)) kpri = "none";
        isInput = kpub.equals("in") || kpri.equals("in");

        // unit
        String uname = elem.getAttribute("units");
        if (!Util.isBlank(uname)) {
            //There's actually a model out there with 'units=""'.
            unit = comp.doc.getUnit(uname);
        }
        else {
            unit = null;
        }
        
        // initial value
        initVal = elem.getAttribute("initial_value");

        // domains
        domains = new Expr.List();
        //Util.verbose("  done with variable " + name);
    }

    //// NamedExpr
    public Expr takeDomDeriv(NamedExpr x) throws Xcept {
        throw new Xcept(this, "takeDomDeriv() not implemented");
    }
    public Expr expandDeriv() throws Xcept { return this; }
    public Expr unitCorrect() { return this; }
    public Unit unit() { return unit; }
    public void addDomains(Expr.List exprs) {
        // needs work
    }
    public String toString(Context ctxt) { return toString(); }
    public String toString() { return mmlName(); }
    public boolean sameAs(Expr expr) {
        return this == expr;
    }
    public int dataType() { return REAL; }

    // query
    public String name() { return name; }
    public String fullName() {
        String cname = comp.name();
        return cname + "." + name;
    }
    public String mmlName() {
        if (varGroup == null) return fullName();
        return varGroup.mmlName;
    }
    public String diagInfo() { return "CellML variable " + fullName(); }

    // CMLVar.List
    public static class List extends ArrayList<CMLVar> {
        public List() { super(); }
        public List(int n) { super(n); }
        public CMLVar var(int i) { return (CMLVar) get(i); }
    }

    // CMLVar.NList
    public static class NList extends NamedList {
        public NList() { super(); }
        public CMLVar var(int i) { return (CMLVar) get(i); }
        public CMLVar var(String n) {
            return (CMLVar) getByName(n);
        }
    }
}
