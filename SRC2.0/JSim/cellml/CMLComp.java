/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// CML component

package JSim.cellml;

import java.io.*;
import java.util.LinkedHashSet;
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import JSim.util.*;
import JSim.mml.*;
import JSim.expr.*;
import JSim.mathml.*;

public class CMLComp implements Named, MLNameSpace {
    protected CMLDoc doc;
    private String name;
    protected CMLVar.NList vars;
    protected CMLEqn.List eqns;
    private MLCSymbol.NList csyms = new MLCSymbol.NList();

    // constructor
    protected CMLComp(CMLDoc d, Element elem) throws Xcept {
        doc = d;
        name = elem.getAttribute("name");
        //Util.verbose("processing component " + name);
        vars = new CMLVar.NList();
        eqns = new CMLEqn.List();
        
        NodeList nodes = elem.getElementsByTagName("variable");
        for (int i=0; i<nodes.getLength(); i++) {
            CMLVar v = new CMLVar(this, (Element) nodes.item(i));
            vars.add(v);
        }

        nodes = elem.getElementsByTagName("math");
        for (int i=0; i<nodes.getLength(); i++) {
            MLMath m = new MLMath((Element) nodes.item(i));
            Expr.List exprs = m.makeExprList(this, csyms);
            for (int j=0; j<exprs.size(); j++) {
                Expr expr = exprs.get(j);
                CMLEqn e = new CMLEqn(this, expr);
                eqns.add(e);
                processDomains(expr, e);
            }

        }

        makeVarNamesUnique();
        nodes = elem.getElementsByTagName("reaction");
        for (int i=0; i<nodes.getLength(); i++) {
            throw new Xcept("Error:  unable to translate CellML models using the deprecated 'reaction' construct.");
        }
    }

    // if eqn has v0:v1, add v1 to v0's domain list
    private void processDomains(Expr expr, CMLEqn e) throws Xcept {
        if (! (expr instanceof CompareExpr)) return;
        CompareExpr cexpr = (CompareExpr) expr;
        if (cexpr.op() != IExpr.EQ) return;
        if (! (cexpr.arg(0) instanceof RealBExpr)) return;
        RealBExpr bexpr = (RealBExpr) cexpr.arg(0);
        if (bexpr.op() != IExpr.DERIV) return;
        if (! (bexpr.arg(0) instanceof CMLVar)) return;
        if (! (bexpr.arg(1) instanceof CMLVar)) return;
        CMLVar v0 = (CMLVar) bexpr.arg(0);
        CMLVar v1 = (CMLVar) bexpr.arg(1);
        v0.domains.addUniq(v1);
        v1.isDomain = true;
        v0.hasDeriv = true;
        e.derivVar = v0;
    }

    // The 'createLegalNameFrom' routine may have accidentally created duplicate names:
    //  there are names in CellML models on the archive that distinguish themselves by
    //  varying the number of underscores they end with, for example.
    // This function renames all identical names to be unique again.
    protected boolean makeVarNamesUnique() throws Xcept {
        LinkedHashSet<String> uniqnames = new LinkedHashSet<String>();
        LinkedHashSet<String> dupnames = new LinkedHashSet<String>();
        boolean anydupes = false;
        for (int n=0; n<vars.size(); n++) {
            if(!uniqnames.add(vars.var(n).mmlName)) {
                dupnames.add(vars.var(n).mmlName);
                anydupes = true;
            }
        }
        if (!anydupes) return false;
        for (int n=0; n<vars.size(); n++) {
            String mmlName = vars.var(n).mmlName;
            if (dupnames.contains(mmlName)) {
                mmlName = createUniqueNameFrom(mmlName, "_v", uniqnames);
                uniqnames.add(mmlName);
                vars.var(n).mmlName = mmlName;
            }
        }
        return makeVarNamesUnique(); //To check in case something really weird went on.
    }

    //Uses the suffix plus a number to create a unique name for 
    protected String createUniqueNameFrom(String orig, String suffix, LinkedHashSet<String> avoiding)
        throws Xcept
    {
        int n=1;
        while(true) {
            String newname = CMLDoc.createLegalNameFrom(orig + suffix + n);
            if (!avoiding.contains(newname)) {
                return newname;
            }
            n++;
        }
    }

    // write MML eqns
    protected void writeMML(PrintWriter out) throws Xcept {
        out.println("");
        out.println("\t// <component name=\"" 
                    + name + "\">");
        for (int i=0; i<eqns.size(); i++) 
            eqns.eqn(i).writeMML(out);
    }

    // MLNameSpace methods
    public Expr compByName(String name) throws Xcept {
        return var(name);
    }
    public Unit unitByName(String name) throws Xcept {
        return doc.getUnit(name);
    }
    public String compNameByElement(Element elem) throws Xcept {
        throw new Xcept("Undefined csymbol " + 
                        UtilXML.getText(elem));
    }
    public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
        throw new Xcept("makeDeriv() not implemented");
    }
    public Expr funcCall(String name, Expr.List elist) throws Xcept {
        throw new Xcept("CellML function " + name + 
                        " not supported");
    }


    // simple query
    public String name() { return name; }
    public String diagInfo() { return "CellML component " + name; }
    public String spaceName() { return name; }
    public CMLVar var(String n) { return vars.var(n); }

    // CMLComp.NList
    public static class NList extends NamedList {
        public NList() { super(16); }
        public CMLComp comp(int i) { return (CMLComp) get(i); }
        public CMLComp comp(String n) {
            return (CMLComp) getByName(n);
        }
    }
}
