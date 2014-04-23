/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML unit for import

package JSim.sbml;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBUnit implements Named {
    protected SBModel sbmodel;
    protected UnitDefinition unit;
    protected SBUnitSpace mSBUnitSpace;
    protected String name;
    protected String expression;
    protected boolean isUnitExpression;

    // constructor
    public SBUnit(SBModel sm, UnitDefinition c) throws Xcept {
        sbmodel = sm;
        unit = c;
        name = unit.getId();
        mSBUnitSpace = sm.getSBUnitSpace();
        calculateExpression();
    }

    // query
    public String name() { return name; }
    public String diagInfo() { return "UnitDefinition " + name(); }
    public boolean isDefined() throws Xcept {
        return sbmodel.unitChecker.isDefined(name);
    }
    public void setIsUnitExpression(boolean iue) {isUnitExpression=iue;}

    // write MML
    public void writeMML(PrintWriter wrt) {
        //System.out.println("Writing out the unit " + name);
        if (!isUnitExpression) {
            //This unit cannot be used directly as the unit for a variable, so we must declare it.
            wrt.println("unit " + name + " = " + expression + ";");
        }
    }

    // declaration string
    public void calculateExpression() throws Xcept {
        double mult = 1;
        int scale = 0;
        String kind = "";
        double exp = 1;
        long nunits = unit.getNumUnits();
        isUnitExpression = true;
        for (int i=0; i<nunits; i++) {
            org.sbml.libsbml.Unit u = unit.getUnit(i);
            double localmult = 1.0;
            if (u.isSetMultiplier()) {
                localmult = u.getMultiplier();
            }
            if (u.isSetScale()) {
                scale = u.getScale();
            }
            if (u.isSetKind()) {
                kind = SBUnitConstants.kindToString(u.getKind());
            }
            if (u.isSetExponent()) {
                exp = u.getExponent();
            }
            String jsimkind = mSBUnitSpace.toJSimFromSBML(kind);
            String jsimunit = mSBUnitSpace.toUnitExpression(jsimkind, localmult, scale);
            if (jsimunit == null) {
                isUnitExpression = false; //at least one 'unit' has a multiplier, forcing us to define a new unit instead of just using a unit expression.
                jsimunit = mSBUnitSpace.toUnitExpression(jsimkind, 1.0, scale);
                if (jsimunit != null) {
                    jsimkind = jsimunit;
                    scale = 0;
                }
            }
            else {
                jsimkind = jsimunit;
                localmult = 1;
                scale = 0;
            }
            //System.out.println("unit " + kind + "(jsim version = " + jsimkind + "), scale " + scale + ", exponent = " + exp + ", multiplier = " + localmult);
            mult *= localmult * Math.pow(10, scale);
            String s = jsimkind;
            if (jsimkind == null) {
                //True for avogadro (for now)
                s = kind;
                mSBUnitSpace.addSBMLUnitDefinition(kind);
            }

            // pretty multiply of expression by s^exp
            //   repress 1's, .0s, negative exps use "/" instead
            String sexp = ("^" + Math.abs(exp));
            if (sexp.endsWith(".0")) 
                sexp = sexp.substring(0, sexp.length()-2);
            if (sexp.equals("^1"))
                sexp = "";
            s = s + sexp;
            if (exp > 0) {
                if (expression == null)
                    expression = s;
                else
                    expression = expression + "*" + s;
            } else {
                if (expression == null)
                    expression = "1/" + s;
                else
                    expression = expression + "/" + s;
            }
            
        }
        if (expression == null) expression = "scalar";

        // better would be to simplify(expression, mult) 
        //   but that's not available yet
        if (!isUnitExpression)
            expression = Util.pretty(mult) + " " + expression;
        else
            expression = mSBUnitSpace.simplifyJSim(expression);

	if (! sbmodel.unitChecker.isLegalName(name))
	    isUnitExpression = false;


        if (isUnitExpression) {
            //System.out.println("Adding that to the converter.");
	    //This means we don't ever write this out.
            mSBUnitSpace.addSBMLJSimConversion(name, expression); 
       }
        else {
            if (sbmodel.unitChecker.isConflict(name, expression)
	    || !sbmodel.unitChecker.isLegalName(name)) {
                sbmodel.changeUnit(name);
                name = sbmodel.mmlUnitName(name);
            }
        }
    }

    // SBUnit.List
    public static class NList extends ArrayList<Named> {
        public NList(SBModel sbm) throws Xcept {
            super();
            long nunits = sbm.model.getNumUnitDefinitions();
            for (int i=0; i<nunits; i++) {
                SBUnit sbu = new SBUnit(sbm, sbm.model.getUnitDefinition(i));
                if (!sbu.isDefined()) {
                    add(sbu);
                }
            }
        }
        public NList() { super(); }
        public SBUnit sbunit(int i) {
            return (SBUnit) get(i);
        }
        public Named sbunit(String n) {
            for (int u=0; u<size(); u++) {
                if (get(u).name().equals(n)) return get(u);
            }
            return null;
        }
        public void addFirst(SBModel sbm, UnitDefinition ud)
            throws Xcept {
            SBUnit sbu = new SBUnit(sbm, ud);
            sbu.setIsUnitExpression(false);
            if (sbunit(sbu.name()) == null) {
                add(0, sbu);
            }
        }
    }
}
