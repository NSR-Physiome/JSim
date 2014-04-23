/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML import: map units to designated JSim unit space (currently nsrunit)

package JSim.sbml;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

import java.io.*;
import java.util.*;
import java.lang.Math.*;
import org.sbml.libsbml.*;

public class SBUnitSpace {
    protected SBMLDocument            mSDoc;
    protected SBModel                 mSBModel;
    private HashMap<String, String>   mSBMLToJSim;

    // constructor
    public SBUnitSpace(SBMLDocument sdoc, SBModel sm) throws Xcept {
        mSBModel = sm;
        mSDoc = sdoc;
        mSBMLToJSim = new HashMap<String, String>();

        // map SBML unit names to canoncal JSim names
        for (int i=0; i<SBUnitConstants.SBML_UNIT_NAMES.length; i++) {
            String sname = SBUnitConstants.SBML_UNIT_NAMES[i];
            String jname = mSBModel.unitChecker.getCanonicalName(sname);
            if (jname != null)
                mSBMLToJSim.put(sname, jname);
        }
    }            
 
    //Looks up on the hash map whether a JSim unit exists for a given SBML unit.  If so, it is returned, and if not returns null.
    public String toJSimFromSBML(String sbmlunit) {
        return mSBMLToJSim.get(sbmlunit);
    }

    //Here is where we figure out if there's a unit expression that we can use instead 
    //of mult*10^scale*jsimkind.  The return value must not start with a number, //
    //so it can be used as a unit expression (or as part of a unit expression).
    public String toUnitExpressionOLD(String jsimkind, double mult, int scale) {
        if (jsimkind==null) return null;
        mult = mult*Math.pow(10,scale);
        if (mult==1.0) return jsimkind;
        if (mult==3600 && jsimkind.equals("sec")) return "hour";
        if (mult==60 && jsimkind.equals("sec")) return "min";
        if (mult==0.000000001 && jsimkind.equals("mol")) return "nanomole";
        if (mult==0.000001 && jsimkind.equals("mol")) return "micromole";
        if (mult==0.001 && jsimkind.equals("mol")) return "millimole";
        return null;
    }

    public String toUnitExpression(String jsimkind, double mult, int scale) {
        double f = mult * Math.pow(10, scale);
        return mSBModel.unitChecker.getCanonicalName(jsimkind, f); 
    }

    //The idea here is to find stuff like 'moles/L' and convert it to 'M'.  We will only be asked about valid JSim expressions whose individual elements were simplified by the 'toUnitExpression' function, above.
    public String simplifyJSimOLD(String expression) {
        if (expression.equals("nanomole/L")) return "nanomolar";
        return expression;
    }

    public String simplifyJSim(String expr) {
        String sexpr = mSBModel.unitChecker.getCanonicalName(expr);
        if (sexpr == null || sexpr.length() > expr.length())
            return expr;
        return sexpr;
    }

    //Tells the converter that we have successfully created a new SBML/JSim unit for an existing JSim/SBML unit, and adds it to the lookup table.
    public void addSBMLJSimConversion(String sbml, String jsim) {
        mSBMLToJSim.put(sbml, jsim);
    }

    //There are a couple 'kind's that are not pre-defined in nsrunit, 
    // so we need to define them here.  If we want to allow writing out of 
    // jsim models that don't use nsrunit (which I would not recommend) we 
    // could define them here.
    public void addSBMLUnitDefinition(String kind)
        throws Xcept {
        int ikind = SBUnitConstants.stringToKind(kind);
        UnitDefinition ud = new UnitDefinition(2, 4);
        //Note:  this is actually an illegal id for a unit definition, so it's possible that a future version of libsbml will disallow it.
        int success = ud.setId(kind);
        if (success != org.sbml.libsbml.libsbmlConstants.LIBSBML_OPERATION_SUCCESS) {
            throw new Xcept("Error:  have to find a different way of setting the unit for this unit kind.");
        }
        Unit unit = new Unit(2, 4);
        switch(ikind) {
        case org.sbml.libsbml.libsbmlConstants.UNIT_KIND_AVOGADRO:
            unit.setMultiplier(6.02214179);
            unit.setScale(23);
            unit.setExponent(1.0);
            unit.setKind(org.sbml.libsbml.libsbmlConstants.UNIT_KIND_DIMENSIONLESS);
            ud.addUnit(unit);
            mSBModel.mExtraUnitDefinitions.add(ud);
            break;
        case org.sbml.libsbml.libsbmlConstants.UNIT_KIND_ITEM:
            unit.setMultiplier(1.0);
            unit.setScale(0);
            unit.setExponent(1.0);
            unit.setKind(org.sbml.libsbml.libsbmlConstants.UNIT_KIND_DIMENSIONLESS);
            ud.addUnit(unit);
            mSBModel.mExtraUnitDefinitions.add(ud);
            break;
        default:
            throw new Xcept("Cannot add unit definition for " + kind + ".");
        }
    }
}
