/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML import utilites targeting specific JSim unit space

package JSim.sbml;

import java.io.File;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

public class SBUnitChecker implements NameSpace {
    private UnitNList units;

    // constructor
    public SBUnitChecker(UnitNList units) { 
        this.units = units;
    }

    // is this name defined?
    public boolean isDefined(String name) throws Xcept {
        try {
            Unit.parse(this, "1 " + name);
            return true;
        } catch (Xcept e) {
            return false;
        }
    }

    // returns true if sbml unit name has conflicting JSim common
    // definition with one provided
    // NOTE: return false for malformed jsimDef 
    public boolean isConflict(String sbmlName, String jsimDef) 
        throws Xcept {
        try {
            Unit usbml = Unit.parse(this, "1 " + sbmlName);
            Unit udef = Unit.parse(this, jsimDef);
            return ! Unit.same(usbml, udef);
        } catch (Xcept e) {
            return false;
       }
    }

    // is unit name legal in JSim?
    public boolean isLegalName(String name) {
    	if (Util.isBlank(name)) 
	    return false;
	char c = name.charAt(0);
	return Character.isLetter(c);
    }
    
    // get canonical name for unit expr,  or null
    public String getCanonicalName(String expr)  {        
        if (Util.isBlank(expr)) return null;
        try {
            Unit u = Unit.parse(this, "1 " + expr);
            return getCanonicalName(u);
        } catch (Xcept e) {
            return null;
        }
    }

    // get shortest unit for multiplied unit expr, or null
    public String getCanonicalName(String expr, double mult) {
        try {
            Unit u = Unit.parse(this, ""  + mult + " " + expr);
            return getCanonicalName(u);
        } catch (Xcept e) {
            return null;
        }
    }  
    
    // get shortest name for given unit    
    // to optimize: create unit lists by dims hash code for faster search
    private String getCanonicalName(Unit u) throws Xcept {
        if (Unit.same(u, Unit.scalar()))
            return Unit.dimless;
        String name = null;
        for (int i=0; i<units.size(); i++) {
            String n = getPrefixedName(u, units.unit(i));
            if (n == null) continue;
            if (name == null || n.length() < name.length())
                name = n;
        }
        return name;
    }

    // return possibly prefixed version of u2 equivalent to u1, or null
    // to optimize: create prefix lookup Hashtable
    private String getPrefixedName(Unit u1, Unit u2) {
        if (Unit.same(u1, u2)) return u2.name();
        if (! units.isPrefixable(u2.name())) return null;
        if (u2.name().length() == 1) return null; // hack to prevent no femtoA, microN
        if (! Unit.compatible(u1, u2)) return null;
        double f = u1.f / u2.f;
        String pfx = units.getPrefix(f);
        if (pfx == null) return null;
        return pfx + u2.name();
    }        

    //  NameSpace methods
    public Unit unitByName(String name) throws Xcept {
        return units.byName(name);
    }
    public Expr compByName(String name) throws Xcept {
        throw new Xcept("SBUnitChecker: compByName not implemented");
    }
    public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
        throw new Xcept("SBUnitChecker: makeDeriv not implemented");
    }
    public Expr funcCall(String name, Expr.List elist) throws Xcept {
        throw new Xcept("SBUnitChecker: funcCall not implemented");
    }

    // test harness has been moved to JSim.tests.TestSBMLUnits
}
