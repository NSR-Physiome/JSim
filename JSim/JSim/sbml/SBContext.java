/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML context for writing MML expressions

package JSim.sbml;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class SBContext extends Context {
    private SBModel model;
    
    // constructor
    public SBContext(SBModel m) {
        super(JSLang.lang);
        model = m;
    }
    
    // MML-friendly name for variable
    public String newName(Named n) {
        String name = n.name();
        try {
            SBVar v = model.sbvars.sbvar(name);
            if (v != null) name = v.mmlName;
        } catch (Xcept e) {
        }
        return name;
    }

    //  function Call 
    public String funcCall(Named n, Expr.List elist) {
        return newName(n) + elist.toString(this);
    }

    // unit cast
    public String unitCast(UnitCast cast) {
        return "(" + cast.expr().toString(this) + " unit " + 
            cast.unit().pubName() + ")";
    }
}

        
