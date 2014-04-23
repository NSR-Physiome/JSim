/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML parameter for import

package JSim.sbml;

import JSim.util.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBParameter implements Named {
    protected SBModel sbmodel;
    protected Parameter parm;
    protected SBVar var;

    // constructor
    public SBParameter(SBModel sm, Parameter p) throws Xcept {
        sbmodel = sm;
        parm = p;
        var = new SBVar(sbmodel, name(), "parameter");
        if (parm.isSetConstant() && parm.getLevel() != 1) {
            var.setConst(parm.getConstant());
        }
        if (parm.isSetValue()) {
            var.setInitValue(parm.getValue());
        }
        InitialAssignment ia = sm.model.getInitialAssignment(p.getId());
        if (ia != null) {
            var.setInitValue(ia.getMath());
        }
        var.setUnit(parm.getUnits());
     }
    
    // query
    public String name() { return parm.getId(); }
    public String diagInfo() { return "Parameter " + name(); }

    // write MML
    public void writeMML(PrintWriter wrt) {
    }

    // SBParameter.List
    public static class NList extends NamedList {
        public NList() { super(); }
        public NList(SBModel sbm) throws Xcept {
            super();
            addList(sbm, sbm.model.getListOfParameters());
        }
        public void addList(SBModel sbm, ListOfParameters paramlist) throws Xcept {
            long nparams = paramlist.size();
            for (int i=0; i<nparams; i++) {
                add(new SBParameter(sbm, paramlist.get(i)));
            }
        }
        public SBParameter sbparm(int i) {
            return (SBParameter) get(i);
        }
        public SBParameter sbparm(String n) {
            return (SBParameter) getByName(n);
        }
    }
}
