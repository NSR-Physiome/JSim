/*NSRCOPYRIGHT
  Copyright (C) 1999-2008 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML constraint for import

package JSim.sbml;

import JSim.util.*;
import JSim.mathml.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBConstraint implements Named {
    protected SBModel sbmodel;
    protected Constraint constraint;
    protected String expr;

    // constructor
    public SBConstraint(SBModel sm, Constraint c) throws Xcept {
        sbmodel = sm;
        constraint = c;

        String xml = libsbml.writeMathMLToString(constraint.getMath());
        expr = sbmodel.mathExprMML(xml);
        //System.err.println(expr);
    }
    
    // query
    public String name() { return "" + hashCode(); }
    public String diagInfo() { return "Constraint " + name(); }

    // write MML
    public void writeMML(PrintWriter wrt) {
        if (expr == null) return;
        wrt.println("  " + expr + ";");
    }

    // SBConstraint.List
    public static class NList extends NamedList {
        public NList(SBModel sbm) throws Xcept {
            super();
            long nconstraints = sbm.model.getNumConstraints();
            for (int i=0; i<nconstraints; i++) {
                add(new SBConstraint(sbm, sbm.model.getConstraint(i)));
            }
        }
        public NList() { super(); }
        public SBConstraint sbconstraint(int i) {
            return (SBConstraint) get(i);
        }
        public SBConstraint sbconstraint(String n) {
            return (SBConstraint) getByName(n);
        }
    }
}
