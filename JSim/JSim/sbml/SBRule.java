/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML rule for import

package JSim.sbml;

import JSim.util.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBRule implements Named {
    protected SBModel sbmodel;
    protected Rule rule;
    protected String aexpr;

    // constructor
    public SBRule(SBModel sm, Rule r) throws Xcept {
        sbmodel = sm;
        rule = r;

        String text = libsbml.writeMathMLToString(rule.getMath());
        String rhs = sbmodel.mathExprMML(text);
        
        if (rule instanceof AssignmentRule) {
            SBVar v = sbmodel.sbvars.sbvar(rule.getVariable());
            v.setAssign(rhs);
        } else if (rule instanceof AlgebraicRule) {
            aexpr = rhs;
        } else if (rule instanceof RateRule) {
            SBVar v = sbmodel.sbvars.sbvar(rule.getVariable());
            v.setODE(rhs);
        }    

    }
    
    // query
    public String name() { return "" + hashCode(); }
    public String diagInfo() { return "Rule " + name(); }
    public boolean isAlgebraic() {return (rule instanceof AlgebraicRule);}
    public boolean hasVar(SBVar var) {
        ASTNodeList astnl = rule.getMath().getListOfNodes();
        for (long node=0; node<astnl.getSize(); node++) {
            //System.out.println("Node " + node + "node name '" + astnl.get(node).getName() + "', varname '" + var.name() + "'");
            String nodename = astnl.get(node).getName();
            if (nodename != null && nodename.equals(var.name())) {
                //System.out.println("Match!");
                return true;
            }
        }
        return false;
    }

    // write MML
    public void writeMML(PrintWriter wrt) {
        if (aexpr == null) return;
        wrt.println("  " + aexpr + " = 0;");
    }

    // SBRule.List
    public static class NList extends NamedList {
        public NList(SBModel sbm) throws Xcept {
            super();
            long nrules = sbm.model.getNumRules();
            for (int i=0; i<nrules; i++) {
                add(new SBRule(sbm, sbm.model.getRule(i)));
            }
        }
        public NList() { super(); }
        public SBRule sbrule(int i) {
            return (SBRule) get(i);
        }
        public SBRule sbrule(String n) {
            return (SBRule) getByName(n);
        }
    }
}
