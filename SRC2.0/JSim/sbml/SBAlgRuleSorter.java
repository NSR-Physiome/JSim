/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// find out which vars are solved by algebraic rules on SBML import

package JSim.sbml;

import JSim.util.*;

import java.io.*;
import java.util.*;
import org.sbml.libsbml.*;

public class SBAlgRuleSorter {
    protected ArrayList<SBRule> m_algrules;
    protected ArrayList<SBVar> m_openvars;
    protected LinkedHashMap<SBRule, ArrayList<SBVar> > m_ruletovarsmap;
    protected String m_warnings;

    // constructor
    public SBAlgRuleSorter(SBRule.NList rules, SBVar.NList vars) throws Xcept {
        m_algrules = new ArrayList<SBRule>();
        m_openvars = new ArrayList<SBVar>();
        m_ruletovarsmap = new LinkedHashMap<SBRule, ArrayList<SBVar> >();
        addRules(rules);
        addVars(vars);
        createMap();
    }

    //Go through all variables and set them as either 'value' or 'implicit'
    public void classifyVars()
        throws Xcept {
        LinkedHashSet<SBVar> openvars = new LinkedHashSet<SBVar>();
        //Start with all possible variables:
        openvars.addAll(m_openvars);
        //Now go through and remove the ones that are determined by our algebraic rules:
        boolean check = sortRules(openvars, m_ruletovarsmap);
        if (!check) {
            String od = "Warning: The original SBML model was overdetermined.\n";
            if (m_warnings==null) m_warnings = od;
            else m_warnings = m_warnings + "\n" + od;
        }
        for (int var=0; var<m_openvars.size(); var++) {
            SBVar sbvar = m_openvars.get(var);
            if (!openvars.contains(sbvar)) {
                sbvar.setImplicit();
            }
        }
    }

    public String getWarnings() {
        return m_warnings;
    }

    //Find the algebraic rules in the rule list and store them.
    private void addRules(SBRule.NList rules) {
        for (int rule=0; rule<rules.size(); rule++) {
            if (rules.sbrule(rule).isAlgebraic()) {
                m_algrules.add(rules.sbrule(rule));
            }
        }
    }

    //Find all unsolved variables and store them, organizing the list such that those without initial values come first.  This way, if we have a choice of which variable to tag 'value' vs. 'implicit', the variable with no initial value is more likely to be tagged 'implicit'.
    private void addVars(SBVar.NList vars) {
        for (int var=0; var<vars.size(); var++) {
            SBVar sbvar = vars.sbvar(var);
            //Variables that are known to vary from some other method (being involved in reactions, having
            // a rate rule, etc.) are not allowed to be solved by algebraic rules.  Neither are variables
            // flagged 'constant'.
            if (!sbvar.hasKnownVariance() && !sbvar.isConst) {
                //Put variables with initial values at the end of the list.
                if (sbvar.hasInitValue()) {
                    m_openvars.add(sbvar);
                }
                else {
                    m_openvars.add(0,sbvar);
                }
            }
        }
    }

    //Construct a mapping between the algebraic rules and the variables they contain.
    private void createMap() {
        //System.out.println("alg rules size " + m_algrules.size());
        for (int rule=0; rule<m_algrules.size(); rule++) {
            ArrayList<SBVar> usedvars = new ArrayList<SBVar>();
            //System.out.println("creating rule with open vars " + m_openvars.toString() );
            SBRule sbrule = m_algrules.get(rule);
            for (int var=0; var<m_openvars.size(); var++) {
                SBVar sbvar = m_openvars.get(var);
                if (sbrule.hasVar(sbvar)) {
                    usedvars.add(sbvar);
                }
            }
            //System.out.println("matching vars list " + usedvars.toString());
            m_ruletovarsmap.put(sbrule, usedvars);
        }
    }

    //The heart of the class:  match algebraic rules with variables they solve, and remove both from the pending list until there are no more algebraic rules left.  The removed variables will be marked 'implicit', the remaining ones 'value'.
    private boolean sortRules(LinkedHashSet<SBVar> openvars, LinkedHashMap<SBRule, ArrayList<SBVar> > ruletovarsmap) 
        throws Xcept {
        //System.out.println("Calling sortrules");
        if (ruletovarsmap.size()==0) return true;
        //Check for rules that map to no variables
        for (Map.Entry<SBRule, ArrayList<SBVar> > entry: ruletovarsmap.entrySet()) {
            if (entry.getValue().size()==0) {
                //System.out.println("Found a rule that maps to nothing.");
                return false;
            }
        }
        //Find a rule that maps to a single variable
        Map.Entry<SBRule, ArrayList<SBVar> > smallest = null;
        for (Map.Entry<SBRule, ArrayList<SBVar> > entry: ruletovarsmap.entrySet()) {
            if (smallest == null) smallest = entry;
            if (entry.getValue().size()==1) {
                //System.out.println("Found a map of size 1");
                SBVar var = entry.getValue().get(0);
                ruletovarsmap.remove(entry.getKey());
                openvars.remove(var);
                for (Map.Entry<SBRule, ArrayList<SBVar> > subentry: ruletovarsmap.entrySet()) {
                    subentry.getValue().remove(var);
                }
                return sortRules(openvars, ruletovarsmap);
            }
            else if (entry.getValue().size() < smallest.getValue().size()) smallest = entry;
        }
        //Pick a rule/variable pair and try it
        if (smallest == null) {
            throw new Xcept("Programming error:  algorithm to calculate determinedness for algebraic rules failed to find any rule/variable combinations even though there were indeed rules.  This should be impossible.");
        }
        ArrayList<SBVar> possiblevars = smallest.getValue();
        for (int vnum=0; vnum<possiblevars.size(); vnum++) {
            LinkedHashMap<SBRule, ArrayList<SBVar> > rvm_copy = new  LinkedHashMap<SBRule, ArrayList<SBVar> >(ruletovarsmap);
            LinkedHashSet<SBVar> ov_copy = new  LinkedHashSet<SBVar>(openvars);
            SBVar var = possiblevars.get(vnum);
            rvm_copy.remove(smallest.getKey());
            ov_copy.remove(var);
            for (Map.Entry<SBRule, ArrayList<SBVar> > subentry: rvm_copy.entrySet()) {
                subentry.getValue().remove(var);
            }
            if (sortRules(ov_copy, rvm_copy)) {
                openvars.removeAll(openvars);
                openvars.addAll(ov_copy);
                ruletovarsmap.clear();
                ruletovarsmap.putAll(rvm_copy);
                //ruletovarsmap.size() should equal zero here.
                return true;
            }
            //else: that didn't work; move on to the next variable with new copies.
        }
    
        return false;
    }

  
}
