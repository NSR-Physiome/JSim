/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

package JSim.cellml;

import JSim.util.*;

import java.io.*;
import java.util.*;

public class CMLEqnSorter {
    private CMLEqn.List m_eqns;
    private ArrayList<CMLVarGroup> m_openvars;
    private LinkedHashMap<CMLEqn, CMLVarGroup.List> m_eqntovarsmap;
    private LinkedHashMap<CMLEqn, CMLVarGroup> m_eqntosolvedvarmap;
    private String m_warnings;
    private boolean m_overdetermined;

    // constructor
    public CMLEqnSorter(CMLComp.NList components, CMLVarGroup.List vars) throws Xcept {
        m_eqns = new CMLEqn.List();
        m_openvars = new ArrayList<CMLVarGroup>();
        m_eqntovarsmap = new LinkedHashMap<CMLEqn, CMLVarGroup.List>();
        m_eqntosolvedvarmap = new LinkedHashMap<CMLEqn, CMLVarGroup>();
        m_overdetermined = false;
        addEquations(components);
        addVars(vars);
        createMap();
    }

    //Store all of the equations from all of the components.
    private void addEquations(CMLComp.NList components) {
        for (int component=0; component<components.size(); component++) {
            m_eqns.addAll(components.comp(component).eqns);
        }
    }

    //Convert the passed-in CMLVarGroup.List to an ArrayList, ordering them so
    // that variables with initial values are at the end of the list.
    private void addVars(CMLVarGroup.List vars) {
        for (int var=0; var<vars.size(); var++) {
            CMLVarGroup cmlvar = vars.varGroup(var);
            //Put variables with initial values at the end of the list
            if (cmlvar.hasInitValue()) {
                m_openvars.add(cmlvar);
            }
            else {
                m_openvars.add(0,cmlvar);
            }
        }
    }

    //Construct a mapping between the algebraic rules and the variables they contain.
    private void createMap() {
        //System.out.println("alg eqs size " + m_eqns.size());
        for (int eq=0; eq<m_eqns.size(); eq++) {
            ArrayList<CMLVarGroup> usedvars = new ArrayList<CMLVarGroup>();
            //System.out.println("creating eq with open vars " + m_openvars.toString() );
            CMLEqn cmleq = m_eqns.get(eq);
            CMLVarGroup.List eqvars = cmleq.getAllVars(); //This is a new object that we now own.
            //System.out.println("matching vars list " + usedvars.toString());
            m_eqntovarsmap.put(cmleq, eqvars);
        }
    }

    //Sort through all the equations and figure out which variables they solve.
    public void mapEquationsToVariables() {
        //First remove any domains from any lists.
        ArrayList<CMLVarGroup> domains = new ArrayList<CMLVarGroup>();
        for (int v=0; v<m_openvars.size(); v++) {
            if (m_openvars.get(v).isDomain) {
                domains.add(m_openvars.get(v));
            }
        }
        for (int d=0; d<domains.size(); d++) {
            m_openvars.remove(domains.get(d));
            for (Map.Entry<CMLEqn, CMLVarGroup.List> subentry: m_eqntovarsmap.entrySet()) {
                subentry.getValue().remove(domains.get(d));
            }
        }

        //Create lists without the dx/dy equations, and simply claim that they all solve the 'x'.
        LinkedHashMap<CMLEqn, CMLVarGroup.List> evm_nodx  = copyE2LHashMap(m_eqntovarsmap);
        ArrayList<CMLVarGroup>                  ov_nodx   = new  ArrayList<CMLVarGroup>();
        ov_nodx.addAll(m_openvars);

        for (Map.Entry<CMLEqn, CMLVarGroup.List> entry: m_eqntovarsmap.entrySet()) {
            CMLVar deriv = entry.getKey().derivVar;
            if (deriv != null) {
                if (ov_nodx.contains(deriv.varGroup)) {
                    updateWithSolvedPair(entry.getKey(), deriv.varGroup, evm_nodx, ov_nodx, m_eqntosolvedvarmap);
                }
            }
        }
        //Save a copy of this for step two, below.
        LinkedHashMap<CMLEqn, CMLVarGroup>      esvm_dxonly = new LinkedHashMap<CMLEqn, CMLVarGroup>();
        esvm_dxonly.putAll(m_eqntosolvedvarmap);

        //Now create lists without other variables that have initial values:
        LinkedHashMap<CMLEqn, CMLVarGroup.List> evm_noinits  = copyE2LHashMap(evm_nodx);
        ArrayList<CMLVarGroup>                  ov_noinits   = new  ArrayList<CMLVarGroup>();
        ov_noinits.addAll(ov_nodx);

        for (int v=0; v<ov_nodx.size(); v++) {
            CMLVarGroup cmlvg = ov_nodx.get(v);
            if (cmlvg.hasInitValue() && !cmlvg.hasDeriv) {
                //Util.verbose("LS removing variable " + cmlvg.mmlName);                
                removeVarFrom(cmlvg, evm_noinits, ov_noinits);
            }
        }
        
        //Now sort through the rest of the equations and figure out a mapping that
        // relates one equation to one as-of-yet-unsolved variable.

        //First, try removing the most restrictive set:  no dx/dy, no initial values:
        //Util.verbose("LS Starting sorting algorithm without dx/dy or initial values.  Open variables:");
        for (int ov=0; ov<ov_noinits.size(); ov++) {
            Util.verbose("    " + ov_noinits.get(ov).mmlName);
        }
        //Util.verbose("LS Open equations: (" + evm_noinits.size() + " total)");
        for (Map.Entry<CMLEqn, CMLVarGroup.List> entry: evm_noinits.entrySet()) {
            //Util.verbose("    " + entry.getKey().toString());
        }
        if (sortEqs(ov_noinits, evm_noinits, m_eqntosolvedvarmap)) {
            //It worked--make this version official.
            //Util.verbose("LS Worked without overriding dx/dy mappings or initial values.");
            m_eqntovarsmap.clear();
            m_openvars = ov_noinits;
            return;
        }

        //If that didn't work, try the next least restrictive set:  no dx/dy, but everything else:
        m_eqntosolvedvarmap.clear();
        m_eqntosolvedvarmap.putAll(esvm_dxonly);
        //Util.verbose("LS Re-starting sorting algorithm without dx/dy only.  Open variables:");
        for (int ov=0; ov<ov_nodx.size(); ov++) {
            //Util.verbose("    " + ov_nodx.get(ov).mmlName);
        }
        //Util.verbose("LS Open equations: (" + evm_nodx.size() + " total)");
        for (Map.Entry<CMLEqn, CMLVarGroup.List> entry: evm_nodx.entrySet()) {
            //Util.verbose("    " + entry.getKey().toString());
        }
        if (sortEqs(ov_nodx, evm_nodx, m_eqntosolvedvarmap)) {
            //It worked--make this version official.
            //Util.verbose("LS Worked without overriding dx/dy mappings but did have to override some initial values.");
            m_eqntovarsmap = copyE2LHashMap(evm_nodx);
            m_openvars = ov_nodx;
            return;
        }

        //If *that* didn't work, try throwing absolutely everything into the pot:
        m_eqntosolvedvarmap.clear();
        //Util.verbose("LS Re-starting sorting algorithm with everything included.  Open variables:");
        for (int ov=0; ov<ov_nodx.size(); ov++) {
            //Util.verbose("    " + ov_nodx.get(ov).mmlName);
        }
        //Util.verbose("LS Open equations:");
        for (Map.Entry<CMLEqn, CMLVarGroup.List> entry: evm_nodx.entrySet()) {
            //Util.verbose("    " + entry.getKey().toString());
        }
        if (sortEqs(m_openvars, m_eqntovarsmap, m_eqntosolvedvarmap)) {
            //It worked--this version is already official because the m_ variables were used.
            //Util.verbose("LS Worked, but only with overriding one or more dx/dy and possibly other initial values.");
            return;
        }

        //If that didn't work, give up.  There's no solution to this problem.
        m_overdetermined = true;
        //Util.verbose("LS Warning: The original CellML model was overdetermined.");
        String od = "//Warning: The original CellML model was overdetermined.\n";
        if (m_warnings==null) m_warnings = od;
        else m_warnings = m_warnings + "\n" + od;
    }

    //Go through all variables, adding appropriate domains to all of them, and setting
    // any that are unsolved to be 'extern'.
    public void classifyVariables() {
        //m_eqntosolvedvarmap is in its final form, so now we can go through the
        // equations and tell each of them which variable they solve:
        for (Map.Entry<CMLEqn, CMLVarGroup> entry: m_eqntosolvedvarmap.entrySet()) {
            CMLEqn eqn = entry.getKey();
            CMLVarGroup var = entry.getValue();
            //Util.verbose("LS equation " + eqn.toString() + "\n     solves " + var.mmlName);
            eqn.solvedVarGroup = entry.getValue();
            var.setIsSolvedByEqn(true);
            if (eqn.solvesDeriv()) {
                var.solvedByDeriv = true;
            }
        }

        if (m_overdetermined) {
            //We'll assume that all variables are solved by *something* and don't need
            // to be set 'extern'.  JSim can't run the model anyway, but it might as well
            // be as close to the original problem model as possible.
            for (int v=0; v<m_openvars.size(); v++) {
                CMLVarGroup cmlvg = m_openvars.get(v);
                cmlvg.setIsSolvedByEqn(true);
                if (cmlvg.hasDeriv) {
                    cmlvg.solvedByDeriv = true;
                }
            }
        }

        //The equations now know enough to set the domains of their solved variables:
        boolean updated = true;
        while (updated) {
            updated = false;
            for (int eq=0; eq<m_eqns.size(); eq++) {
                if (m_eqns.get(eq).expandSolvedDomains()) {
                    updated = true;
                }
            }
        }
    }

    public String getWarnings() {
        return m_warnings;
    }

    //The heart of the class:  match implicit equations with variables they solve, and remove both from the pending list until there are no more implicit equations left.  The removed variables will be marked 'solved'.
    private boolean sortEqs(ArrayList<CMLVarGroup>                  openvars,
                            LinkedHashMap<CMLEqn, CMLVarGroup.List> eqntovarsmap,
                            LinkedHashMap<CMLEqn, CMLVarGroup>      eqntosolvedvarmap)  {
        //Util.verbose("LS Number of solved equations:  " + eqntosolvedvarmap.size() );
        //Check to see if we're done and don't have any more equations to sort:
        if (eqntovarsmap.size()==0) return true;

        //Check for equations that map to no variables, which would mean we made a bad choice earlier.
        for (Map.Entry<CMLEqn, CMLVarGroup.List> entry: eqntovarsmap.entrySet()) {
            if (entry.getValue().size()==0) {
                //Util.verbose("LS That didn't work!  This equation now has no variables it can solve:  " + entry.getKey().toString());
                return false;
            }
        }

        //Find a equation that maps to a single unsolved variable if possible.  Otherwise, find the equation that
        // maps to the smallest number of unsolved variables.
        Map.Entry<CMLEqn, CMLVarGroup.List> smallest = null;
        for (Map.Entry<CMLEqn, CMLVarGroup.List> entry: eqntovarsmap.entrySet()) {
            if (smallest == null) smallest = entry;
            if (entry.getValue().size()==1) {
                //Util.verbose("LS Note: only " + entry.getValue().get(0).mmlName + " can now be solved by " + entry.getKey().toString());
                CMLVarGroup var = entry.getValue().get(0);
                updateWithSolvedPair(entry.getKey(), var, eqntovarsmap, openvars, eqntosolvedvarmap);
                return sortEqs(openvars, eqntovarsmap, eqntosolvedvarmap);
            }
            else if (entry.getValue().size() < smallest.getValue().size()) smallest = entry;
        }

        //Now pick an unsolved variable and see what happens if we use the equation to solve it.
        CMLVarGroup.List possiblevars = smallest.getValue();
        for (int vnum=0; vnum<possiblevars.size(); vnum++) {
            LinkedHashMap<CMLEqn, CMLVarGroup.List> evm_copy =  copyE2LHashMap(eqntovarsmap);
            ArrayList<CMLVarGroup>                  ov_copy =   new  ArrayList<CMLVarGroup>(openvars);
            LinkedHashMap<CMLEqn, CMLVarGroup>      esvm_copy = copyE2VHashMap(eqntosolvedvarmap);
            CMLVarGroup var = possiblevars.get(vnum);
            updateWithSolvedPair(smallest.getKey(), var, evm_copy, ov_copy, esvm_copy);
            if (sortEqs(ov_copy, evm_copy, esvm_copy)) {
                openvars.removeAll(openvars);
                openvars.addAll(ov_copy);
                eqntovarsmap.clear();
                eqntovarsmap.putAll(evm_copy);
                eqntosolvedvarmap.clear();
                eqntosolvedvarmap.putAll(esvm_copy);
                //eqntovarsmap.size() should equal zero here.
                return true;
            }
            //else: that didn't work; move on to the next variable with new copies.
            //Util.verbose("LS That didn't work!  Moving on to the next variable for equation " + smallest.getKey().toString());
        }

        //If we reach here, it means that there are no open variables that this equation can solve.  
        return false;
    }

    private void updateWithSolvedPair(CMLEqn eqn, CMLVarGroup solved,
                                      LinkedHashMap<CMLEqn, CMLVarGroup.List> eqntovarsmap,
                                      ArrayList<CMLVarGroup> openvars,
                                      LinkedHashMap<CMLEqn, CMLVarGroup> eqntosolvedvarmap)
    {
        //Util.verbose("LS Trying:  solving " + solved.mmlName + " with the equation " + eqn.toString());                
        //Add the pair to the 'solved' list:
        eqntosolvedvarmap.put(eqn, solved);

        //Remove the variable from the 'open' list:
        openvars.remove(solved);

        //Remove the equation from the equation/vars map:
        eqntovarsmap.remove(eqn);

        //Remove the variable from all the 'unsolved variables' lists of the equation/vars maps:
        for (Map.Entry<CMLEqn, CMLVarGroup.List> subentry: eqntovarsmap.entrySet()) {
            subentry.getValue().remove(solved);
        }
    }

    private void removeVarFrom(CMLVarGroup toremove,
                               LinkedHashMap<CMLEqn, CMLVarGroup.List> eqntovarsmap,
                               ArrayList<CMLVarGroup> openvars)
    {
        //Remove the variable from the 'open' list:
        openvars.remove(toremove);

        //Remove the variable from all the 'unsolved variables' lists of the equation/vars maps:
        for (Map.Entry<CMLEqn, CMLVarGroup.List> subentry: eqntovarsmap.entrySet()) {
            subentry.getValue().remove(toremove);
        }
        
    }

    private  LinkedHashMap<CMLEqn, CMLVarGroup.List> copyE2LHashMap( LinkedHashMap<CMLEqn, CMLVarGroup.List> original) {
        LinkedHashMap<CMLEqn, CMLVarGroup.List> copy =  new  LinkedHashMap<CMLEqn, CMLVarGroup.List>();
        for (Map.Entry<CMLEqn, CMLVarGroup.List> subentry: original.entrySet()) {
            CMLEqn eqn = subentry.getKey();
            CMLVarGroup.List listcopy = new CMLVarGroup.List();
            listcopy.addAll(subentry.getValue());
            copy.put(eqn, listcopy);
        }
        return copy;
    }

    private  LinkedHashMap<CMLEqn, CMLVarGroup> copyE2VHashMap( LinkedHashMap<CMLEqn, CMLVarGroup> original) {
        LinkedHashMap<CMLEqn, CMLVarGroup> copy =  new  LinkedHashMap<CMLEqn, CMLVarGroup>();
        for (Map.Entry<CMLEqn, CMLVarGroup> subentry: original.entrySet()) {
            copy.put(subentry.getKey(), subentry.getValue());
        }
        return copy;
    }
}
