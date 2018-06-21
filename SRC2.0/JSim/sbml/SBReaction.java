/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML reaction for import

package JSim.sbml;

import JSim.util.*;

import java.io.*;
import java.util.*;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBReaction implements Named {
    protected SBModel sbmodel;
    protected Reaction reac;
    protected SBVar vrate;
    protected String eqn; // chem eqn
	protected String notes;
    public int nstoichs;
    protected ArrayList<Stoich> stoichs; // product stoich

    // constructor
    public SBReaction(SBModel sm, Reaction c) throws Xcept {
        sbmodel = sm;
        reac = c;
		if (reac.isSetNotes()) {
				SBNotes newNote = new SBNotes(reac.getNotesString());
				newNote.removeXMLTags();
				newNote.addCommentIdentifiers();
				this.notes = new String(newNote.getNote());
			}
    	else { this.notes = new String(""); }


        // load stoichiometry
        stoichs = new ArrayList<Stoich>();
        for (int i=0; i<reac.getNumReactants(); i++) {
            addStoich(reac.getReactant(i), false);
        }
        for (int i=0; i<reac.getNumProducts(); i++) {
            addStoich(reac.getProduct(i), true);
        }
        nstoichs = stoichs.size();
        // build eqn
        eqn = "";
        String eq = (reac.getReversible()) ? "=>" : "<=>";
        for (int i=0; i<nstoichs; i++) {
            if (eq != null && isProduct(i)) {
                eqn = eqn + eq + " ";
                eq = null;
            }
            if (! mult(i).equals("1"))
                eqn = eqn + mult(i);
            eqn = eqn + spec(i) + " ";
        }        

        // rate variable & assignment
        vrate = new SBVar(sbmodel, name(), "rate");
        vrate.setConst(false);
        KineticLaw law = reac.getKineticLaw();
        String rate = (law == null) ?
            makeKRate() : makeMathRate(law);
		if (law.isSetNotes()) {
				SBNotes newNote = new SBNotes(law.getNotesString());
				newNote.removeXMLTags();
				newNote.addCommentIdentifiers();
				vrate.setNotes(newNote.getNote());
			}
   
		if(!this.notes.equals("")) {rate = rate.concat(this.notes);} // << this should work ok
        vrate.setAssign(rate);
        String sunit = "substance";
        String tunit = "time";
        SBUnitSpace ucv = sbmodel.getSBUnitSpace();
        String jsunit = ucv.toJSimFromSBML(sunit);
        if (jsunit != null) sunit = jsunit;
        String jtunit = ucv.toJSimFromSBML(tunit);
        if (jtunit != null) tunit = jtunit;
        vrate.setUnit(sunit + "/" + tunit);

        // add deltas to each ODE
        for (int i=0; i<nstoichs; i++) {
            String mult = mult(i);
            if (!isProduct(i)) mult = "-" + mult;
            mult = (mult.equals("1")) ? "" : (mult + "*");
            String delta =  mult + vrate.mmlName;
            //System.out.println("Adding delta " + delta + " to " + vamt(i).name());
            vamt(i).addODEDelta(delta);
        }
        if (c.isSetFast() && c.getFast()) {
            sm.addWarning("Warning:  The SBML reaction '" + name() + "' was a 'fast reaction' in the original model.  This concept is not currently translatable to JSim, and simulation results may differ from expectations as a result.");
        }
    }
    
    // add stoichiometry in one list
    private void addStoich(SpeciesReference sref, boolean isProduct) throws Xcept {
        String spec = sref.getSpecies();
        SBSpecies sbspec = sbmodel.sbspecs.sbspec(spec);
        if (sbspec == null) throw new Xcept("Reaction " + name() + ": invalid species " + spec);

        // get stoichiometry multiplier
        String mult = "1";
        Model sbml = sbmodel.document.getModel();
        if (sref.isSetId() && !sref.getId().equals("")) {
            //If the species reference has an ID, we're going to assume it's because it's being used in math somewhere, either as the target of a rule or being used within a rule.
            SBVar srvar = new SBVar(sbmodel, sref.getId(), "species reference");
            if (sref.isSetConstant()) {
                srvar.setConst(sref.getConstant());
            }
            InitialAssignment ia = sbmodel.model.getInitialAssignment(sref.getId());
            if (ia != null) {
                srvar.setInitValue(ia.getMath());
				if (ia.isSetNotes()) {
					SBNotes newNote = new SBNotes(ia.getNotesString());
					newNote.removeXMLTags();
					newNote.addCommentIdentifiers();
					srvar.setNotes(newNote.getNote());
				}


            }
            else if (sref.isSetStoichiometryMath()) {
                //Note:  'stoichiometryMath' is only used in L2 models, which cannot
                // additionally have assignment rules or initial assignments to the
                // species reference ID.  However, it's possible that someone might
                // want this variable as *output*.
                String text = libsbml.writeMathMLToString(sref.getStoichiometryMath().getMath());
                String rhs = sbmodel.mathExprMML(text);
                srvar.setAssign(rhs);
            }
            else if (sref.isSetStoichiometry()) {
                srvar.setInitValue(sref.getStoichiometry());
            }
            else {
                srvar.setInitValue(1);
            }
            mult = sref.getId();
        }
        else if (sref.isSetStoichiometryMath()) {
            String math = libsbml.writeMathMLToString(sref.getStoichiometryMath().getMath());
            mult = "(" + sbmodel.mathExprMML(math) + ")";
        }
        else if (sref.isSetStoichiometry()) {
            mult = Util.pretty(sref.getStoichiometry());
            if (sbmodel.document.getLevel()==1 && sref.getDenominator() != 1) {
                mult = "(" + mult + "/" + Util.pretty(sref.getDenominator()) + ")";
            }
        }
        // create Stoich table entry
        Stoich stoich = new Stoich();
        stoich.vamt = sbspec.vspec;
        stoich.spec = spec;
        stoich.mult = mult;
        stoich.isProduct = isProduct;
        stoichs.add(stoich);
    }

    // create math based rate
    private String makeMathRate(KineticLaw law) throws Xcept {
        for (int p=0; p<law.getNumParameters(); p++) {
            Parameter localparam = law.getParameter(p);
            String lpname = localparam.getId();
            if (sbmodel.getSBVar(lpname) != null) {
                String localname = vrate.mmlName + "_" + lpname;
                int num = 1;
                while (sbmodel.getSBVar(localname)!=null) {
                    localname = vrate.mmlName + "_" + lpname + num;
                    num++;
                }
                localparam.setId(localname);
                ASTNode astn = renameIDs(law.getMath(), lpname, localname);
                law.setMath(astn);
            }
            sbmodel.sbparms.add(new SBParameter(sbmodel, localparam));
        }
        sbmodel.sbparms.addList(sbmodel, law.getListOfParameters());
        //LS DEBUG:  Does not rename these parameters; there might be overlap of namespace issues.
        ASTNode astn = law.getMath();
        String ret = sbmodel.mathExprMML(libsbml.writeMathMLToString(astn));
        return ret;
    }

    //Rename the IDs found in this ASTNode (used to rename local parameters to make them unique
    private ASTNode renameIDs(ASTNode astn, String oldname, String newname) {
        String name = astn.getName();
        if (name != null && name.equals(oldname)) {
            astn.setName(newname);
        }
        for (int child=0; child<astn.getNumChildren(); child++) {
            renameIDs(astn.getChild(child), oldname, newname);
        }
        return astn;
    }
    
    // create kf/kb based rate law
    private String makeKRate() throws Xcept {
        SBVar kf = new SBVar(sbmodel, name() + ".kf", "rate");
        kf.setConst(true);
        SBVar kb = new SBVar(sbmodel, name() + ".kb", "rate");
        kb.setConst(true);
        String krate = "" + kf;
        String kprod = " - " + kb;
        for (int i=0; i<nstoichs; i++) {
            if (kprod != null && isProduct(i)) {
                krate = krate + kprod;
                kprod = null;
            }
            krate = krate + "*" + vamt(i);
            if (! mult(i).equals("1"))
                krate = krate + "^" + mult(i);
        }
        return krate;
    }        

    // query
    public String name() { return reac.getId(); }
    public String diagInfo() { return "Reaction " + name(); }
    public Stoich stoich(int i) { return (Stoich) stoichs.get(i); }
    public SBVar vamt(int i) { return stoich(i).vamt; }
    public String spec(int i) { return stoich(i).spec; }
    public String mult(int i) { return stoich(i).mult; }
    public boolean isProduct(int i) { return stoich(i).isProduct; }

    // write MML
    public void writeMML(PrintWriter wrt) {
        wrt.println("// " + name() + ": " + eqn+" "+ this.notes);
    }

    // Stoich class
    public static class Stoich {
        public SBVar vamt; // amount var for species
        public String spec;  // user name of species
        public String mult;
        public boolean isProduct;
    }

    // SBReaction.List
    public static class NList extends NamedList {
        public NList(SBModel sbm) throws Xcept {
            super();
            long nrxns = sbm.model.getNumReactions();
            for (int i=0; i<nrxns; i++) {
                add(new SBReaction(sbm, sbm.model.getReaction(i)));
            }
        }
        public NList() { super(); }
        public SBReaction sbreac(int i) {
            return (SBReaction) get(i);
        }
        public SBReaction sbreac(String n) {
            return (SBReaction) getByName(n);
        }
    }
}
