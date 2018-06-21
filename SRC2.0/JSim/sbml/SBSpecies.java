/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML species for import

package JSim.sbml;

import JSim.util.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBSpecies implements Named {
    protected SBModel sbmodel;
    protected Species spec;
    protected boolean isSubstanceOnly;
    protected SBVar vspec;

    // constructor
    public SBSpecies(SBModel sm, Species s) throws Xcept {
        sbmodel = sm;
        spec = s;
        SBUnitSpace ucv = sm.getSBUnitSpace();

        // units and name pre-processing
        String sunits = null;
        if (s.isSetSubstanceUnits()) {
            sunits = spec.getSubstanceUnits();
        }
        else {
            sunits = "substance";
        }
        String jsunits = ucv.toJSimFromSBML(sunits);
        if (jsunits != null) sunits = jsunits;
        String volunits = sbcomp().unit();
        if (volunits.equals("")) {
            volunits = "volume";
        }
        String jvolunits = ucv.toJSimFromSBML(volunits);
        if (jvolunits != null) volunits = jvolunits;
        String cunits = sunits + "/" + volunits;
        isSubstanceOnly = spec.getHasOnlySubstanceUnits();
        if (sbcomp().spatialDimensions() == 0) {
            isSubstanceOnly = true;
        }
        vspec = new SBVar(sbmodel, name(), "species");
        if (isSubstanceOnly) {
            vspec.setUnit(sunits);
        }
        else {
            vspec.setUnit(cunits);
        }
        if (spec.isSetConstant()) {
            vspec.setConst(spec.getConstant());
        }
        vspec.setBC(spec.getBoundaryCondition());
        vspec.setIsSubstanceOnly(isSubstanceOnly);
        String compNameMML = sbcomp().v.mmlName;
        vspec.compartment = compNameMML;

		if (s.isSetNotes()) {
				SBNotes newNote = new SBNotes(s.getNotesString());
				newNote.removeXMLTags();
				newNote.addCommentIdentifiers();
				vspec.setNotes(newNote.getNote());
			}

        if (spec.isSetConversionFactor()) {
            vspec.conversionFactor = spec.getConversionFactor();
        }
        else if (sm.model.isSetConversionFactor()) {
            vspec.conversionFactor = sm.model.getConversionFactor();
        }
        boolean initIsExtern = true;
        if (spec.isSetInitialConcentration()) {
            initIsExtern = false;
            double ival = spec.getInitialConcentration();
            if (!isSubstanceOnly || ival==0) {
                vspec.setInitValue(ival);
            }
            else {
                //Convert concentration to substance
                ASTNode form = new ASTNode(libsbml.AST_REAL);
                form.setValue(ival);
                ASTNode times = new ASTNode();
                times.setType(libsbml.AST_TIMES);
                times.addChild(form);
                ASTNode compartment = new ASTNode();
                compartment.setName(compNameMML);
                compartment.setType(libsbml.AST_NAME);
                times.addChild(compartment);
                vspec.setInitValue(times);
            }
        }
        else if (spec.isSetInitialAmount()) {
            initIsExtern = false;
            double ival = spec.getInitialAmount();
            if (isSubstanceOnly || ival==0) {
                vspec.setInitValue(ival);
            }
            else {
                //Convert substance to concentration
                ASTNode form = new ASTNode(libsbml.AST_REAL);
                form.setValue(ival);
                ASTNode div = new ASTNode();
                div.setType(libsbml.AST_DIVIDE);
                div.addChild(form);
                ASTNode compartment = new ASTNode();
                compartment.setName(compNameMML);
                compartment.setType(libsbml.AST_NAME);
                div.addChild(compartment);
                vspec.setInitValue(div);
            }
        }
        InitialAssignment ia = sm.model.getInitialAssignment(name());
        if (ia != null) {
            initIsExtern = false;
            ASTNode form = new ASTNode(ia.getMath());
            if (isSubstanceOnly) {
                ASTNode div = new ASTNode();
                div.setType(libsbml.AST_DIVIDE);
                div.addChild(form);
                ASTNode compartment = new ASTNode();
                compartment.setName(compNameMML);
                compartment.setType(libsbml.AST_NAME);
                div.addChild(compartment);
                vspec.setInitValue(div);
            }
            else {
                vspec.setInitValue(form);
            }
			if (ia.isSetNotes()) {
				SBNotes newNote = new SBNotes(ia.getNotesString());
				newNote.removeXMLTags();
				newNote.addCommentIdentifiers();
				vspec.setNotes(newNote.getNote());
			}

        }
    }
    
    // query
    public String toString() { return diagInfo(); }
    public String name() { return spec.getId(); }
    public String diagInfo() { return "Species " + name(); }
    public SBCompartment sbcomp() throws Xcept { 
        String cname = spec.getCompartment();
        SBCompartment sbcomp = sbmodel.sbcomps.sbcomp(cname);
        if (sbcomp == null) throw new Xcept
            ("Species " + name() + " compartment " + cname  + " not found");
        return sbcomp;
    }

    // write MML declaration
    public void writeMML(PrintWriter wrt) {
    }

    // write MML state eqn
    public void writeMML2(PrintWriter wrt) {
    }

    // SBSpecies.List
    public static class NList extends NamedList {
        public NList(SBModel sbm) throws Xcept {
            super();
            long nspecs = sbm.model.getNumSpecies();
            for (long i=0; i<nspecs; i++) {
                add(new SBSpecies(sbm, sbm.model.getSpecies(i)));
            }
        }
        public NList() { super(); }
        public SBSpecies sbspec(int i) {
            return (SBSpecies) get(i);
        }
        public SBSpecies sbspec(String n) {
            return (SBSpecies) getByName(n);
        }
    }
}
