/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML compartment for import

package JSim.sbml;

import JSim.util.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBCompartment implements Named {
    protected SBModel sbmodel;
    protected Compartment comp;
    protected SBVar v;
	protected String notes;
    
    // constructor
    public SBCompartment(SBModel sm, Compartment c) throws Xcept {
        sbmodel = sm;
        comp = c;
		if (c.isSetNotes()) {
				SBNotes newNote = new SBNotes(c.getNotesString());
				newNote.removeXMLTags();
				newNote.addCommentIdentifiers();
				this.notes = new String(newNote.getNote());
				this.notes = this.notes.replace(System.getProperty("line.separator"), "");
			}
    	else { this.notes = new String(""); }

		// Store notes in SBVar ......
        v = new SBVar(sbmodel, name(), "compartment");
        double sz = 1;  // non-spec hack 4 biomodel auto-verify
        if (comp.isSetSize()) {
            sz = comp.getSize();
        }
        else if (comp.isSetVolume()) {
            sz = comp.getVolume(); //For SBML level 1
        }
        if (comp.isSetConstant() && comp.getLevel() != 1) {
            v.setConst(comp.getConstant());
        }
        v.setInitValue(sz);
		v.setNotes(this.notes);
        InitialAssignment ia = sm.model.getInitialAssignment(c.getId());
        if (ia != null) {
            v.setInitValue(ia.getMath());
			if (ia.isSetNotes()) {
				SBNotes newNote = new SBNotes(ia.getNotesString());
				newNote.removeXMLTags();
				newNote.addCommentIdentifiers();
				v.setNotes(newNote.getNote());
			}

        }
        String cunit = unit();
        v.setUnit(cunit);
    }
    
    // query
    public String name() { return comp.getId(); }
    public String diagInfo() { return "Compartment " + name(); }
    public String unit() {
        String cunit;
        if (comp.isSetUnits()) {
            cunit = comp.getUnits();
        }
        else {
            long sd = comp.getSpatialDimensions();
            if (sd == 3) {
                cunit = "volume";
            }
            else if (sd == 2) {
                cunit = "area";
            }
            else if (sd == 1) {
                cunit = "length";
            }
            else if (sd == 0) {
                cunit = "dimensionless"; //LS DEBUG I guess?
            }
            else {
                //LS DEBUG:  throw an error?
                cunit = "volume";
            }
        }
        String jcunit = sbmodel.getSBUnitSpace().toJSimFromSBML(cunit);
        if (jcunit != null) {
            return jcunit;
        }
        return cunit;
    }
    public long spatialDimensions() { 
        return comp.getSpatialDimensions(); 
    }

    // write MML
    public void writeMML(PrintWriter wrt) {
    }

    // SBCompartment.List
    public static class NList extends NamedList {
        public NList(SBModel sbm) throws Xcept {
            super();
            long ncmp = sbm.model.getNumCompartments();
            for (int i=0; i<ncmp; i++) {
                add(new SBCompartment(sbm, sbm.model.getCompartment(i)));
            }
        }
        public NList() { super(); }
        public SBCompartment sbcomp(int i) {
            return (SBCompartment) get(i);
        }
        public SBCompartment sbcomp(String n) {
            return (SBCompartment) getByName(n);
        }
    }
}
