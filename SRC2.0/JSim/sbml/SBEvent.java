/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML event for import

package JSim.sbml;

import JSim.util.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBEvent implements Named {
    protected SBModel sbmodel;
    protected Event event;

    // constructor
    public SBEvent(SBModel sm, Event c) throws Xcept {
        sbmodel = sm;
        event = c;
        if (sbmodel.abortEvents) 
            throw new Xcept("SBML <event> tag not yet supported");
    }
    
    // query
    public String name() { return event.getId(); }
    public String diagInfo() { return "Event " + name(); }

    // write MML
    public void writeMML(PrintWriter wrt) {
        wrt.println("// WARNING: SBML events are not currently supported: "
                    + name());
    }

    // SBEvent.List
    public static class NList extends NamedList {
        public NList(SBModel sbm) throws Xcept {
            super();
            long nev = sbm.model.getNumEvents();
            for (int i=0; i<nev; i++) {
                add(new SBEvent(sbm, sbm.model.getEvent(i)));
            }
        }
        public NList() { super(); }
        public SBEvent sbevent(int i) {
            return (SBEvent) get(i);
        }
        public SBEvent sbevent(String n) {
            return (SBEvent) getByName(n);
        }
    }
}
