/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// auto configured Solver page

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.gui.*;

public class GRTSolver extends GRTPage {

	// state
	private ASModel rt;
	private PSolverSettings solver;
	private Document doc;

	// constructor
	public GRTSolver(GRTAuto auto, PSolverSettings s) {
	    super(auto);
	    solver = s;
	    name = solver.name();
	    rt = auto.rt();
	}

	// make XML Element
	public Element makeXML(Document d) {
	    doc = d;
	    Element page = doc.createElement("page");
	    page.setAttribute("name", "Solvers");

	    Element e = doc.createElement("text");
	    e.setAttribute("text", "Solver Settings");
	    e.setAttribute("fontMult", "1.3");
	    e.setAttribute("pos", "1 1");
	    page.appendChild(e);
	    int y=3;

	    // PDE controls, if needed
	    ASModel.Flags flags = rt.getFlags();
	    if (flags.usesPDESolver()) {
	        Element tbl = doc.createElement("table");
	        tbl.setAttribute("pos", "1 " + y);
	        e = doc.createElement("var");
	        e.setAttribute("name", "solver.pde_which");
	        e.setAttribute("text", "PDE method");
	        tbl.appendChild(e);
	        page.appendChild(tbl);

	        // solver selection message
	        String[] msgs = new String[] {
	            "coarse grid gives segmented output",
	            "causes excessive spread",
	            "robust, but slow"
	        };
	        for (int i=0; i<msgs.length; i++) {
	            if (! flags.usesPDESolvers[i])
	                msgs[i] = "not supported for this problem";
	            e = doc.createElement("text");
	            e.setAttribute("text", "(" + msgs[i] + ")");
	            e.setAttribute("pos", "18 " + y);
	            e.setAttribute("visible", "solver.pde_which=" + i);
	            page.appendChild(e);
	        }

	        for (int i=0; i<solver.pde_which.nLabels(); i++)
	            makeTable(page, solver.pde_which, 3, y+2, i, i, "true");

	        y += 4;
	    }

	    // ODE controls, if needed
	    boolean doODE = flags.usesODESolver
	        || flags.usesPDESolvers[ASModel.PDE_LSFEA];
	    if (doODE) {
	        String odevis = "true";
	        if (! flags.usesODESolver) 
	            odevis = "solver.pde_which=" + ASModel.PDE_LSFEA;

	        // ode_which control
	        Element tbl = doc.createElement("table");
	        tbl.setAttribute("pos", "1 " + y);
	        tbl.setAttribute("visible", odevis);
	        e = doc.createElement("var");
	        e.setAttribute("name", "solver.ode_which");
	        e.setAttribute("text", "ODE method");
	        tbl.appendChild(e);
	        page.appendChild(tbl);

	        // non-Auto tables
	        for (int i=0; i<solver.ode_which.nLabels(); i++)
	            makeTable(page, solver.ode_which, 18, y, i, i, odevis);

	        // Auto labels and tables
	        y += 3;
	        e = doc.createElement("text");
	        e.setAttribute("text", "Dopri5 tried first:");
	        e.setAttribute("pos", "1 " + y);
	        e.setAttribute("visible", odevis + " and solver.ode_which=0");
	        page.appendChild(e);
	        e = doc.createElement("text");
	        e.setAttribute("text", "Radau used if Dopri5 fails:");
	        e.setAttribute("pos", "18 " + y);
	        e.setAttribute("visible", odevis + " and solver.ode_which=0");
	        page.appendChild(e);
	        y += 2;
	        makeTable(page, solver.ode_which, 1, y, 1, 0, odevis);
	        makeTable(page, solver.ode_which, 18, y, 2, 0, odevis);
	        y += 17; // ???
	        
	    }

	    // FZero controls, if needed
	    if (flags.usesFzero2Solver) {

	        // Auto labels and tables
	        e = doc.createElement("text");
	        e.setAttribute("text", "Non-linear zero finders:");
	        e.setAttribute("pos", "1 " + y);
	        page.appendChild(e);
	        y += 2;
	        String[] names = new String[] {
	            "bound", "unbound", "errtol", "maxcalls", 
	            "maxiters", "eps", "istep", "npoints", "inittemp",
                    "populationsize","mutationrate","crossoverrate",
                    "mutationstep","elitecutoff","selectmethod" };
	        makeTable(page, "fzero", names, 1, y);
	        y += 10;
	    }

	    // random number controls, if needed
	    if (flags.usesRandom) {
	        e = doc.createElement("text");
	        e.setAttribute("text", "Random number generator:");
	        e.setAttribute("pos", "1 " + y);
	        page.appendChild(e);
	        y += 2;
	        String[] names = new String[] { "seed" };
	        makeTable(page, "random", names, 1, y);
	        y += 2;
	    }

	    // no solvers need message?
	    if (y == 3) {
	        e = doc.createElement("text");
	        e.setAttribute("text", 
	            "This model does not use JSim's internal ODE/PDE solvers or zero-finders.");
	        e.setAttribute("pos", "3 3");
	        page.appendChild(e);
	    }

	    return page;
	}

	// make tables for one type of solver
	private void makeTable(Element page, 
	ChoiceControl which, double x, double y, int i, int ivis, String xvis) {
	    Control.List cntls = solver.controls(which, i);
	    if (cntls.size() == 0) return;
	    Element tbl = doc.createElement("table");
	    tbl.setAttribute("pos", "" + x + " " + y);
	    tbl.setAttribute("visible", 
	        xvis + " and solver." + which.name() + "=" + ivis);
	    for (int j=0; j<cntls.size(); j++) {
	        Control cntl = cntls.control(j);
	        Element e = doc.createElement("var");
	        e.setAttribute("name", "solver." + cntl.name());
	        String lab = solver.prettyName(cntl,
	            which, i);
	        e.setAttribute("text", lab);
	        tbl.appendChild(e);
	    }
	    page.appendChild(tbl);
	}

	// make table of given controls
	private void makeTable(Element page, String pfx, String[] names, double x, double y) {
	    Element tbl = doc.createElement("table");
	    tbl.setAttribute("pos", "" + x + " " + y);
	    for (int j=0; j<names.length; j++) {
	        String name = names[j];
	        Element e = doc.createElement("var");
	        e.setAttribute("name", "solver." + pfx + "_" + name);
	        e.setAttribute("text", name);
	        tbl.appendChild(e);
	    }
	    page.appendChild(tbl);
	}
	    
}

