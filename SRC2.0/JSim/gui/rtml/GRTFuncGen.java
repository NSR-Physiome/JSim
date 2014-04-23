/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// auto configured FuncGen page

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
import JSim.project.*;
import JSim.gui.*;

public class GRTFuncGen extends GRTPage {

	// state
	private FuncGen fgen;
	private Document doc;

	// constructor
	public GRTFuncGen(GRTAuto auto, FuncGen f) {
	    super(auto);
	    fgen = f;
	    name = fgen.name();
	}

	// make XML Element
	public Element makeXML(Document d) {
	    doc = d;
	    Element page = doc.createElement("page");
	    page.setAttribute("name", "FuncGen " + name);

	    Element e = doc.createElement("text");
	    e.setAttribute("text", "Function Generator " + name);	    
	    e.setAttribute("fontMult", "1.3");
	    e.setAttribute("pos", "1 1");
	    page.appendChild(e);

	    e = doc.createElement("removeButton");
	    e.setAttribute("name", name);
	    e.setAttribute("pos", "29 4");
	    page.appendChild(e);

	    Element tbl = doc.createElement("table");
	    tbl.setAttribute("pos", "1 3");
	    tbl.setAttribute("widths", "7 10 0");
	    e = doc.createElement("var");
	    e.setAttribute("name", name + ".domain0");
	    e.setAttribute("text", "Domain");
	    tbl.appendChild(e);
	    e = doc.createElement("var");
	    e.setAttribute("name", name + ".which");
	    e.setAttribute("text", "Function");
	    tbl.appendChild(e);
	    page.appendChild(tbl);
	    
	    // sub tables
	    int y=8;
	    for (int i=0; i<fgen.which.nLabels(); i++) {
	    	PNamed func = fgen.func(i);

		// single column of fgen parms
		if (func.nChild() <= 9) {
		    tbl = makeTable(doc, 2, y, i, 0, func.nChild());	
	 	    page.appendChild(tbl);

		// double columns of fgen parms
		} else {
		    int j = (func.nChild()+1)/2;
		    tbl = makeTable(doc, 2, y, i, 0, j);	
	 	    page.appendChild(tbl);
		    tbl = makeTable(doc, 20, y, i, j, func.nChild());	
	 	    page.appendChild(tbl);
		}
	    }

	    // preview graph
	    e = doc.createElement("graph");
	    e.setAttribute("pos", "1 32");
	    e.setAttribute("size", "36 17");
	    e.setAttribute("expr", name);
	    page.appendChild(e);

	    return page;
	}

	// make single func sub-table
	Element makeTable(Document doc, int xpos, int ypos,
	int i, int j0, int j1) {  
	    Element tbl = doc.createElement("table");
	    tbl.setAttribute("pos", "" + xpos + " " + ypos);
	    tbl.setAttribute("widths", "10 7 0");
	    tbl.setAttribute("visible", name + ".which=" + i);
	    int ct=0;
	    PNamed func = fgen.func(i);
	    for (int j=j0; j<j1; j++) {
		if (! (func.child(j) instanceof Control)) continue;
		Control cntl = (Control) func.child(j);
		Element e = doc.createElement("var");
		e.setAttribute("name", 
		    name + "." + func.name() + "." + cntl.name());
		String lab = cntl.name();
		e.setAttribute("text", lab);
		tbl.appendChild(e);
		ct++;
	    }
	    return tbl;
	}

}		
