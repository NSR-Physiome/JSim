/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// auto configured GRT document

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
import JSim.gui.model.*;

public class GRTAuto extends GRTDoc{

	// state info
	private Element root;
	private String errMsg;
	private ASModel rt;
	private ASVar.List vars, dvars, ivars, ovars;
	private boolean units;
	private Document doc;

	// constructor
	public GRTAuto(GModelPars p) {
	    super(p);
	    reload();
	}

	// reload
	public void reload() {
	    clear();

	    // get model stuff
	    rt = null;
	    errMsg = null;
	    try {
		rt = gmodel().pmodel().rt();
		vars = rt.getASVars();
		dvars = new ASVar.List(4);
		ivars = new ASVar.List(vars.size());
		ovars = new ASVar.List(vars.size());
		units = rt.getFlags().needsUnits;
		for (int i=0; i<vars.size(); i++) {
		    ASVar v = vars.asvar(i);
		    if (v.isDomain())
			dvars.add(v);
		}
		for (int i=0; i<vars.size(); i++) {
		    ASVar v = vars.asvar(i);
		    if (v.isDomain()) continue;
		    boolean ignore = false;
		    for (int j=0; j<dvars.size(); j++) {
			ASVar x = dvars.asvar(j);
			if (isSubVar(x, v)) ignore = true;
		    }
		    if (ignore) continue;
		    if (v.isInput())
			ivars.add(v);
		    else 
			ovars.add(v);
		}
	    } catch (Xcept e) {
		errMsg = e.cleanMessage();
	    }

	    doc = UtilXML.createDoc("JSim");
	    root = doc.getDocumentElement();
	    makePages();

	    // solver page
 	    PModelVars vars = gmodel().pmodel().vars();
	    PSolverSettings psolver = vars.solver();
	    GRTSolver gsolver = new GRTSolver(this, psolver);
	    Element e = gsolver.makeXML(doc);
	    root.appendChild(e);

	    // funcs
	    for (int i=0; i<vars.nChild(); i++) {
		if (! (vars.child(i) instanceof FuncGen))
		    continue;
		FuncGen func = (FuncGen) vars.child(i);
		GRTFuncGen gfunc = new GRTFuncGen(this, func);
		e = gfunc.makeXML(doc);
		root.appendChild(e);
	    } 

	    reload(doc.getDocumentElement());
	}

	// make pages in root
	public void makePages() {
	    Element page = doc.createElement("page");
	    page.setAttribute("name", "Model_Parms");
	    root.appendChild(page);

	    if (errMsg != null) {
		Element msg = doc.createElement("text");
	        msg.setAttribute("pos", "0.5 3");
	        msg.setAttribute("text", errMsg);
	    	page.appendChild(msg);
		root.appendChild(page);
		return;
	    }
	
	    // domain blocks
	    double x = 0.5;
	    double y = 0;
	    for (int i=0; i<dvars.size(); i++) {
		ASVar d = dvars.asvar(i);
		Element lab = doc.createElement("text");
		lab.setAttribute("text", "Domain " + d.name());
	    	lab.setAttribute("fontMult", "1.3");
		lab.setAttribute("pos", "" + x + " " + y);
		page.appendChild(lab);
		y += 2;
		ASVar.List vsub = subVars(d);
		double[] ws = widths(null);
		Element tbl = makeTable(vsub, ws);
		tbl.setAttribute("pos", "" + x + " " + y);
		y += 1 + GRTTable.HMULT*vsub.size();
		page.appendChild(tbl);
	    }
//	    y += 1;
		
	    // input header
	    double yin = y;
	    Element ihdr = doc.createElement("text");
	    ihdr.setAttribute("pos", "" + x + " " + y);
	    ihdr.setAttribute("fontMult", "1.3");
	    ihdr.setAttribute("text", "Model Inputs");
	    page.appendChild(ihdr);
	    y += 2;

	    // input table(s)
	    double[] wins = widths(ivars);
	    Element itbl = makeTable(ivars, wins);
	    itbl.setAttribute("pos", "" + x + " " + y);
	    page.appendChild(itbl);
	    y += 1 + GRTTable.HMULT*ivars.size();

	    // double column inputs/outputs
	    double[] wouts = widths(ovars);
	    double wtot = wins[0] + wins[1] + wins[2]
	        + wouts[0] + wouts[1] + wouts[2];
	    if (wtot < 52 && ivars.size() > 20) {
	    	y = yin;
		x = x + wins[0] + wins[1] + wins[2] + 1;
	    }

	    // outputs header
	    Element ohdr = doc.createElement("text");
	    ohdr.setAttribute("pos", "" + x + " " + y);
	    ohdr.setAttribute("fontMult", "1.3");
	    ohdr.setAttribute("text", "Model Outputs");
	    page.appendChild(ohdr);
	    y += 2;
		
	    // outputs table
	    Element otbl = makeTable(ovars, wouts);
	    otbl.setAttribute("pos", "" + x + " " + y);
	    page.appendChild(otbl);

	}

	// xwidths for input/output lists
	private double[] widths(ASVar.List list) {
	    int wlab = 7;
	    if (list != null) {
	    	for (int i=0; i<list.size(); i++) {
	    	    ASVar v = list.asvar(i);
		    String l = parenName(v);
		    wlab = Math.max(wlab, l.length());
		}
	    } 
	    double wval = 7.2; // .6 ~= 1 digit
	    double wunits = units ? 9 : 0;
	    return new double[] { wlab/1.4, wval, wunits, 0 };
	}


	// create table from var list
	private Element makeTable(ASVar.List list, double[] ws) {
	    Element tbl = doc.createElement("table");
	    tbl.setAttribute("widths",
		"" + Util.pretty(ws[0]) + 
		" " + Util.pretty(ws[1]) + 
		" " + Util.pretty(ws[2])); 
	    for (int i=0; i<list.size(); i++) {
		ASVar v = list.asvar(i);
		Element e = doc.createElement("var");
		e.setAttribute("name", v.name());
		tbl.appendChild(e);
	    }
	    return tbl;
	}

	// is sub-variable
	private boolean isSubVar(ASVar v, ASVar sv) {
	    String vn = v.name();
	    String svn = sv.name();
	    int vl = vn.length();
	    int svl = svn.length();
	    if (svl <= vl+1) return false;
	    if (svn.charAt(vl) != '.') return false;
	    svn = svn.substring(0, vl);
	    return vn.equals(svn);
	}
		
	// subvariable list
	private ASVar.List subVars(ASVar d) {
	    ASVar.List list = new ASVar.List(4);
	    for (int i=0; i<vars.size(); i++) {
		ASVar v = vars.asvar(i);
		if (isSubVar(d, v)) list.add(v);
	    }
	    return list;
	}

	// query
	public ASModel rt() { return rt; }
	public Document doc() { return doc; }
}
