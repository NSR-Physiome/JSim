/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// CellML document

package JSim.cellml;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class CMLDoc {    
    protected UnitNList preUnits; // pre-defined CellML base units
    protected UnitNList sysUnits; // JSim system units
    protected CMLUnit.NList units; // model unit table
    protected CMLUnit.List seqUnits; // model units in order of creation
    protected CMLComp.NList comps; // components
    protected CMLConnect.List connects; // connections
    protected CMLVarGroup.List varGroups; // connected var groups
    protected ConstContext ctxt; // for writing united constants
    protected int unitConversionErrs; // # eqns failing unit balance
    private Expr.List powerVars; // power substitution variables
    private Expr.List powerValues; // power substitution values
    private   String notes;
    private   CMLVarGroup.List overdeterminedVars;
    private   CMLVarGroup.List underdeterminedVars;

    // constructor
    public CMLDoc(Document doc, UnitNList pu, UnitNList su) throws Xcept {
        preUnits = pu.copy();
        sysUnits = su;
        units = new CMLUnit.NList();
        seqUnits = new CMLUnit.List();
        comps = new CMLComp.NList();
        connects = new CMLConnect.List();
        varGroups = new CMLVarGroup.List();
        overdeterminedVars = new CMLVarGroup.List();
        underdeterminedVars = new CMLVarGroup.List();
        ctxt = new ConstContext(JSLang.lang);
        ctxt.unitConst = true;
        notes = "";

        // parse file XML
        Element root = doc.getDocumentElement();
        if (! root.getNodeName().equals("model"))
            throw new Xcept("CellML file root element must be <model>");
        NodeList nodes = root.getElementsByTagName("import");
        for (int i=0; i<nodes.getLength(); i++) {
            Element importel = (Element) nodes.item(i);
            NodeList components = importel.getElementsByTagName("component");
            if (components.getLength() > 0) {
                throw new Xcept("Unable to interpret CellML 1.1 files that import components.");
            }
        }
       
        nodes = root.getElementsByTagName("documentation");
        for (int i=0; i<nodes.getLength(); i++) {
            collectNotes((Element) nodes.item(i));
        }
        nodes = root.getElementsByTagName("units");
        ArrayList uelems = scheduleUnits(nodes);
        for (int i=0; i<uelems.size(); i++) {
            CMLUnit u = new CMLUnit(this, (Element) uelems.get(i));
            units.add(u);
            seqUnits.add(u);
        }
        nodes = root.getElementsByTagName("component");
        for (int i=0; i<nodes.getLength(); i++) {
            CMLComp c = new CMLComp(this, (Element) nodes.item(i));
            comps.add(c);
        }
        nodes = root.getElementsByTagName("connection");
        for (int i=0; i<nodes.getLength(); i++) {
            CMLConnect c = new CMLConnect(this, (Element) nodes.item(i));        
            connects.add(c);
        }
    
        // build CMLVarGroups
        for (int i=0; i<comps.size(); i++) {
            CMLComp comp = comps.comp(i);
            for (int j=0; j<comp.vars.size(); j++) {
                CMLVar v = comp.vars.var(j);
                if (v.varGroup != null) continue;
                CMLVarGroup g = new CMLVarGroup(this, v);
                varGroups.add(g);
            }
        }
        // create short names for CMLVarGroups
        for (int i=0; i<varGroups.size(); i++) {
            CMLVarGroup g = varGroups.varGroup(i);
            boolean origbad = false;
            for (int j=0; j<g.varNames.size(); j++) {
                String gname = createLegalNameFrom(g.varNames.str(j));
                String origgname = gname;
                boolean found = false;
                for (int k=0; k<varGroups.size(); k++) {
                    if (i==k) continue;
                    CMLVarGroup g1 = varGroups.varGroup(k);
                    if (g1.mmlName.equals(g.mmlName)) {
                        origbad = true;
                    }
                    if (g1.varNames.containSame(gname)) {
                        found = true;
                        //break;
                    }
                }
                if (!found) {
                    g.mmlName = gname;
                    break;
                }
                else if (origbad) {
                    //We have found no name that works, since the original name was also already used,
                    // due to our 'createLegalNameFrom' function happening to duplicate a name that
                    // already existed in that component.
                    int suffix = 1;
                    String newname = createLegalNameFrom(origgname + "_" + suffix);
                    //Util.verbose("  LS original name " + g.mmlName + " is already used.  Trying " + newname);
                    while (found) {
                        found = false;
                        newname = createLegalNameFrom(origgname + "_" + suffix);
                        suffix = suffix+1;
                        for (int k=0; k<varGroups.size(); k++) {
                            if (i==k) continue;
                            CMLVarGroup g1 = varGroups.varGroup(k);
                            if (g1.varNames.containSame(newname)) {
                                found = true;
                                break;
                            }
                        }
                    }
                    g.mmlName = newname;
                }
            }
            //Util.verbose("VarGroup " + g.varNames + " named " + g.mmlName);
        }

        // Collect *all* the equations and map variable groups to them.
        CMLEqnSorter eqs = new CMLEqnSorter(comps, varGroups);
        // For all equations that do not yet map to a variable, figure out what they should map to.
        // This assumes a 1:1 equation:variable-to-solve ratio (which won't be true if CellML ever
        // actually supports partial differential equations).
        eqs.mapEquationsToVariables();
        //Assign domains to the varGroups, and figure out which ones need to be extern.
        eqs.classifyVariables();

        //Find the variables that were changed to create a correctly-determined model
        saveDeterminedChanges();

        // check unit correction
        unitConversionErrs = 0;
        powerVars = new Expr.List();
        powerValues = new Expr.List();
        for (int i=0; i<comps.size(); i++) {
            CMLComp comp = comps.comp(i);
            for (int j=0; j<comp.eqns.size(); j++) {
                CMLEqn eqn = comp.eqns.get(j);
                eqn.checkUnitConversion();
                if (! eqn.unitConversionOK)
                    unitConversionErrs++;
            }
        }
	
        // substitute power variables
        Util.verbose("GLOBAL POWER VARGROUPS: " + powerVars);
        if (powerVars.size() == 0) return;
        for (int i=0; i<powerVars.size(); i++) 
            ((CMLVar) powerVars.expr(i)).varGroup.isStatic = true;
        Util.verbose("GLOBAL POWER VALUES: " + powerValues);
        for (int i=0; i<comps.size(); i++) {
            CMLComp comp = comps.comp(i);
            for (int j=0; j<comp.eqns.size(); j++) {
                CMLEqn eqn = comp.eqns.get(j);
                eqn.substituteValues(powerVars, powerValues);
            }
        }
    }

    //Take the input name and if it is not a legal name in JSim, modify it so that it is.
    // LS NOTE:  should be moved to MMLUtil.
    // LS NOTE 2:  called from the CMLVarGroup constructor.
    static protected String createLegalNameFrom(String name)
        throws Xcept {
        if (MMLUtil.isLegalName(name)) return name;
        String origname = name;
        while (name.indexOf("__") >= 0) {
            name = name.replaceAll("__", "_");
        }
        if (MMLUtil.isLegalName(name)) return name;
        name = "was_" + name;
        while (name.indexOf("__") >= 0) {
            name = name.replaceAll("__", "_");
        }
        if (!MMLUtil.isLegalName(name)) {
            throw new Xcept("Unable to create a legal name from the string '" + origname + "'");
        }
        return name;
    }

    //Find all <title>, <para>, and <caption> elements in the documentation, wrap them, and store.
    private void collectNotes(Element documentation) {
        if (documentation==null) return;
        if (documentation.getNodeName().equals("title") ||
            documentation.getNodeName().equals("para") ||
            documentation.getNodeName().equals("caption")) {
            String text = UtilXML.getNestedText(documentation);
            text = Util.crushWhitespace(text);
            text = text.replaceAll("\\*/", "* /");
            text = Util.wrap(text, 60);
            text = text.replaceAll("\n", "\n * ");
            if (notes.equals("")) {
                notes = " * ";
            }
            else {
                notes = notes + "\n * \n * ";
            }
            notes = notes + text;
        }
        else if (documentation.getNodeName().equals("imagedata")) {
            if (documentation.hasAttribute("fileref")) {
                notes = notes + "\n * \n * [[Image file: " + documentation.getAttribute("fileref") + "]]";
            }
        }
        else {
            NodeList children = documentation.getChildNodes();
            for (int c=0; c<children.getLength(); c++) {
                if (children.item(c) instanceof Element) {
                    collectNotes((Element) children.item(c));
                }
            }
        }
    }
    // schedule processing order for unit Elements
    private ArrayList scheduleUnits(NodeList nodes) 
        throws Xcept {
        int n = nodes.getLength();
        Element[] unproc = new Element[n];
        for (int i=0; i<n; i++)
            unproc[i] = (Element) nodes.item(i);

        ArrayList<Element> proc = new ArrayList<Element>(n);
        StringList procNames = new StringList(n);
        StringList unprocNames = new StringList();
        boolean working = true;
        while (working && proc.size()<n) {
            working = false;
            unprocNames = new StringList();
            for (int i=0; i<n; i++) {
                Element e = unproc[i];
                if (e == null) continue;
                String ename = e.getAttribute("name");
                StringList ereq = requiredUnitNames(e);
                if (! procNames.containSame(ereq)) {
                    //Util.verbose("postponing unit " + ename);
                    unprocNames.add(ename);
                    continue;
                }
                //Util.verbose("scheduling unit " + ename);
                proc.add(e);
                procNames.add(ename);
                unproc[i] = null;
                working = true;
            }
        }
        //We will assume that the unprocessed units are unprocessed because they
        // contain references to undefined units.  We'll simply define these referenced
        // units as being new fundamental units.  Note:  'celsius' also falls into this
        // category, as it is predefined in the CellML specification, but not in JSim.
        // This is handled separately by the Unit code, and celsius units are converted to kelvin.
        if (proc.size() < n) {
            for (int i=0; i<n; i++) {
                Element e = unproc[i];
                if (e == null) continue;
                String ename = e.getAttribute("name");
                //Util.verbose("scheduling unit " + ename);
                proc.add(e);
                unproc[i] = null;
            }
        }
        return proc;
    }

    // required sub-units for unit Element
    private StringList requiredUnitNames(Element elem) {
        StringList subs = new StringList(4);
        NodeList nodes = elem.getElementsByTagName("unit");
        for (int i=0; i<nodes.getLength(); i++) {
            Element e = (Element) nodes.item(i);
            String n = e.getAttribute("units");
            try {
                Unit u = preUnits.byName(n);
                // OK not to check prefix since can't redefine these
            } catch (Xcept x) {
                subs.add(n);
            }
        }
        return subs;
    }

    // get unit by name,  Xcept if not found
    protected Unit getUnit(String uname) throws Xcept {
        //Check CMLUnits first, because some may have been redefined.
        CMLUnit cunit = units.unit(uname);
        if (cunit != null) return cunit.unit();
        try { 
            return preUnits.byName(uname);
        } catch (Xcept e) {
            CMLUnit newunit = new CMLUnit(this, uname);
            units.add(newunit);
            seqUnits.add(newunit);
            return newunit.unit();
        }
    }

    // update powerVars/powerValues
    protected void updatePowerVars(CMLVar v, Expr value) {
    	if (powerVars.contains(v)) return;
        CMLVarGroup vg = v.varGroup;
        for (int i=0; i<vg.vars.size(); i++) {
            CMLVar v1 = vg.vars.var(i);
            powerVars.add(v1);
            powerValues.add(value);
        }
    }

    //Loop through the vargroups and store which ones are overdetermined and which are under (if any).
    private void  saveDeterminedChanges() {
        for (int v=0; v<varGroups.size(); v++) {
            CMLVarGroup vg = varGroups.varGroup(v);
            if (vg.isUnderdetermined()) {
                underdeterminedVars.add(vg);
            }
            else if (vg.isOverdetermined()) {
                overdeterminedVars.add(vg);
            }
        }
    }

    // write MML
    public void writeMML(Writer wrt) throws Xcept {
        PrintWriter out = new PrintWriter(wrt, true);
        if (!notes.equals("")) {
            out.println("/*");
            out.println(notes);
            out.println(" */\n");
        }
        out.println("import nsrunit;");
        if (unitConversionErrs > 0) out.println(
	    "// Warning: unit conversion turned off due to unit errors in "
	    + unitConversionErrs + " equation(s)");
        String ustat = (unitConversionErrs == 0) ? "on" : "off";
        out.println("unit conversion " + ustat + ";");
        for (int i=0; i<seqUnits.size(); i++) 
            seqUnits.unit(i).writeMML(out);
	out.println("property cellMLNames=string;");
        out.println("");
 
 	// main component
        out.println("math main {");
        //Write out over/underdetermined warnings
        if (underdeterminedVars.size() > 0) {
            String warning = "Warning:  the following variables were set 'extern' or given an initial value of '0' because the model would otherwise be underdetermined:  ";
            for (int v=0; v<underdeterminedVars.size(); v++) {
                if (v>0) {
                    warning = warning + ", ";
                }
                warning = warning + underdeterminedVars.varGroup(v).mmlName;
            }
            warning = Util.wrap(warning, 60);
            warning = "\t//" + warning;
            warning = warning.replaceAll("\n", "\n\t//  ");
            out.println(warning);
        }

        if (overdeterminedVars.size() > 0) {
            String warning = "Warning:  the following variables had initial values which were suppressed because the model would otherwise be overdetermined:  ";
            for (int v=0; v<overdeterminedVars.size(); v++) {
                if (v>0) {
                    warning = warning + ", ";
                }
                warning = warning + overdeterminedVars.varGroup(v).mmlName;
            }
            warning = Util.wrap(warning, 60);
            warning = "\t//" + warning;
            warning = warning.replaceAll("\n", "\n\t//  ");
            out.println(warning);
        }
        
        // write variable declarations for domains
        for (int i=0; i<varGroups.size(); i++) {
            if (varGroups.varGroup(i).isDomain) {
                varGroups.varGroup(i).writeMML(out);
            }
        }

        // write variable declarations for everything else without a string initial value
        for (int i=0; i<varGroups.size(); i++) {
            if (!varGroups.varGroup(i).isDomain && !varGroups.varGroup(i).initValueIsString()) {
                varGroups.varGroup(i).writeMML(out);
            }
        }

        // write variable declarations for everything else
        for (int i=0; i<varGroups.size(); i++) {
            if (!varGroups.varGroup(i).isDomain && varGroups.varGroup(i).initValueIsString()) {
                varGroups.varGroup(i).writeMML(out);
            }
        }

        // write equations for each component
        for (int i=0; i<comps.size(); i++) 
            comps.comp(i).writeMML(out);

        if (comps.size()==0) {
            //No components found--probably a units-only CellML model.
            out.println("\t//Note:  no math in this CellML model:  probably used for defining units or metadata only.");
            //LS DEBUG:  if the new parser doesn't need this, do something different (maybe
            // even not writing out the 'math main' bit at all)
            out.println("\textern real all;");
        }
        // done 
        out.println("}");
    }

}

