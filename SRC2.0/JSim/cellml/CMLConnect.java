/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// CML component

package JSim.cellml;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class CMLConnect {
    private CMLDoc doc;
    protected CMLComp comp1, comp2;
    protected Expr.List vars1, vars2;

    // constructor
    protected CMLConnect(CMLDoc d, Element elem) throws Xcept {
        doc = d;
        //Util.verbose("processing connection"); 
        NodeList nodes = elem.getElementsByTagName("map_components");
        if (nodes.getLength() != 1) throw new Xcept(
              "<connect> requires unique <map_components> element.");
        Element mapComp = (Element) nodes.item(0);
        comp1 = getComp(mapComp, "component_1");
        comp2 = getComp(mapComp, "component_2");
        //Util.verbose("  " + comp1.name() + " " + comp2.name());
        vars1 = new Expr.List(8);
        vars2 = new Expr.List(8);
        nodes = elem.getElementsByTagName("map_variables");
        for (int i=0; i<nodes.getLength(); i++) {
            Element mapv = (Element) nodes.item(i);
            CMLVar v1 = getVar(mapv, "variable_1", comp1);
            CMLVar v2 = getVar(mapv, "variable_2", comp2);
            vars1.add(v1);
            vars2.add(v2);
            v1.connectVars.add(v2);
            v2.connectVars.add(v1);
        }
        
    }

    // find a comp
    private CMLComp getComp(Element mapc, String attr) 
        throws Xcept {
        String name = mapc.getAttribute(attr);
        if (Util.isBlank(name)) throw new Xcept( 
             "<map_components> tag missing \"" + attr + "\" attribute.");
        CMLComp c =  doc.comps.comp(name);
        if (c == null) throw new Xcept(
             "Unknown component \"" + name + "\" in <connection>.");
        return c;
    }

    // find a var
    private CMLVar getVar(Element mapv, String attr, CMLComp comp) 
        throws Xcept {
        String name = mapv.getAttribute(attr);
        if (Util.isBlank(name)) throw new Xcept(comp, 
             "<map_variables> tag missing \"" + attr + "\" attribute.");
        CMLVar v =  comp.vars.var(name);
        if (v == null) throw new Xcept
            (comp, "No such variable \"" + name + "\" in component \"" + comp.name() +"\" in <connection>.");
        return v;
    }

    // write MML variables
    protected void writeMML(PrintWriter out) throws Xcept {
        for (int i=0; i<vars1.size(); i++) {
            CMLVar v1 = (CMLVar) vars1.expr(i);
            CMLVar v2 = (CMLVar) vars2.expr(i);
            out.println("\t" + v1.mmlName() + " = " + v2.mmlName() + ";");
        }
    }
    
    // query
    public String toString() { return "CellML connection " + 
            comp1.name() + " " + comp2.name(); }
    public String diagInfo() { return toString(); }

    // CMLConnect.List
    public static class List extends ArrayList<CMLConnect> {
        public List() { super(); }
        public CMLConnect connect(int i) { return (CMLConnect) get(i); }
    }
}
