/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// CML unit

package JSim.cellml;

import JSim.util.*;
import JSim.expr.*;
import java.io.PrintWriter;
import java.util.ArrayList;
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 

public class CMLUnit implements Named {
    private CMLDoc doc;
    private String cmlName; // name in original CellML document
    private String mmlName; // name in output MML
    private Unit unit; // named according to MML
    private boolean fundamental; // fundamental (base) unit?
    private boolean renamed;
    private boolean unknown;
    private boolean celsius;

    // constructor
    protected CMLUnit(CMLDoc d, Element elem) throws Xcept {
        doc = d;
        renamed = false;
        unknown = false;
        celsius = false;
        cmlName = elem.getAttribute("name");
        //Util.verbose("processing unit " + cmlName);

        // calculate internal unit
        unit = Unit.scalar();
        double mult = 1;
        NodeList nodes = elem.getElementsByTagName("unit");
        for (int i=0; i<nodes.getLength(); i++) {
            Unit u = getSubUnit((Element) nodes.item(i));
            double exp = getExponent((Element) nodes.item(i));
            if (Math.abs(exp) != 1) 
                u = u.power(Math.abs(exp));
            if (exp > 0) 
                unit = unit.mult(u);
            else
                unit = unit.div(u);
            double localmult = getMultiplier((Element) nodes.item(i));
            if (localmult != 1) {
                mult = mult * localmult;
            }
            
        }

        // define mmlName
        Unit testunit = new Unit("test", unit, mult);
        if (conflict(doc.sysUnits, cmlName, testunit)) {
            renamed = true;
            mmlName = uniqueName(cmlName);
        }
        else {
            mmlName = cmlName;
        }
        unit = new Unit(mmlName, unit, mult);

        // new fundamental unit?
        fundamental = elem.getAttribute("base_units").equals("yes");
        if (!fundamental) return;
        doc.preUnits.addFund(mmlName);
        unit = doc.preUnits.byName(mmlName);
    }

    protected CMLUnit(CMLDoc d, String name) throws Xcept {
        doc = d;
        renamed = false;
        unknown = true;
        cmlName = name;
        mmlName = name;
        celsius = false;
        //Util.verbose("processing unknown unit '" + cmlName + "'.");
        fundamental = false;

        if (name.equals("celsius")) {
            celsius = true;
            unit = doc.sysUnits.byName("kelvin");
            mmlName = "kelvin";
            unit = doc.sysUnits.byName(mmlName);
            unknown = false;
            return;
        }
        // calculate internal unit.  If it's predefined in JSim, assume that's the one we want.
        try {
            unit = doc.sysUnits.byName(cmlName);
        } catch (Xcept e) {
            unit = Unit.scalar();
            unknown = true;
            fundamental = true;
        }
        if (fundamental) {
            doc.preUnits.addFund(mmlName);
            unit = doc.preUnits.byName(mmlName);
        }
    }

    // get subunit by prefix & name
    private Unit getSubUnit(Element elem) throws Xcept {

        // get base unit
        String base = elem.getAttribute("units");
        Unit u;
        try {
            u = doc.preUnits.byName(base);
        } catch (Xcept e) {
            CMLUnit ubase = doc.units.unit(base);
            if (ubase==null) {
                //Define a new fundamental unit
                CMLUnit newunit = new CMLUnit(doc, base);
                doc.units.add(newunit);
                doc.seqUnits.add(0,newunit);
                u = newunit.unit();
            }
            else {
                u = ubase.unit();
            }
        }
        
        // process prefix
        String pfx = elem.getAttribute("prefix");
        if (Util.isBlank(pfx)) return u;
        UnitNList.Prefix upfx = 
            doc.preUnits.prefixes.prefix(pfx);
        try {
            double mult = (upfx == null) ?
                Math.pow(10, Util.toInt(pfx)) : upfx.mult();
            return new Unit(pfx+base, u.f*mult, u.dim);
        } catch (Xcept e) {
            throw new Xcept(this,
                            "Illegal unit prefix: " + pfx);
        }
    }

    // get unit exponent
    private double getExponent(Element elem) throws Xcept {
        String kexp = elem.getAttribute("exponent");
        return  Util.isBlank(kexp) ? 1.0 : Util.toDouble(kexp);
    }

    // get unit multiplier
    private double getMultiplier(Element elem) throws Xcept {
        String mult = elem.getAttribute("multiplier");
        return  Util.isBlank(mult) ? 1.0 : Util.toDouble(mult);
    }

    // does provisional unit conflict with unit table?
    private boolean conflict(UnitNList units, String n, Unit u) {
        try {
            Unit u0 = units.byName(n);
            return ! Unit.same(u, u0); 
        } catch (Xcept e) {
            return false;
        }
    }
    
    // generate unique name 
    private String uniqueName(String n) {
        while(true) {
            n = n + "_";
            if (doc.sysUnits.defined(n)) continue;
            if (doc.units.unit(n) != null) continue;
            return n;
        }
    }

    // write MML definition of unit, if needed
    public void writeMML(PrintWriter out) throws Xcept {
        if (celsius) {
            out.println("//Warning:  the unit 'celsius' is not well defined.");
            out.println("//  It may mean 'distance from 0 degrees C' in some contexts,  and 'distance in C degrees' in others.");
            out.println("//  We assume this model meant the latter, and have converted the unit to the equivalent but better-defined 'kelvin' instead.");
        }
        else if (renamed) {
            out.println("//Warning:  unit " + mmlName + " renamed from " + cmlName + 
	    	", as the latter is predefined in JSim with different fundamental units.");
        }
        else if (unknown && fundamental) {
            out.println("//Warning:  unit " + mmlName + 
	        " unknown; assuming it is a fundamental unit.");
        }
        else if (unknown) {
            out.println("//Warning:  unit " + mmlName + 
	        " unknown; assuming it is equivalent to the JSim predefined unit of the same name.");
        }
        if (fundamental) {
            out.println("unit " + mmlName + " = fundamental;");
            return;
        }
        try {
            Unit pu = doc.preUnits.byName(mmlName);
            out.println("// unit " + mmlName + " predefined");
        } catch (Xcept e) {
            out.println("unit " + mmlName + "=" + 
                        unit.fundStr(doc.preUnits) + ";");
        }
    }

    // query
    public String name() { return cmlName; }
    public String diagInfo() { return "CellML unit " + name(); }
    public Unit unit() { return unit; }

    // CMLUnit.List
    public static class List extends ArrayList<CMLUnit> {
        public List() { super(); }
        public CMLUnit unit(int i) { return (CMLUnit) get(i); }
    }

    // CMLUnit.NList
    public static class NList extends NamedList {
        public NList() { super(16); }
        public CMLUnit unit(int i) { return (CMLUnit) get(i); }
        public CMLUnit unit(String n) {
            return (CMLUnit) getByName(n);
        }
    }
}
