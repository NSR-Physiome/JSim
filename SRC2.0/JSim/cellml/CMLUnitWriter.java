/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// unit handling for CellML export

package JSim.cellml;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

import java.io.*;
import java.util.*;
import java.lang.Math.*;
import org.w3c.dom.*;

public class CMLUnitWriter {
    private CMLWriter writer; 
    private HashMap<String, String> mJSimToCellML;
    private Element xunitList;
    // names of SBML-defined units
    public static String[] CELLML_UNIT_NAMES = new String[] {
    "ampere", "becquerel", "candela", "celsius", "coulomb", 
    "dimensionless", "farad", "gram", "gray", "henry", 
    "hertz", "joule", "katal", "kelvin", "kilogram", 
    "litre", "liter", "lumen", "lux", "metre", "meter", "mole", 
    "newton", "ohm", "pascal", "radian", "second", "siemens", 
    "sievert", "steradian", "tesla", "volt", "watt", "weber"
    };


    // constructor
    public CMLUnitWriter(CMLWriter writer, Element xunitList) throws Xcept {
        this.writer = writer;
        this.xunitList = xunitList;
        mJSimToCellML = new HashMap<String, String>();

        // map all unit names common to CellML & JSim
        for (int i=0; i<CELLML_UNIT_NAMES.length; i++) {
            String uname = CELLML_UNIT_NAMES[i];
            addCellMLJSimConversion(uname, uname);
        }

        // special fixed additions for nsrunit - NOT GENERAL !!!
        // better would be to have a defined mapping in XMML from fundamental
        //  units to SI units.
        addCellMLJSimConversion("ampere", "amp");
        addCellMLJSimConversion("gram", "g");
        addCellMLJSimConversion("joule", "J");
        addCellMLJSimConversion("kelvin", "degK");
        addCellMLJSimConversion("hertz", "hz");
        addCellMLJSimConversion("kilogram", "kg");
        addCellMLJSimConversion("litre", "L");
        addCellMLJSimConversion("metre", "m");
        addCellMLJSimConversion("mole", "mol");
        addCellMLJSimConversion("newton", "N");
        addCellMLJSimConversion("pascal", "pa");
        addCellMLJSimConversion("second", "sec");
    }
        

    // create CellML UnitDefinitions
    protected void loadUnits() throws Xcept {
        ArrayList<Element> xunits = new ArrayList<Element>();
        NodeList nodes = xunitList.getChildNodes();
        for (int i=0; i<nodes.getLength(); i++) {
            if (! (nodes.item(i) instanceof Element)) continue;
            Element e = (Element) nodes.item(i);
            xunits.add(e);
        }
        for (int u=0; u<xunits.size(); u++) {
            //System.out.println("Unit " + u);
            Element xunit = xunits.get(u);
            String type = xunit.getTagName();
            if (type.equals("fundamentalUnit")) {
                String base = xunit.getAttribute("id");
                String basecellml = toCellMLFromJSim(base);
                if (basecellml==null) {
                    basecellml = makeCellMLIdFrom(base);
                    Element fund = writer.createElement("units");
                    fund.setAttribute("name", basecellml);
                    addCellMLJSimConversion(basecellml, base);
                }
            }
            else if (type.equals("derivedUnit")) {
                processDerivedUnit(xunit);
            }
            else {
                writer.addWarning("Warning:  unknown unit list element '" + type + "'.");
            }
        }
        //System.out.println("Done with units.");
    }

    private void processDerivedUnit(Element xunit) throws Xcept {
        String jsimunit = xunit.getAttribute("id");
        //System.out.println("Calculating derived unit " + jsimunit);
        if (toCellMLFromJSim(jsimunit) != null) {
            //This unit already exists in CellML, probably as a fundamental unit.  Don't create another one.
            return;
        }
        String cellmlunit = makeCellMLIdFrom(jsimunit);
        while (isTranslatedUnit(cellmlunit)) {
            cellmlunit = cellmlunit + "_";
        }
        Element cmlunit = writer.createElement("units");
        cmlunit.setAttribute("name", cellmlunit);
        addCellMLJSimConversion(cellmlunit, jsimunit);
        ArrayList<Element> factors = new ArrayList<Element>();
        NodeList nodes = xunit.getChildNodes();
        for (int i=0; i<nodes.getLength(); i++) {
            if (! (nodes.item(i) instanceof Element)) continue;
            Element e = (Element) nodes.item(i);
            factors.add(e);
        }                    
        if (factors.size()==0) {
            //A dimensionless number
            Element unit = cmlunit.getOwnerDocument().createElement("unit");
            cmlunit.appendChild(unit);
            unit.setAttribute("multiplier", "1");
            unit.setAttribute("units", "dimensionless");
        }
        else {
            String firstMultiplier = "";
            int firstunit=0;
            Element factor = factors.get(0);
            if (factor.getTagName().equals("realFactor")) {
                firstMultiplier = factor.getAttribute("multiplier");
                firstunit=1;
                //System.out.println("Multiplier = " + firstMultiplier.toString());
            }
            if (factors.size()==firstunit) {
                //No unitFactors at all, just a realFactor.
                Element unit = cmlunit.getOwnerDocument().createElement("unit");
                cmlunit.appendChild(unit);
                unit.setAttribute("multiplier", firstMultiplier.toString());
                unit.setAttribute("units", "dimensionless");
            }
            else {
                createUnitWithFactors(cmlunit, factors, firstunit, firstMultiplier);
            }
        }
    }

    private void createUnitWithFactors(Element cmlunit, ArrayList<Element> factors,
                                       int firstunit, String firstMultiplier) {
        for (int f=firstunit; f<factors.size(); f++) {
            Element factor = factors.get(f);
            String exp = factor.getAttribute("exponent");
            String unitId = factor.getAttribute("unitID");
            String cellmlUnitId = makeCellMLIdFrom(unitId);
            addCellMLJSimConversion(cellmlUnitId, unitId);
            Element unit = cmlunit.getOwnerDocument().createElement("unit");
            cmlunit.appendChild(unit);
            unit.setAttribute("units", cellmlUnitId);
            unit.setAttribute("exponent", exp);
            if (f==firstunit && !firstMultiplier.equals("")) {
                unit.setAttribute("multiplier", firstMultiplier);
            }
        }
    }

    //Looks up on the hash map whether an CellML unit exists for a given
    // JSim unit.  If so, it is returned, and if not returns null.
    protected String toCellMLFromJSim(String jsimunit) {
        //System.out.println("Finding CellML version of " + jsimunit + ": " + mJSimToCellML.get(jsimunit));
        return mJSimToCellML.get(jsimunit);
    }

    //Tells the converter that we have successfully created a new CellML/JSim
    // unit for an existing JSim/CellML unit, and adds it to the lookup table.
    private void addCellMLJSimConversion(String cellml, String jsim) {
        mJSimToCellML.put(jsim, cellml);
    }

    private boolean isTranslatedUnit(String unit) {
        return mJSimToCellML.containsValue(unit);
    }

    //Limitations on CellML ids (same as SBML) are that they must contain only letters,
    // numbers, and '_', and that they must only start with letters or '_'.  There is
    // no such restriction for JSim ids, so when you have a JSim ID, you have to mangle
    // it to create a valid CellML id.  This routine tries to do so in a semantic-preserving
    // way unique to unit ids.
    private String makeCellMLIdFrom(String jsimunit){
        //System.out.println("Converting " + jsimunit);
        String cellml = mJSimToCellML.get(jsimunit);
        if (cellml != null) return cellml;
        cellml = jsimunit;
        cellml = cellml.replace(" ", "");
        cellml = cellml.replace("/","_per_");
        cellml = cellml.replace("^-1","_inv");
        cellml = cellml.replace("^-", "_inv");
        cellml = cellml.replace("^","");
        cellml = cellml.replace("*","_x_");
        cellml = cellml.replace("-","_neg_");
        //Any other non-CellML-legal charcters, just replace with an underscore
        cellml = cellml.replaceAll("[^A-Za-z0-9_]", "_");
        //If we start with a digit, don't:
        if (cellml.matches("^[0-9].*")) {
            cellml = "_" + cellml;
        }
        //System.out.println("Final version '" + cellml + "'");
        return cellml;
    }

}
