/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// unit handling for SBML export

package JSim.sbml;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

import java.io.*;
import java.util.*;
import java.lang.Math.*;
import org.sbml.libsbml.*;
import org.w3c.dom.*;

public class SBUnitWriter {
    private SBWriter writer; 
    private HashMap<String, String> mJSimToSBML;

    // constructor
    public SBUnitWriter(SBWriter writer) throws Xcept {
        this.writer = writer;
        mJSimToSBML = new HashMap<String, String>();

        // map all unit names common to SBML & JSim
        for (int i=0; i<SBUnitConstants.SBML_UNIT_NAMES.length; i++) {
            String uname = SBUnitConstants.SBML_UNIT_NAMES[i];
            if (uname.equals("avogadro")) continue;
            if (uname.equals("item")) continue;
            addSBMLJSimConversion(uname, uname);
        }

        // special fixed additions for nsrunit - NOT GENERAL !!!
        // better would be to have a defined mapping in XMML from fundamental
        //  units to SI units.
        addSBMLJSimConversion("ampere", "amp");
        addSBMLJSimConversion("gram", "g");
        addSBMLJSimConversion("joule", "J");
        addSBMLJSimConversion("kelvin", "degK");
        addSBMLJSimConversion("hertz", "hz");
        addSBMLJSimConversion("kilogram", "kg");
        addSBMLJSimConversion("litre", "L");
        addSBMLJSimConversion("metre", "m");
        addSBMLJSimConversion("mole", "mol");
        addSBMLJSimConversion("newton", "N");
        addSBMLJSimConversion("pascal", "pa");
        addSBMLJSimConversion("second", "sec");
    }
        

    // create SBML UnitDefinitions
    protected void loadUnits() throws Xcept {
        ArrayList<Element> xunits = new ArrayList<Element>();
        NodeList nodes = xunitList().getChildNodes();
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
                processFundamentalUnit(xunit);
            }
            else if (type.equals("derivedUnit")) {
                processDerivedUnit(xunit);
            }
            else {
                warnings().add("Warning:  unknown unit list element '" + type + "'.");
            }
        }
        //System.out.println("Done with units.");
    }

    private void processFundamentalUnit(Element xunit) {
        String base = xunit.getAttribute("id");
        String type = xunit.getAttribute("unitType");
        String basesbml = toSBMLFromJSim(base);
        if (basesbml==null) {
            basesbml = makeSBMLIdFrom(base);
            UnitDefinition unitdef = model().createUnitDefinition();
            unitdef.setId(basesbml);
//		unitdef.setNotes("My unit notes..!");

            addSBMLJSimConversion(basesbml, base);
        }
    }

    private void processDerivedUnit(Element xunit) throws Xcept {
        String jsimunit = xunit.getAttribute("id");
		//       System.out.println("Calculating derived unit " + jsimunit);
        if (toSBMLFromJSim(jsimunit) != null) {
            //This unit already exists in SBML, probably as a fundamental unit.  Don't create another one.
            return;
        }
        String sbmlunit = makeSBMLIdFrom(jsimunit);
        while (model().getUnitDefinition(sbmlunit) != null) {
            sbmlunit = sbmlunit + "_";
        }
        UnitDefinition unitdef = model().createUnitDefinition();
        unitdef.setId(sbmlunit);
	 // unitdef.setNotes("Notes for a derived unit!",true);
        unitdef.setName(jsimunit);
        addSBMLJSimConversion(sbmlunit, jsimunit);
        ArrayList<Element> factors = new ArrayList<Element>();
        NodeList nodes = xunit.getChildNodes();
        for (int i=0; i<nodes.getLength(); i++) {
            if (! (nodes.item(i) instanceof Element)) continue;
            Element e = (Element) nodes.item(i);
            factors.add(e);
        }                    
        if (factors.size()==0) {
            //A dimensionless number
            org.sbml.libsbml.Unit unit = unitdef.createUnit();
            unit.setMultiplier(1);
            unit.setKind(SBUnitConstants.stringToKind("dimensionless"));
            unit.setExponent(1);
            unit.setScale(0);
        }
        else {
            Double firstMultiplier = 1.0;
            int firstunit=0;
            Element factor = factors.get(0);
            if (factor.getTagName().equals("realFactor")) {
                String mult = factor.getAttribute("multiplier");
                firstMultiplier = Double.valueOf(mult);
                firstunit=1;
                //System.out.println("Multiplier = " + firstMultiplier.toString());
            }
            if (factors.size()==firstunit) {
                //No unitFactors at all, just a realFactor.
                org.sbml.libsbml.Unit unit = unitdef.createUnit();

                unit.setMultiplier(firstMultiplier);
                unit.setKind(SBUnitConstants.stringToKind("dimensionless"));
                unit.setExponent(1);
                unit.setScale(0);
            }
            else {
                createUnitWithFactors(unitdef, factors, firstunit, firstMultiplier);
            }
        }
        if (isSkippedSBMLFundamental(sbmlunit)) {
            if (unitMatches(sbmlunit, unitdef)) {
                model().removeUnitDefinition(unitdef.getId());
            }
            else {
                sbmlunit = sbmlunit + "_";
                unitdef.setId(sbmlunit);
            }
        }
    }

    private void createUnitWithFactors(UnitDefinition unitdef, ArrayList<Element> factors,
                                       int firstunit, double firstMultiplier) {
        for (int f=firstunit; f<factors.size(); f++) {
            Element factor = factors.get(f);
            double exp = Double.parseDouble(factor.getAttribute("exponent"));
            String unitId = factor.getAttribute("unitID");
            String sbmlUnitId = makeSBMLIdFrom(unitId);
            int kind = SBUnitConstants.stringToKind(sbmlUnitId);
            //System.out.println("Unit exp = " + exp + ", id = " + unitId + ", sbml ID = " + sbmlUnitId + ", kind = " + kind);
            if (kind == org.sbml.libsbml.libsbmlConstants.UNIT_KIND_INVALID) {
                //Note:  this restriction should be lifted later.
                warnings().add("Unable to translate JSim unit " + unitId +
                               " to SBML because no corresponding SBML fundamental unit could be found.");
                model().removeUnitDefinition(unitdef.getId());
                return;
            }
            addSBMLJSimConversion(sbmlUnitId, unitId);
            org.sbml.libsbml.Unit unit = unitdef.createUnit();

            unit.setKind(kind);
            if (f==firstunit) {
                unit.setMultiplier(firstMultiplier);
            }
            else {
                unit.setMultiplier(1);
            }
            unit.setExponent(exp);
            unit.setScale(0);
        }
    }

    // query main writer
    protected Model model() { return writer.model; }
    protected StringList warnings() { return writer.warnings; }
    protected Element xunitList() { return writer.xunitList; }

    //Looks up on the hash map whether an SBML unit exists for a given JSim unit.
    // If so, it is returned, and if not returns null.
    public String toSBMLFromJSim(String jsimunit) {
        //System.out.println("Finding SBML version of " + jsimunit + ": " + mJSimToSBML.get(jsimunit));
        return mJSimToSBML.get(jsimunit);
    }

    //Tells the converter that we have successfully created a new SBML/JSim unit
    // for an existing JSim/SBML unit, and adds it to the lookup table.
    public void addSBMLJSimConversion(String sbml, String jsim) {
        mJSimToSBML.put(jsim, sbml);
    }

    //As of this writing, two SBML fundamental units have no JSim pre-defined
    // equivalent.  This function allows us to test for this condition, as well
    // as change it later if NSR units are updated (for example: to add an
    // avogadro unit).
    public boolean isSkippedSBMLFundamental(String sbmlunit) {
        if (sbmlunit.equals("avogadro")) return true;
        if (sbmlunit.equals("item")) return true;
        return false;
    }

    //Limitations on SBML ids are that they must contain only letters, numbers,
    // and '_', and that they must only start with letters or '_'.  There is no
    // such restriction for JSim ids, so when you have a JSim ID, you have to
    // mangle it to create a valid SBML id.  This routine tries to do so in a
    // semantic-preserving way unique to unit ids.  (Note: there is no such
    // restriction on the 'name' property for SBML unit objects, so the original
    // jsimunit string is used for the 'name' instead.)
    public String makeSBMLIdFrom(String jsimunit){
        //System.out.println("Converting " + jsimunit);
        String sbml = mJSimToSBML.get(jsimunit);
        if (sbml != null) return sbml;
        sbml = jsimunit;
        sbml = sbml.replace(" ", "");
        sbml = sbml.replace("/","_per_");
        sbml = sbml.replace("^-1","_inv");
        sbml = sbml.replace("^-", "_inv");
        sbml = sbml.replace("^","");
        sbml = sbml.replace("*","_x_");
        sbml = sbml.replace("-","_neg_");
        //Any other non-SBML-legal charcters, just replace with an underscore
        sbml = sbml.replaceAll("[^A-Za-z0-9_]", "_");
        //If we start with a digit, don't:
        if (sbml.matches("^[0-9].*")) {
            sbml = "_" + sbml;
        }
		//System.out.println("Final version '" + sbml + "'");
        return sbml;
    }


    //This function is called if a unit named with an SBML fundamental unit name
    // has had a UnitDefinition created for it anyway.  It is illegal to create a
    // new UnitDefinition with the same name as an SBML fundamental unit, so we need
    // to know whether the UnitDefinition we have created matches what SBML already
    // thinks this unit should be (in which case we can remove the UnitDefinition
    // entirely) or whether it's actually something new (in which case we need to rename
    // the unit id).
    public boolean unitMatches(String sbmlunit, UnitDefinition unitdef)
        throws Xcept {
        if (sbmlunit.equals("avogadro")) {
            //should be 6.02214179*10^23 dimensionless
            if (unitdef.getNumUnits() != 1) return false;
            Unit unit=unitdef.getUnit(0);
            double mult = unit.getMultiplier() * Math.pow(10.0, unit.getScale());
            if (mult != 6.02214179*Math.pow(10.0,23.0)) return false;
            if (unit.getExponent() != 1) return false;
            return (unit.getKind() ==  org.sbml.libsbml.libsbmlConstants.UNIT_KIND_DIMENSIONLESS);
        }
        if (sbmlunit.equals("item")) {
            //should be dimensionless
            if (unitdef.getNumUnits() != 1) return false;
            Unit unit=unitdef.getUnit(0);
            if (unit.getMultiplier() != 1) return false;
            if (unit.getScale() != 0) return false;
            if (unit.getExponent() != 1) return false;
            return (unit.getKind() ==  org.sbml.libsbml.libsbmlConstants.UNIT_KIND_DIMENSIONLESS);
        }
        //Unimplemented test!  Who knows!
        throw new Xcept("Programming error: unable to determine whether the fundamental SBML unit "
                        + sbmlunit + " matches the unit definition JSim created for the name.");
    }

}
