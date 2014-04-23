/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML unit constants

package JSim.sbml;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

import java.io.*;
import java.util.*;
import java.lang.Math.*;
import org.sbml.libsbml.*;

public class SBUnitConstants {

    // static SBML unit name lookup
    private static HashMap<String, Integer> sbmlUnitKinds;
    private static HashMap<Integer, String> sbmlUnitNames;

    // names of SBML-defined units
    public static String[] SBML_UNIT_NAMES = new String[] {
    "ampere", "becquerel", "candela", "celsius", "coulomb", 
    "dimensionless", "farad", "gram", "gray", "henry", 
    "hertz", "item", "joule", "katal", "kelvin", "kilogram", 
    "litre", "litre", "lumen", "lux", "metre", "metre", "mole", 
    "newton", "ohm", "pascal", "radian", "second", "siemens", 
    "sievert", "steradian", "tesla", "volt", "watt", "weber", 
    "invalid" 
    };

    // kind codes of SBML-defined units (match order with above)
    public static int[] SBML_UNIT_KINDS = new int[] {
    libsbmlConstants.UNIT_KIND_AMPERE,
    libsbmlConstants.UNIT_KIND_BECQUEREL,
    libsbmlConstants.UNIT_KIND_CANDELA,
    libsbmlConstants.UNIT_KIND_CELSIUS,
    libsbmlConstants.UNIT_KIND_COULOMB,
    libsbmlConstants.UNIT_KIND_DIMENSIONLESS,
    libsbmlConstants.UNIT_KIND_FARAD,
    libsbmlConstants.UNIT_KIND_GRAM,
    libsbmlConstants.UNIT_KIND_GRAY,
    libsbmlConstants.UNIT_KIND_HENRY,
    libsbmlConstants.UNIT_KIND_HERTZ,
    libsbmlConstants.UNIT_KIND_ITEM,
    libsbmlConstants.UNIT_KIND_JOULE,
    libsbmlConstants.UNIT_KIND_KATAL,
    libsbmlConstants.UNIT_KIND_KELVIN,
    libsbmlConstants.UNIT_KIND_KILOGRAM,
    libsbmlConstants.UNIT_KIND_LITER,
    libsbmlConstants.UNIT_KIND_LITRE,
    libsbmlConstants.UNIT_KIND_LUMEN,
    libsbmlConstants.UNIT_KIND_LUX,
    libsbmlConstants.UNIT_KIND_METER,
    libsbmlConstants.UNIT_KIND_METRE,
    libsbmlConstants.UNIT_KIND_MOLE,
    libsbmlConstants.UNIT_KIND_NEWTON,
    libsbmlConstants.UNIT_KIND_OHM,
    libsbmlConstants.UNIT_KIND_PASCAL,
    libsbmlConstants.UNIT_KIND_RADIAN,
    libsbmlConstants.UNIT_KIND_SECOND,
    libsbmlConstants.UNIT_KIND_SIEMENS,
    libsbmlConstants.UNIT_KIND_SIEVERT,
    libsbmlConstants.UNIT_KIND_STERADIAN,
    libsbmlConstants.UNIT_KIND_TESLA,
    libsbmlConstants.UNIT_KIND_VOLT,
    libsbmlConstants.UNIT_KIND_WATT,
    libsbmlConstants.UNIT_KIND_WEBER,
    libsbmlConstants.UNIT_KIND_INVALID
    };
  

    // static initialization
    static {
        sbmlUnitNames = new HashMap<Integer, String>();
        sbmlUnitKinds = new HashMap<String, Integer>();
        for (int i=0; i<SBML_UNIT_NAMES.length; i++) {
            String name = SBML_UNIT_NAMES[i];
            Integer code = new Integer(SBML_UNIT_KINDS[i]);
            sbmlUnitNames.put(code, name);
            sbmlUnitKinds.put(name, code);
        }
    }
        
    // SBML kind/String lookups
    public static String kindToString(int kind) {
        return sbmlUnitNames.get(new Integer(kind));
    }
    public static int stringToKind(String name) {
        Integer kind = sbmlUnitKinds.get(name);
        return (kind == null) ? 
            libsbmlConstants.UNIT_KIND_INVALID : kind.intValue(); 
    }

}
