/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// check SBML defined unit against JSim common definition

package JSim.tests;

import java.io.File;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.sbml.*;
import JSim.lserver.*; 

public class TestSBMLUnits {

	// test harness
	public static void main(String[] args) throws Exception {
	    NamedVal.NList options = new NamedVal.NList();
	    LSServer server = new LSServer(options, null, null);
	    UnitNList units = server.getCommonUnits("nsrunit.mod");
	    SBUnitChecker checker = new SBUnitChecker(units);

	    // test isConflict
	    if (args[0].equals("-c")) {
	        boolean isConflict = checker.isConflict(args[1], args[2]);
	        System.out.println("isConflict=" + isConflict);
	    }

	    // test canonical name
	    if (args[0].equals("-s1")) {
		String name = args[1];
		String sname = checker.getCanonicalName(name);
	        System.out.println("shortestName=" + sname);
	    }

	    // test canonical name,mult
	    if (args[0].equals("-s2")) {
		String name = args[1];
		double mult = Util.toDouble(args[2]);
		String sname = checker.getCanonicalName(name, mult);
	        System.out.println("shortestName=" + sname);
	    }
	}	    
}
