/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// antimony block

package JSim.mml; 

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*;

import java.io.PrintStream;
import java.util.ArrayList;

public class AntimonySys extends Sys {
	public String text; // antimony text

	// constructors
	public AntimonySys(Comp p, String n, Expr.List e) throws Xcept {
            super(p, n, e);
	    StringConst s = (StringConst) e.expr(0);
	    text = s.stringVal(null);
        }

	// write Flat
	public void writeFlat(PrintStream out, Context ctxt)
	throws Xcept {
	    Model.Translator translator = getModel().translator;
	    if (translator == null) throw new Xcept(
	    	"Model.Translator needed for Antimony model code is not available.");
	    String mml = translator.antimony2MML(text);
	    out.println(mml);
	}
	    
}

