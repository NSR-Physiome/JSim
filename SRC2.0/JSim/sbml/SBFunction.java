/*NSRCOPYRIGHT
  Copyright (C) 1999-2018 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML function for import

package JSim.sbml;

import JSim.util.*;
import JSim.expr.*;
import JSim.mathml.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 
import org.sbml.libsbml.*;

public class SBFunction implements Named {
    protected SBModel sbmodel;
    protected FunctionDefinition func;
    protected String name;
    protected MLLambda lambda;
    protected int ncalls;

    // constructor
    public SBFunction(SBModel sm, FunctionDefinition c) throws Xcept {
        sbmodel = sm;
        func = c;
        name = func.getId();
        String text = libsbml.writeMathMLToString(func.getMath());
        MLMath math = new MLMath(text);
        lambda = math.makeLambda(sbmodel, sbmodel.csymbols);
        ncalls = 0;
    }
    
    // create call
    protected Expr makeCall(Expr.List args) throws Xcept {
        if (lambda.nargs() != args.size()) throw new Xcept
            (this, "Function requires " + lambda.nargs() + " arguments, "
             + args.size() + " were found.");
        String vname = name + "_call" + ncalls++;
        SBVar v = new SBVar(sbmodel, vname, "functionCall");
        v.setPrivate(true);
        Expr expr = lambda.eval();
        expr = expr.replace(lambda.args(), args);
        String vassn = expr.toString(sbmodel.ctxt);
        v.setAssign(vassn);
		if (this.func.isSetNotes()) {
				SBNotes newNote = new SBNotes(this.func.getNotesString());
				newNote.removeXMLTags();
				newNote.addCommentIdentifiers();
				v.setNotes(newNote.getNote());
			}

        return new NamedRealConst(vname, 0);
    }

    // query
    public String name() { return name; }
    public String diagInfo() { return 
            "Function definition " + name + lambda.args() + 
            "=" + lambda.eval(); }

    // write MML
    public void writeMML(PrintWriter wrt) {
        wrt.println("  // " + diagInfo() + ";");
    }

    // SBFunction.List
    public static class NList extends NamedList {
        public NList() { super(); }
        public NList(SBModel sbm) throws Xcept { 
            super();
            long nfuncs = sbm.model.getNumFunctionDefinitions();
            for (int i=0; i<nfuncs; i++) {
                try {
                    add(new SBFunction(sbm, sbm.model.getFunctionDefinition(i)));
                }
                catch(Xcept e) {
                    FunctionDefinition fd = sbm.model.getFunctionDefinition(i);
                    String infix = libsbml.formulaToString(fd.getMath());
                    String error = e.getMessage();
                    error = error.replaceAll("\\n"," ");
                    sbm.addWarning("MathML error:  " + error);
                    sbm.addWarning("Unable to create function definition '" + fd.getId() + "':  " + infix);
                }
            }
        }
        public SBFunction sbfunc(int i) {
            return (SBFunction) get(i);
        }
        public SBFunction sbfunc(String n) {
            return (SBFunction) getByName(n);
        }
    }
}
