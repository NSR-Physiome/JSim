/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MathML test harness

package JSim.tests; import JSim.mathml.*;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.Node; 
import org.w3c.dom.NodeList; 
import org.w3c.dom.CharacterData; 
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class MLLambdaTest implements MLNameSpace {
	private NamedList exprs;

	public static void main(String[] args) throws Exception {
	    if (args.length < 1) throw new Xcept(
	    	"Usage: MLMath filename");
	    File file = new File(args[0]);
	    StringList names = new StringList();
	    for (int i=1; i<args.length; i++)
	    	names.add(args[i]);
	    String text = UtilIO.readText(file);
	    MLMath math = new MLMath(text);
	    System.out.println("Extern names=" + names);
	    
	    MLCSymbol delay = new MLCSymbol("delay") {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
		    String s = "" + args.expr(0) + 
		    	"(t-(" + args.expr(1) + "))";
		    return new NamedRealConst(s, Double.NaN);
		}
	    };
	    MLCSymbol.NList csyms = new MLCSymbol.NList();
	    csyms.add(delay);
	    
	    MLLambdaTest test = new MLLambdaTest(names);

	    MLLambda lambda = math.makeLambda(test, csyms);
	    System.out.println("Lambda: " + lambda);
	}
	

	// Constructor
	public MLLambdaTest(StringList names) {
	    exprs = new NamedList();
	    for (int i=0; i<names.size(); i++)
		exprs.add(new NamedRealConst(names.str(i), Double.NaN));
	}
	    

	// NAMESPACE METHODS
	public Expr compByName(String name) throws Xcept {
	    return (Expr) exprs.getByName(name);
	}
	public String compNameByElement(Element elem) throws Xcept {
	    return UtilXML.getText(elem);
	}
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
	    throw new Xcept("makeDeriv() not implemented");
	}
	public Unit unitByName(String name) throws Xcept {
	    return Unit.scalar();
	}
	public Expr funcCall(String name, Expr.List elist) throws
	Xcept {
	     throw new Xcept("funcCall() not implemented");
	}
}
