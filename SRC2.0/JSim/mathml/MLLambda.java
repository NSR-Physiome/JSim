/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MathML lambda (function) construct

package JSim.mathml;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.Node; 
import org.w3c.dom.NodeList; 
import org.w3c.dom.CharacterData; 
import org.w3c.dom.Text; 
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class MLLambda implements MLNameSpace {
	private NameSpace nsParent; // parent namespace
	private String name; // use assigned
	private ArrayList<NamedExpr> args; // NamedExpr arguments
	private Expr eval;  // evaluator
	
	// constructor
	public MLLambda(NameSpace ns) {
	    nsParent = ns;
	    args = new ArrayList<NamedExpr>();
	}

	// add stuff
	public void setName(String name) { this.name = name; }
	public void addArg(String n) throws Xcept { 
	    if (arg(n) != null) throw new Xcept(
	    	"Duplicate lambda argument name: " + n);
	    args.add(new NamedRealConst(n, 0)); 
	}
	public void setEval(Expr eval) { this.eval = eval; }

	// query
	public int nargs() { return args.size(); }
	public NamedExpr arg(int i) {
	    return args.get(i); 
	}
	public NamedExpr arg(String n) {
	    for (int i=0; i<args.size(); i++) 
	    	if (arg(i).name().equals(n))
		    return arg(i);
   	    return null;
	}
	public Expr.List args() {
	    Expr.List list = new Expr.List(nargs());
	    for (int i=0; i<args.size(); i++)
	    	list.add(arg(i));
	    return list;
	}
	public Expr eval() { return eval; }
	public String name() { return name; }
	public String toString() {
	    String f = (name == null) ? "lambda" : name;
	    return f + args + "=" + eval;
	}
	public String diagInfo() { return "MathML lambda: " + this; }

	// namespace query
	public Expr compByName(String name) throws Xcept {
	    Expr e = arg(name);
	    if (e == null) 
	    	e = nsParent.compByName(name);
	    if (e == null) throw new Xcept(
	    	"<lambda> can't find variable " + name);
	    return e;
	}
	public String compNameByElement(Element e) throws Xcept {
	    return UtilXML.getText(e);
	}
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
	    return nsParent.makeDeriv(e1, e2);
	}
	public Unit unitByName(String name) throws Xcept {	
	    return nsParent.unitByName(name);
	}
	public Expr funcCall(String name, Expr.List elist) 
	throws Xcept {	
	    return nsParent.funcCall(name, elist);
	}

}
