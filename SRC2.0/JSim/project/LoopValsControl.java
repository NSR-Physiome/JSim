/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// comma separated loop values

package JSim.project;

import java.util.StringTokenizer;
import JSim.util.*;
import JSim.expr.*;

public class LoopValsControl extends StringControl {
	static final public int EMPTY = 0;
	static final public int LIST = 1000;
	static final public int BAD = -1;

	// instance
	private ModelParControl parCntl; // which par to loop
	private int op;		 // ADD, SUB, MULT, DIV or above constants
	private double factor;   // if op=ADD, SUB, MULT or DIV
	private String[] listVals; // alt values,  if LIST
	private String[] runVals; // run-time assignments

	// constructor
	public LoopValsControl(PNamed p, String n, ModelParControl pc) throws Xcept {
	    super(p, n);
	    parCntl = pc;
	    op = EMPTY;
	}

	// update internal values
	public void revalidate() {
	    valid = false;
	    validMsg = "";
	    op = BAD;
	    listVals = null;

	    // blank field
	    if (isBlank()) {
		op = EMPTY;

	    // starts with @
	    } else if (value.charAt(0) == '@') {
		switch (value.charAt(1)) {
		case '+': op = IExpr.ADD; break;
		case '-': op = IExpr.SUB; break;
		case '*': op = IExpr.MULT; break;
		case '/': op = IExpr.DIV; break;
		default: 
		    validMsg = "unsupported @ operator (expected + - * or /)";
		    return;
		}
		try {
		    factor = getNumber(value.substring(2));
		    valid = true;
		} catch (Xcept e) {
		    validMsg = e.getMessage();
		}

	    // list of values
	    } else {
		op = LIST;
	    	StringTokenizer stok = new StringTokenizer(value, ",");
	    	listVals = new String[stok.countTokens()];
	    	for (int i=0; i<listVals.length; i++) 
		   listVals[i] = stok.nextToken();
		valid = true;
	    }
	}

	// get number
	private double getNumber(String s) throws Xcept {
	    try {
		return Double.parseDouble(s);
	    } catch (NumberFormatException e) {
		throw new Xcept("Expected number,  found \"" + s + "\"");
	    }
	}

	// # values if LIST,  otherwise 0
	public int listSize() { 
	    if (op != LIST) return 0;
	    return listVals.length + 1;
	}

	// create runVals array before run,  do error checking
	public void runPrep(int nloops) throws Xcept {
	    runVals = null;
	    if (parCntl.isBlank() && op == EMPTY) return;
	    Control par = parCntl.control();
	    if (par == null) throw new Xcept(parCntl,
		"loop parameter invalid");
	    
	    // initialize runVals with current value
	    runVals = new String[nloops];
	    runVals[0] = par.stringVal();
	    if (nloops == 1) return;

	    // LIST values
	    if (!valid) throw new Xcept(this,
		"loop values field empty or invalid");
	    if (op == LIST) {
	    	for (int i=1; i<nloops; i++) {
		    int j = (i-1<listVals.length) ?
			i-1 : listVals.length-1;
		    runVals[i] = listVals[j];
		}

	    // @ iterations
	    } else {
		double f = 0;
		try {
		    f = getNumber(runVals[0]);
	        } catch (Xcept e) {
		    throw new Xcept(par, 
			"loop initial values must be numeric");
		}
		for (int i=1; i<nloops; i++) {
		    switch (op) {
		    case IExpr.ADD: f += factor; break;
		    case IExpr.SUB: f -= factor; break;
		    case IExpr.MULT: f *= factor; break;
		    case IExpr.DIV: f/= factor; break;
		    }
		    runVals[i] = Util.pretty(f);
		}
	    }	    
	}

	// return ith loop value
	public String runVal(int i) {
	    if (runVals == null) return null;
	    return runVals[i];
	}
}

