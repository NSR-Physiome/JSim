// write java code for any RTProblem

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

abstract public class JProblemWriter extends JMethodWriter {

	// constructor
	public JProblemWriter(JMethodWriter parent, SeqItem item) 
	throws Xcept {
	    super(parent, item);
	}

	// write problem object declaration
	public void writeDecl() throws Xcept {
	    println("public " + problemClass() + " " + 
	        problemObject() + ";");
	}

	// write problem object instantiation
	public void writeInst() throws Xcept {
	    println(problemObject() + " = new "
	        + problemClass() + "(this);");
	}

	// write solve call
	public void writeSolverCall() throws Xcept {
	    println(problemObject() + ".solve(this);");
	}

	// write problem class
	abstract public void writeClass() throws Xcept;

	// simple queries
	abstract public String extendsClass();
	public String problemClass() { 
	    return extendsClass() + "__" + id;
	}
	public String problemObject() { 
	    return problemClass().toLowerCase(); 
	}

	// write ctxt call
	public void writeCtxtCall(String name, String args, String
	types) throws Xcept {
	    startMethod(name, "ctxt," + args, "RTContext," + types);
	    println("((XContext) ctxt)." + name + "__" + id + 
	    	"(" + args + ");");
	    stopMethod();
	}
}


