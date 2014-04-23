/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Context for rendering model code

package JSim.expr;
import JSim.util.*;

import java.util.Random;

abstract public class Context {
	public Lang lang;	 // language to use
	public boolean unitConst; // united constants ?
	public Random random;    // random number generator

	// constructor
	public Context(Lang l) {
	    lang = l;
	    unitConst = false;
	}

	// format real value with unit
  	public String toString(double value, Unit u) {
	    String sv = toString(value);
	    if (unitConst && !Unit.same(u, Unit.scalar()))
	    	return "(" + sv + " " + u.name + ")";
	    else
	    	return sv;
	}
		
	// format real value
	public String toString(double value) {
	    return Util.pretty(value, true);
	}
	    
	// name for variable in this context
	abstract public String newName(Named n);

	// function call
	abstract public String funcCall(Named n, Expr.List elist);

	// unit casting
	abstract public String unitCast(UnitCast cast);

	// random numbers
	public void setRandomSeed(long seed) {
	    random = seed > 0 ? 
	    	new Random(seed) : new Random();
	}
	public double random() {
	    if (random == null)
	    	random = new Random();
	    return random.nextDouble();
	}
	public double randomg() {
	    if (random == null)
	    	random = new Random();
	    return random.nextGaussian();
	}
	   
}
