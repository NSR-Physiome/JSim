import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.jruntime.*;

// square a value
public class EBsquare extends RTXFunc {

	// constructor
    	public EBsquare(RTModel m, String n) 
	    throws Xcept { super(m, n); }

	// return planning info
	public static Info getInfo(Expr.List args) throws Xcept {
	    if (args.size() != 1)
		throw new Xcept("EBsquare requires 1 argument");
	    Info info = new Info();
	    info.ninputs = 1;
	    info.dataType = Expr.REAL;
	    return info;
	}

	// calculate value, 
	public double realCalculate(RealNData[] args) throws Xcept {
//	    System.err.println("EBsquare.calculate()");

	    // check args
	    if (args.length != 1) 
		throw new Xcept(this, "requires 1 arguments");
	    RealNData u = args[0];
	    if (u.ndim() != 0) throw new Xcept(this,
		"EBsquare only supports scalars");

	    // calculate
	    double uval = u.realVal();
	    return uval*uval;
	}
}		


