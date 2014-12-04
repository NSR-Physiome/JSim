import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.jruntime.*;

// cube a value,  but also return output arg (not supported)
public class EBcube extends RTXFunc {

	// constructor
    	public EBcube(RTModel m, String n) 
	    throws Xcept { super(m, n); }

	// return planning info
	public static Info getInfo(Expr.List args) throws Xcept {
	    if (args.size() != 2)
		throw new Xcept("EBcube requires 2 arguments");
	    Info info = new Info();
	    info.ninputs = 1;
	    info.dataType = Expr.REAL;
	    return info;
	}

	// calculate value, 
	public void voidCalculate(RealNData[] args) throws Xcept {
//	    System.err.println("EBcube.calculate()");

	    // check args
	    if (args.length != 2) 
		throw new Xcept(this, "requires 2 arguments");
	    RealNData u = args[0];
	    if (u.ndim() != 0) throw new Xcept(this,
		"EBcube only supports scalars");
	    RealNData v = args[1];
	    if (v.ndim() != 0) throw new Xcept(this,
		"EBcube only supports scalars");

	    // calculate
	    double uval = u.realVal();
	    v.set(uval*uval*uval);
	}
}		


