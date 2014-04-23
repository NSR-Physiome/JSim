import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.jruntime.*;

// reverse a time series
public class EBreverse extends RTXFunc {

	// constructor
    	public EBreverse(RTModel m, String n) 
	    throws Xcept { super(m, n); }

	// return planning info
	public static Info getInfo(Expr.List args) throws Xcept {
	    if (args.size() != 2)
		throw new Xcept("EBreverse requires 2 arguments");
	    Info info = new Info();
	    info.ninputs = 1;
	    info.dataType = Expr.VOID;
	    return info;
	}

	// calculate value, 
	public void voidCalculate(RealNData[] args) throws Xcept {

//	    System.err.println("EBreverse.calculate()");

	    // check args
	    if (args.length != 2) 
		throw new Xcept(this, "requires 2 arguments");
	    RealNData u = args[0];
	    RealNData v = args[1];
	    if (u.ndim() != 1 || v.ndim() != 1) throw new Xcept(this,
		"EBsquare only supports 1 dimensional vars");
	    RegularGridData t = (RegularGridData) u.grid(0);
	    RegularGridData tv = (RegularGridData) v.grid(0);
	    if (! t.sameAs(tv)) throw new Xcept(this,
		"parameter grid mismatch");
	    int[] inx = new int[1];

	    // calculate v
	    for (int i=0; i<t.ct(); i++) {
		inx[0] = i;
		double uval = u.realVal(inx);
		inx[0] = t.ct()-i-1;
		v.set(inx, uval);
	    }
	}
}		


