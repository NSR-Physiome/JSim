import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.jruntime.*;

// 1-dimensional integrator
public class EBintegrate extends RTXFunc {

	// constructor
    	public EBintegrate(RTModel m, String n) 
	    throws Xcept { super(m, n); }

	// return planning info
	public static Info getInfo(Expr.List args) throws Xcept {
	    if (args.size() != 1)
		throw new Xcept("EBinverse requires 1 argument");
	    Info info = new Info();
	    info.ninputs = 1;
	    info.dataType = Expr.REAL;
	    return info;
	}

	// return real value
	public double realCalculate(RealNData[] args) throws Xcept {
//	    System.err.println("EBintegrate.calculate()");

	    // check args
	    if (args.length != 1) 
		throw new Xcept(this, "requires 1 argument");
	    RealNData u = args[0];
	    if (u.ndim() != 1) throw new Xcept(this,
		"EBintegrate only supports 1 dimensional vars");
	    RegularGridData t = (RegularGridData) u.grid(0);
	    int[] inx = new int[1];

	    // calculate
	    double tot = 0;
	    for (int i=0; i<t.ct(); i++) {
		int mult = 2;
		if (i==0 || i==t.ct()-1) mult = 1;
		inx[0] = i;
		tot += mult*u.realVal(inx);
	    }
	    tot *= (t.max() - t.min()) / (2*(t.ct() - 1));
	    return tot;
	}
}		


