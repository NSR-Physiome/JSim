/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// preview for function generator

package JSim.project; import JSim.aserver.*;
import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.fgen.*;

public class FuncGenPreview {
	private FuncGen funcGen;
	private String name;
	private FgenMaster master;
	private FgenContext ctxt;

	// constructor
	public FuncGenPreview(FuncGen f) {
	    funcGen = f;
	    name = funcGen.name();
	}
	
	// calculate preview data
	public RealNData calcData() throws Xcept {

	    // create master & context?
	    name = funcGen.name();
	    if (master == null || ! name.equals(master.name())) 
	    	master = new FgenMaster(name);
	    if (ctxt == null) ctxt = new Ctxt(); 
	    master.init(ctxt);
	    
	    // calculate data
	    return master.data();
	}

	// calculate preview grid for domain
	private GridData calcGrid(String n) throws Xcept {
	    double min = getVal(n + ".min", 0);
	    double max= getVal(n + ".max", min+30);
	    if (max <= min) max = min + 30;
	    double delta = assignVal(n + ".delta");
	    double ct = assignVal(n + ".ct");
	    if (! Double.isNaN(delta)) 
		ct = ((int) ((max-min)/delta + 1.5));
	    else if (Double.isNaN(delta) && Double.isNaN(ct))
	    	ct = finalVal(n + ".ct");
	    if (Double.isNaN(ct)) ct = 61;	    
	    if (ct < 2) ct = 2;	    	
	    return new RegularGridData(null,
		null, min, max, (int) ct);
	}

	// get preview par val
	private double getVal(String name, double def) {
	    double v = assignVal(name);
	    if (! Double.isNaN(v)) return v;
	    v = finalVal(name);
	    if (! Double.isNaN(v)) return v;
	    return def;
	}

	// current assignment value 
	private double assignVal(String name) {
	    try {
	    	ASVar v = funcGen.pmodel().rt().getASVar(name);
		String s = v.getAssign();
		if (s == null) return Double.NaN;
		return Util.toDouble(s);
 	    } catch (Xcept e) {
	    	return Double.NaN;
	    }
	}

	// final value from last run
	private double finalVal(String name) {
	    try {
	    	ASVar v = funcGen.pmodel().rt().getASVar(name);
		return v.finalRealVal();
	    } catch (Xcept e) {
	    	return Double.NaN;
	    }
	}

	// Ctxt preview context
	public class Ctxt extends FgenContext {
	    
	    // constructor
	    public Ctxt() {
	        super(JSLang.lang);
	    }
	    
	    // get named value
	    public NamedVal namedVal(String n) throws Xcept {
	    	return funcGen.parent().namedVal(n);
	    }
	    
	    // get domain
	    public NamedExpr domain(String n) throws Xcept {
	    	ASVar v = funcGen.pmodel().rt().getASVar(n);
		if (! v.isDomain()) throw new Xcept(funcGen.pmodel(),
		    "Invalid domain " + v);
		return v;
	    }
	    
	    // get preview grid
	    public GridData gdata(NamedExpr v) throws Xcept {
	        return calcGrid(v.name());
	    }
	}
}

