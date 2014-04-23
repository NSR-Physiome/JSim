/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// common code for based external function with MML defined args

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.lang.reflect.*;
import java.util.ArrayList;

abstract public class CommonFunc extends XFunc {

	// instance vars
	private int dataType; // REAL or VOID
	protected Par.List pars; // func dummy pars
	private int ninputs;	// # input pars
	private StringList pardoms; // dummy domains

	// constructor
	public CommonFunc(String n, int ftype, int dtype, StringList pstr) 
	throws Xcept {
	    super(n, ftype);
	    dataType = dtype;

	    // build/verify parm config
	    pars = new Par.List(pstr.size());
	    pardoms = new StringList(2);
	    boolean input = true;
	    for (int i=0; i<pstr.size(); i++) {
		String p = pstr.str(i);
		if (p == null) {
		    if (input) 
			input = false;
		    else throw new Xcept(this,
			"illegal parameter list");
		    continue;
		}
		Par par = new Par(this, p);
		pars.add(par);
		for (int j=0; j<par.ndim; j++) 
		    pardoms.add(par.doms.str(j));
		if (input) ninputs++;
	    }
	}

	// add code
	abstract public void addCode(String n, String code) 
	throws Xcept;

	// create function call
	public XFuncCall createCall(Expr.List args) throws Xcept {
	    if (args.size() != nargs()) throw new Xcept(this,
		"requires " + nargs() + " arguments");
	    XFuncCall fc = new XFuncCall(this, args, dataType, ninputs);

	    // cross check fc.args() domains agains pardoms
	    XFuncArg.List fcargs = fc.args();
	    Expr[] fcdoms = new Expr[pardoms.size()];
	    for (int i=0; i<pars.size(); i++) {
		Par par = pars.par(i);
		XFuncArg fcarg = fcargs.arg(i);
		for (int j=0; j<par.ndim; j++) {
		    String pardom = par.doms.str(j);
		    int inx = pardoms.indexOf(pardom);
		    if (j>=fcarg.argDoms().size()) throw new Xcept(this,
			"missing domain \"" + pardom + "\" in function call");
		    Expr fcargdom = fcarg.argDoms().expr(j);
		    if (fcdoms[inx] == null) fcdoms[inx] = fcargdom;
		    if (! fcargdom.sameAs(fcdoms[inx])) throw new Xcept(this,
			"inconsistent domain \"" + pardom + 
			"\" definition in function call");
 		}
	    }

	    return fc;
	}

	// query
	public int dataType() { return dataType; }
	public int nargs() { return pars.size(); }
	public int ninputs() { return ninputs; }
	public String parName(int i) { return pars.par(i).name; }

	// write flat 
	public String flatHdr() {
	    String hdr = (this instanceof NativeFunc) ?
		"native " : "source ";
	    hdr = hdr + ((dataType == Expr.REAL) ? 
		"real function " : "procedure ");
	    hdr = hdr + name + " (";
	    for (int i=0; i<pars.size(); i++) {
		if (i == ninputs)
		    hdr = hdr + ";";
		else if (i>0)
		    hdr = hdr + ",";
		hdr = hdr + pars.par(i).desc;
	    }
	    hdr = hdr + ") {";
	    return hdr;
	}


	// Parameter class
	public static class Par {
	    public CommonFunc func;
	    public String desc;
	    public String name;
	    public int ndim;
	    public StringList doms;

	    public Par(CommonFunc f, String d) throws Xcept { 
		func = f; 
		desc = d;
		name = desc;
		doms = new StringList(2);
		
		while (name.lastIndexOf('@')>0) {
		    int inx = name.lastIndexOf('@');
		    ndim++;
		    String dom = name.substring(inx+1);
		    doms.add(dom);
		    name = name.substring(0, inx);
		}

		if (doms.size() != ndim) throw new Xcept(f,
		    "duplicate @domain in function or procedure declaration");
	    }

	    public static class List extends ArrayList<Par> {
		public List(int n) { super(n); }
		public Par par(int i) { return (Par) get(i); }
	    }
	}
}
