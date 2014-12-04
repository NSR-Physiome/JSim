/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// run-time calculable derivative of u:v

package JSim.jruntime; 
import JSim.util.*;
import JSim.expr.*;


public class RTDeriv extends Expr {

	// static creation method
	public static Expr create(RTModel model, Expr u, Expr v)
	throws Xcept {
	    if (u instanceof RTVar && v instanceof RTRealDomain) 
		return model.getVar("" + u + ":" + v);
	    return new RTDeriv(model, u, v);
	}

	// instance vars
	public RTModel model;  // for this model
	public Expr u;	// u in u:v
	public Expr v;  // v in u:v 

	// constructor
	public RTDeriv(RTModel m, Expr uu, Expr vv) throws Xcept {
	    model = m;
	    u = uu;
	    v = vv;
	    if (v instanceof RTVar && ! ((RTVar) v).isInput())
		throw new Xcept(this,
		"Ad hoc run-time derivatives must be WRT input variable");
	}

	// identical expression
	public boolean sameAs(Expr e) {
	    if (! (e instanceof RTDeriv)) return false;
	    RTDeriv d = (RTDeriv) e;
	    return u.sameAs(d.u) && v.sameAs(d.v);
	}

	// add vars/domains
        public void addNamedExpr(Expr.List list) throws Xcept {
	    u.addNamedExpr(list);
	    v.addNamedExpr(list);
	}	    
        public void addDomains(Expr.List list) {
	    u.addDomains(list);
	    v.addDomains(list);
	}	    

	// real value
	public int dataType() { return Expr.REAL; }
	public double realVal(Context ctxt0) throws Xcept {
	    RTContext ctxt = (RTContext) ctxt0;
	    double f0 = u.realVal(ctxt);

	    // find store for par p
	    RTDataStore pstore = model.sensStore(v.toString());
	    if (pstore == null) return Double.NaN; // Xcept?

	    // get f1 = perturbed f value
	    ctxt = new RTContext(ctxt, pstore);
	    double f1;
	    try {
		f1 = u.realVal(ctxt);
	    } catch (Xcept e) { 
		f1 = Double.NaN;
	    }

	    // calculate sensitivity
	    return (f1-f0) / pstore.sensDelta();
	}


 	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr u1 = u.unitCorrect();
	    Expr v1 = v.unitCorrect();
	    return new RTDeriv(model, u1, v1);
	}

	// derived unit
	public Unit unit()  { 
	    Unit uunit = u.unit();
	    Unit vunit = v.unit();
	    if (uunit == null) uunit = Unit.scalar();
	    if (vunit == null) vunit = Unit.scalar();
	    try {
		return uunit.deriv(vunit); 
	    } catch (Xcept e) {
		return null;
	    }
	} 

	// useless/unimplements Expr stuff
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, "takeDeriv not implemented");
	} 
	public Expr expandDeriv() throws Xcept {
	    throw new Xcept(this, "expandDeriv not implemented");
	} 
	public Expr simplify() throws Xcept {
	    throw new Xcept("RTDeriv.simplify() not implemented");
	}

	// string rep
	public String toString() { 
	    return "" + u + ":" + v;
	}
	public String toString(Context ctxt) {
	    return toString();
	}
}
