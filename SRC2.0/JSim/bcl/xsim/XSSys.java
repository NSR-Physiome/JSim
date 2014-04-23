/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XSim model wrapper

package JSim.bcl.xsim;

import java.io.PrintStream;
import java.net.URL;
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class XSSys extends Sys {
	protected XSLib lib; 	// mainline library
	protected Ivar t;	// XSim ivar
	private Expr.List tlist; // ivar only list	

	// constructor
	public XSSys(Comp p, String n, Expr.List e) throws Xcept {
            super(p, n, e);
        }

	// t-only Expr list,  for variable construction
	public Expr.List tlist() throws Xcept {
	    if (tlist == null) {
	    	tlist = new Expr.List(1);
	    	tlist.add(t);
	    }
	    return tlist;
	}

	// write Flat
	public void writeFlat(PrintStream out, Context ctxt) throws Xcept {
	    // expand child derivatives required before processing
	    expandChildDerivs();

	    // create XS variable lists
	    int maxloc = 0;
	    Var.List vsin = new Var.List(32); // static inputs
	    Var.List vsout = new Var.List(32); // static outputs
	    Var.List vdin = new Var.List(32); // dynamic inputs
	    Var.List vdout = new Var.List(32); // dynamic outputs
	    vsin.add(t.vmin);
	    vsin.add(t.vmax);
	    vsin.add(t.vdelta);
	    for (int i=0; i<child.size(); i++) {
		if (! (child.comp(i) instanceof Var)) continue;
		Var v = (Var) child.comp(i);
		Var.List list = null;
		if (v instanceof XSVar.RealInput ||
		v instanceof XSVar.IntInput ||
		v instanceof XSVar.ChoiceInput ||
		v instanceof Ivar) {
		    list = v.hasDomain(t) ? vdin : vsin;
		} else if (v instanceof XSVar.RealOutput) {
		    list = v.hasDomain(t) ? vdout : vsout;
		}
		if (list == null) continue;
		int loc = loc(v); 
		if (loc > maxloc) maxloc = loc;
		list.add(v);
	    }
	    Var.List vinit = new Var.List(32); // xsinit vars
	    vinit.addAll(vdin);
	    vinit.addAll(vsin);

	    // write xsinit maincode
	    out.println("");
	    out.println("source procedure xsinit(" +
		fargList(vinit) +
		"; xsInitFlag) {");
	    out.println("\tlanguage = \"java\";");
	    out.println("\tmaincode={{");
	    out.println("\t    xsmodel.setModel(model);");
	    for (int i=0; i<vinit.size(); i++) 
		out.println("\t    " + setPCode(vinit.var(i)));
	    out.println("\t    xsmodel.init();");
	    out.println("\t    xsInitFlag.set(1);");
	    out.println("\t}};");

	    // write xsinit bottomcode (HACK)
	    out.println("\tbottomcode={{");
	    out.println("\t    } // drop out of JSxsinit__class");
	    out.println("\t    public static RTXSimModel xsmodel;");
	    out.println("\t    static { xsmodel = new JSX" +
		lib.libName() + "(); }");
	    out.println("\t    public void xsinitDummy() {" + 
		" // absorb dangling right brace");
	    out.println("\t}};");
	    out.println("}");

	    // write xsloop
	    out.println("");
	    out.println("source procedure xsloop(xsInitFlag, " +
		fargList(vdin) +
		"; xsLoopFlag, " +
		fargList(vdout) +
		") {");
	    out.println("\tlanguage = \"java\";");
	    out.println("\tmaincode={{");
	    for (int i=0; i<vdin.size(); i++) 
		out.println("\t    " + setPCode(vdin.var(i)));
	    out.println("\t    xsmodel.loop();");
	    out.println("\t    xsLoopFlag.set(1);");
	    for (int i=0; i<vdout.size(); i++) 
		out.println("\t    " + getPCode(vdout.var(i)));
	    out.println("\t}};");
	    out.println("}");

	    // write xscomp maincode
	    out.println("");
	    out.print("source procedure xscomp(" +
		"xsLoopFlag; xsCompFlag");
	    if (vsout.size() > 0) 
		out.print("," + fargList(vsout));
	    out.println(") {");
	    out.println("\tlanguage = \"java\";");
	    out.println("\tmaincode={{");
	    out.println("\t    xsmodel.comp();");
	    out.println("\t    xsCompFlag.set(1);");
	    for (int i=0; i<vsout.size(); i++) 
		out.println("\t    " + getPCode(vsout.var(i)));
	    out.println("\t}};");
	    out.println("}");

	    // write MathSys
	    out.println("");
	    out.println("math " + name() + " {");
	    super.writeFlat(out, ctxt);

	    // xsim special variables/func calls
	    out.println("");
	    out.println("\tprivate real xsInitFlag, xsLoopFlag(" +
		t.name() + "), xsCompFlag;");
	    out.println("\twhen (" + t.name() + "=" +
		t.name() + ".min) xsinit(" +
		fcallList(vinit) + ", xsInitFlag);");
	    out.println("\txsloop(xsInitFlag, " +
		fcallList(vdin) + 
		", xsLoopFlag, " +
		fcallList(vdout) + ");");
	    out.print("\txscomp(xsLoopFlag(" +
		t.name() + ".max), xsCompFlag");
	    if (vsout.size() > 0)
		out.print("," + fcallList(vsout));
	    out.println(");");
	    out.println("}");	// terminate MathSys

	    // write external java code
	    String res = "JSXWrapJ.txt";
	    URL url = getClass().getResource(res);
	    if (url == null) throw new Xcept(this,
		"XSim wrapper resource \"" + res + "\" not found");
	    String txt = UtilIO.readText(url);
	    txt = txt.replaceAll("proto", lib.libName());
	    out.println("extern java {{");
	    out.println(txt);
	    out.println("}};");
	}

	// setP for a variable
	private String setPCode(Var v) throws Xcept {
	    Domain x = xdomain(v);
	    if (x == null) return 
		"xsmodel.setP(" + 
		(loc(v)-1) + 
		", " + fargName(v) + ".realVal());";
	    else return
		"xsmodel.setP(" +
		(loc(v)-1) +
		", Math.min(" +
		fargName(v) + 
		".nsamples()," + 
		maxDim(v) +
		"), " +
		locincr(v) +
		", " + 
		fargName(v) + 
		".samples());";
	}
	    
	// getP for a variable
	private String getPCode(Var v) throws Xcept {
	    Domain x = xdomain(v);
	    if (x == null) return 
		fargName(v) +
		".set(xsmodel.getP(" + 
		(loc(v)-1) + 
		"));";
	    else return 
		fargName(v) +
		".setSome(" +
		"xsmodel.getP(" + 
		(loc(v)-1) +
		", Math.min(" +
		fargName(v) + 
		".nsamples()," + 
		maxDim(v) +
		"), " +
		locincr(v) +
		"));"; 
	}

	// function argument name for variable
	private String fargName(Var v) {
	    return "xs" + v.toString().replace('.', '_');
	}

	// function argument name for variable
	private String fargAtName(Var v) throws Xcept {
	    String n = fargName(v);
	    Domain x = xdomain(v);
	    if (x != null) 
		n = n + "@" + fargName(x);
	    return n;
	}

	// function argument list
	private String fargList(Var.List vlist) throws Xcept {
	    StringBuffer s = new StringBuffer();
	    for (int i=0; i<vlist.size(); i++) {
		if (i>0) s.append(",");
		if (i>0 && i%6==0) s.append("\n");
		s.append(fargAtName(vlist.var(i)));
	    }
	    return s.toString();
	}

	// function call list
	private String fcallList(Var.List vlist) throws Xcept {
	    StringBuffer s = new StringBuffer();
	    for (int i=0; i<vlist.size(); i++) {
		if (i>0) s.append(",");
		if (i>0 && i%6==0) s.append("\n");
		Var v = vlist.var(i);
		s.append(v.toString());
		Domain x = xdomain(v);
		if (x == null) continue;
		s.append("@" + x.toString());
	    }
	    return s.toString();
	}

	// X domain for variable
	protected Domain xdomain(Var v) throws Xcept {
	    Domain x = null;
	    for (int i=0; i<v.ndim(); i++) {
		if (v.domain(i) == t) continue;
		if (x != null) throw new Xcept(v,
		    "Too many domains for XSim variable");
		x = v.domain(i);
	    }
	    return x;
	}

	// loc increment
	private int locincr(Var v) throws Xcept {
	    return intVal(v, "locincr", 1);
	}

	// max dimension
	private int maxDim(Var v) throws Xcept {
	    return intVal(v, "dim", 0);
	}

	// loc value for variable
	private int loc(Var v) throws Xcept {
	    if (v.getParent() == t) {
		if (v == t.vmin) return loc(t)-2;
		if (v == t.vmax) return loc(t)-1;
		if (v == t.vdelta) return loc(t)+1;
	    }
	    return intVal(v, "loc", 0);
	}

	// get integer value for P-array associated variable property
	private int intVal(Var v, String p, int def) throws Xcept {
	    CompProp prop = v.prop(p);
	    if (prop == null) throw new Xcept(v,
		"no such property <" + p + ">");
	    if (prop.isSet()) return (int) prop.constRealVal();
	    if (def != 0) return def;
	    throw new Xcept(prop,
		"property not defined");
	}

}
