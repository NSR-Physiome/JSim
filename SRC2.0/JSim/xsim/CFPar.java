/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file parameter

package JSim.xsim;

import java.io.*;
import JSim.util.*;
import JSim.expr.*;

public class CFPar extends CFItem {
	public static final int LOGICAL=1;
	public static final int CHOICE=2;
	public static final int INT=3;
	public static final int REAL=4;
	public static final int STRING=5;

	public static final int UNDECIDED = 0;
	public static final int STATIC = 1;
	public static final int DYNAMIC = 2;

	public int type;
	private String xsname; // original XSim name
	private String jsname; // JSim name,  if different
	public int loc;
	public boolean input;
	public int dynamic;
	public int dim1;
	public String init;
	public String fixeval;
	public String eval;
	public String unit;
	public StringList labels;
	public StringList help;
	public int labelBase;
	public boolean skip; // skip output generation
	public PrintStream nullErr;
	public PrintStream saveErr;

	// constructor
	public CFPar(CF c, int t, String n) {
	    super(c);
	    type = t;
	    xsname = n;
	    dim1 = 1;
	    input = true;
	    dynamic = UNDECIDED;
	    labels = new StringList(4);
	    n = safeName(xsname);
	    if (! xsname.equals(n))
		rename(n);
	    help = new StringList(4);
	    nullErr = new PrintStream(new NullOutputStream());
	    saveErr = System.err;
	}

	// rename this variable
	public void rename(String n) {
	    warn(name() + " renamed " + n);
	    jsname = n;
	    cf.renamedPars.add(this);
	}

	// set attribute
	public void set(String key, String value) {
	    key = key.toLowerCase();
	    if (key.equals("input")) 
		input = true;
	    else if (key.equals("output")) 
		input = false;
	    else if (key.equals("loc")) {
		if (type == REAL) loc = toInt(value);
		if (loc+dim1 > cf.maxloc) cf.maxloc = loc+dim1;
	    } else if (key.equals("static")) 
		dynamic = STATIC;
	    else if (key.equals("dynamic")) 
		dynamic = DYNAMIC;
	    else if (key.equals("dim1")) {
		dim1 = toInt(value);
		if (loc+dim1 > cf.maxloc) cf.maxloc = loc+dim1;
		cf.dims.addUniq("" + dim1);
	    } else if (key.equals("init")) 
		init = value;
	    else if (key.equals("fixeval"))  
		fixeval = value;
	    else  if (key.equals("eval")) 
		eval = value;
	    else  if (key.equals("units")) 
		unit = value;
	    else if (key.equals("absmin")) 
		help.add(key + "=" + value);
	    else if (key.equals("min")) 
		help.add(key + "=" + value);
	    else if (key.equals("max")) 
		help.add(key + "=" + value);
	    else if (key.equals("absmax")) 
		help.add(key + "=" + value);
	}
	public void setList(String key, StringList value) {
	    key = key.toLowerCase();
	    if (key.equals("values")) 
		labels = value;
	}

	// post-process: consolidate parseable slaves
	protected void post() {
	    if (fixeval != null) init = null;
	    String ex = fixeval;
	    String n = headName(ex);
	    if (n == null) return;
	    CFPar m = cf.par(n);
	    if (m == null) return;
	    if (m.type == REAL) return;
	    if (m.type==CHOICE && ex.length() > n.length()+1) {
		try {
		    int l = n.length();
		    if (ex.charAt(l) == '+') l++;
		    String b = ex.substring(l);
		    m.labelBase = Util.toInt(b);
	    	} catch (Xcept e) {
		    return;
	    	}
	    } else {
		if (! ex.equals(n)) return;
	    }

	    // consolidate this variable with master m
	    m.loc = loc;
//	    if (name().length() < m.name().length()) {
//		m.rename(name());
//		rename(name() + "_JSIMSLAVE");
//	    }
	    skip = true;
	}
	    
	// get name at head
	private String headName(String s) {
	    if (s == null) return null;
	    int i=0;
	    while(i<s.length() && 
		(s.charAt(i) == '_' ||
		Character.isLetterOrDigit(s.charAt(i))))
		i++;
	    s = s.substring(0, i);
	    if (Util.isBlank(s)) return null;
	    return s;
	}

	// query
	public String name() {
	    if (jsname != null) return jsname;
	    return xsname;
	}
	public boolean dynamic() { 
	    switch (dynamic) {
	    case STATIC:	return false;
	    case DYNAMIC:	return true;
	    default:		return !input;
	    }		
	}
	public String eval() {
	    if (fixeval != null) return fixeval;
	    if (eval != null) return eval;
	    return null;
	}

	// write MML Var
	public void writeMMLVar() {
	    if (skip) return;

	    // variable type
	    String stype = null;
	    switch (type) {
	    case LOGICAL: 
	    case CHOICE: 
		stype = "choice";
		if (init != null) {
		    int inx = labels.indexOf(init);
		    init = (inx<0) ? null : ("" + (inx+1+labelBase));
		}
		break;
	    case INT: 
		stype = "int"; 
		break;
	    case REAL: 
		stype = "real"; 
		break;
	    case STRING: 
		warn("STRING datatype not yet supported for parameter " + xsname);
		return;
	    default: 
		error("Unknown datatype for parameter " + xsname);
		return;
	    }

	    // input/output + default value
	    String seqn = "";
	    if (input) {
		stype = stype + "Input";
		if (init != null) {
		    seqn = " = " + init;
		} else if (eval() == null) {
		    stype = "extern " + stype;
		    warn("Missing or invalid init or missing eval for parameter " + xsname);
		} 
	    } else {
		stype = stype + "Output";
	    }

	    // unit declaration or comment
	    String sunit = "";
	    String scomment = "";
	    if (! Util.isBlank(unit)) {
		try {
		    System.setErr(nullErr);
		    Unit.parse(cf.unitModel, "1 " + unit);
		    System.setErr(saveErr);
		    sunit = " " + unit;
		} catch (Xcept e) {
		    String unit1 = unit;
		    unit1 = unit1.replaceAll("1/\\(Molar s\\)", "1/Molar/s");
//		    unit1 = unit1.replaceAll("arbitrary", "dimensionless");
		    unit1 = unit1.replaceAll("none", "dimensionless");
		    unit1 = unit1.replaceAll("no.units", "dimensionless");
		    unit1 = unit1.replaceAll("seconds", "sec");
		    unit1 = unit1.replaceAll("microsec", "usec");
		    unit1 = unit1.replaceAll("amount/ml", "1/ml");
		    unit1 = unit1.replaceAll(" ", "*");
		    try {
		    	Unit.parse(cf.unitModel, "1 " + unit1);
		    	System.setErr(saveErr);
		    	sunit = " " + unit1;
		    } catch (Xcept e1) {   
		    	System.setErr(saveErr);
		    	warn("illegal unit <" + unit1 + 
		 	    "> for parameter " + xsname);
		    	scomment = " // XSim units=\"" + unit + "\"";
		    }
		}
	    }

	    // variable args
	    String sargs = "";
	    if (type == CHOICE) {
		sargs = "(";
		if (labelBase != 0) 
		    sargs = sargs + (labelBase+1) + ",";
		sargs = sargs + labels.toString(",", true) + ")";
	    } else {
		StringList largs = new StringList(2);
		if (dynamic()) largs.add(cf.ivar.name());
		if (dim1>1) largs.add("x" + dim1);
		if (largs.size() > 0) 
		    sargs = "(" + largs.toString(",", false) + ")";
	    }

	    // location / dim info
	    if (loc == 0) loc = cf.maxloc++;
	    String sdim = "";
	    if (dim1 > 1) 
		sdim = " " + name() + ".dim=" + dim1 + ";";

	    // help info
	    String shelp = "";
	    if (help.size() > 1) 
		shelp = " " + name() + ".help = \"" +
		    help.toString("  ", false) + "\";";

	    // print line
	    println("\t" + stype + " " + 
		name() + sargs + seqn + sunit + "; " +
		name() + ".loc=" + loc + ";" + 
		sdim + shelp + scomment );
	}

	// write MML Eqn for slaved inputs
	public void writeMMLEqn() {
	    if (skip || ! input) return;
	    String s = eval();
	    if (s == null) return;

	    // reprocess eval to MML syntax
	    s = s.replaceAll("\\*\\*", "^"); // power operator
	    s = s.replaceAll("==", "="); // equality test
	    while (s.indexOf("?") > 0) {
		int sx1 = s.indexOf('?');
		int sx2 = s.lastIndexOf(':');
		if (sx2 <= sx1) {
		    warn("Malformed eval for parameter " + xsname +
			": " + s);
		    break;
		}
		s = "if (" + s.substring(0, sx1) +
		    ") (" +  s.substring(sx1+1, sx2) + 
		    ") else (" + s.substring(sx2+1, s.length()) +
		    ")";
	    }

	    // replace renamed variables
	    for (int i=0; i<cf.renamedPars.size(); i++) {
		CFPar p = (CFPar) cf.renamedPars.item(i);
		String s0 = p.xsname;
	    	s0 = s0.replaceAll("\\(", "\\\\(");
	    	s0 = s0.replaceAll("\\)", "\\\\)");
		s = s.replaceAll("'" + s0 + "'", p.jsname);
		s = s.replaceAll(s0, p.jsname);
	    }

	    // write it all
	    println("\t" + name() + " = " + s + ";");
	}
		
}
