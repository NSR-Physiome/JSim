/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// model read from file

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.*;
import java.util.Hashtable;
import java.util.Enumeration;

public class ModelReader extends Model  {
	private String filename;// file name being read
	protected Comp comp;	// working component, parent of children
	protected Expr sd;	// Sub-domain expression (when clause)
	protected Event wevent;	// working event 
	protected XFunc wfunc; // working extern func
	private Templ wtempl;	// template for next component to create
	private int waccess;	// access for next comp ...
	public ModelScanner scan;// lexical scanner

	private Hashtable<Integer,String> comments_HT; // mml text line numbers and associated comments. 
	protected Hashtable<String,Integer> identifiers_HT; // mml identifiers and text line numbers where found.

	// testing constructor
	public ModelReader(File file) throws Xcept {
	    this (file, null, defaultImportPath());
	}

	// model text constructor
	public ModelReader(String modelText) throws Xcept {
	    this(new StringReader(modelText), null, defaultImportPath());
	}
	
	// File reader constructor
	public ModelReader(File file, ClassLoader cload, String ipath)
	throws Xcept {
	    super(cload, ipath);
	    filename = file.toString();
	    try {
	        FileReader rdr = new FileReader(file);
		common(rdr);
		rdr.close();
	    } catch (Exception e) {
		throw new Xcept(e.toString() + 
		   "file name <" + file + ">");
	    }
	}

	// reader constructor
	public ModelReader(Reader rdr, ClassLoader cload, String ipath)
	throws Xcept {
	    super(cload, ipath);
	    filename = null;
	    common(rdr);
	}

	// common contstructor code
	private void common(Reader rdr) throws Xcept {
	    comp = this;
        comments_HT = new Hashtable<Integer,String>();
        identifiers_HT = new Hashtable<String,Integer>();
	    // setup scanner & parser
	    scan = new ModelScanner(rdr);
	    ModelParser pars = new ModelParser(scan);
	    pars.mr = this;

	    // try to parse
	    try {
	    	pars.parse();
			if( scan.getComments().size()>0)
			{
				comments_HT = scan.getComments();
				/*	 Enumeration<Integer> keys = comments_HT.keys();
				while(keys.hasMoreElements()) {
					Integer key = keys.nextElement();
						System.out.println("Value of "+key+" is: "+comments_HT.get(key));
				} */
			}
			/*		  Enumeration<String> ident_keys = identifiers_HT.keys();
			while(ident_keys.hasMoreElements()) {
				String key = ident_keys.nextElement();
				System.out.println("Value of "+key+" is: "+identifiers_HT.get(key));
				} */

	    } catch (Exception e) {
		XceptPos pos = currPos();
		if (! (e instanceof Xcept)) 
		    throw new Xcept(e.toString(), pos);
		Xcept xe = (Xcept) e;
		pos.child = xe.pos;
		xe.pos = pos;
		throw xe;
	    }
	}
	
	// default import path
	public static String defaultImportPath() throws Xcept {
	    String path = System.getProperty("jsim.path");
	    if (! Util.isBlank(path)) return path;
	    return 
		System.getProperty("user.home") + 
		    File.separator + ".jsim" + 
		    File.separator + "local" + 
		File.pathSeparator + 
		Util.jsimHome() + File.separator + "local" + 
		File.pathSeparator + 
		Util.jsimHome() + File.separator + "common" + 
		File.pathSeparator + 
		".";
	}

	// version #
	protected void version(String s) throws Xcept {
	    if (s == null) return; // no version check
	    if (s.equals("v1.0") || ! s.startsWith("v1.")) throw new Xcept(
		"JSim " + s + " is not an MML version supported by this program");
	}

	// import library
	protected void importLib(String s) throws Xcept {
	    String fname = UtilIO.pathFind(s+".mod", importPath);
	    Model m = new ModelReader(new File(fname), classLoader, importPath);
            units.merge(m.units);

	    // import java templates (comp templates more complicated)
	    for (int i=0; i<m.templ.size(); i++) {
		Templ t1 = m.templ.templ(i);
		Templ t0 = templ.templ(t1.name());
		if (t0 == null) 
		    t1.clone(this);
		else if (! t0.isCompatible(t1)) throw new Xcept(this,
		    "Incompatible definitions for template " + t1.name());
	    }

	    // import xfuncs 
	    for (int i=0; i<m.xfuncs.size(); i++) {
		XFunc f = m.xfuncs.xfunc(i);
		registerXFunc(f);
	    }
	}

	// parse Unit Conversion name
	protected void setUnitControl(String s) throws Xcept {
	    if (unitControl != NONE) throw new Xcept(
		"Multiple unit conversion statements unsupported");
	    if (s.equals("off")) unitControl = OFF; else 
	    if (s.equals("on")) unitControl = ON; else 
		throw new Xcept(
		"Unsupport unit conversion keyword " + s);
	}

	// add extern code
	protected void addExternCode(String j, String txt)
	throws Xcept {
	    if (! j.equals("java")) throw new Xcept(this,
		"extern code must be java");
	    externJava.add(txt);
	}

	// create new source or native function
	protected void newXFunc(String ftype, String dtype,
	String name, StringList pstr) throws Xcept {
	    int dataType;
	    if (dtype.equals("void"))
		dataType = Expr.VOID;
	    else if (dtype.equals("real"))
		dataType = Expr.REAL;
	    else throw new Xcept(this,
		"Illegal data type \"" + dtype + 
		"\" for function " + name);
	    
	    // ClassFunc needs no arg list
	    if (ftype.equals("class")) {
	    	if (pstr != null) throw new Xcept(
		    "class function/procedure declaration for " +
		    name + " should have no argument list");
	    	wfunc = new ClassFunc(name, dataType, null, classLoader);
		return;
	    }

	    // NativeFunc and SourceFunc need arg list
	    if (pstr == null) throw new Xcept(
	    	"function/procedure declaration for " +
		name + " missing required argument list");
	    if (ftype.equals("native")) 
		wfunc = new NativeFunc(name, dataType, pstr);
	    else if (ftype.equals("source"))
		wfunc = new SourceFunc(name, dataType, pstr);
	    else throw new Xcept(this,
		"Unsupported function type \"" + ftype + 
		"\" for function " + name);
	}

	// done with current function
	protected void doneXFunc() throws Xcept {
	    wfunc.validate();
	    registerXFunc(wfunc);
	    wfunc = null;
	}

	// add code to current function
	protected void addXFuncCode(String n, String code) throws Xcept {
	    if (n.equals("reentrant"))
	    	wfunc.setReentrant(Util.toBoolean(code));
	    else if (wfunc instanceof CommonFunc)
	    	((CommonFunc) wfunc).addCode(n, code);
	    else if (wfunc instanceof ClassFunc)
	    	((ClassFunc) wfunc).addCode(n, code);
	    else throw new Xcept(wfunc,
	    	"Unsupported XFunc class");
	}

	// set current comp unit, error if already set differently
	protected void setCompUnit(Unit unit) throws Xcept {
	    Unit u = comp.unit();
	    if (u == null) 
	    	comp.setUnit(unit);
	    else if (! Unit.same(u, unit)) throw new Xcept(
	    	"Units " + unit.pubName() + " for " + comp +
		" conflict with previously declared units " + 
		u.pubName());
	}

	// create new component template
	protected void newCompTempl(String n, String tname, 
	StringList parms) throws Xcept {
	    CompTempl ct = new CompTempl(this, n, tname, parms);
	    comp = ct.comp;
	}

	// done with component template
	protected void doneCompTempl() {
	    comp = this;
	}

	// set templ, access and isTempl
	protected void setTempl(String t, int acc)
	throws Xcept {
	    wtempl = getTempl(t);
	    waccess = acc;
	}

	// create list for setAccess
	protected Comp.List listAccess(Comp.List l, String n) throws Xcept {
	    if (l == null) l = new Comp.List(1);
	    NamedExpr e = comp.getNamed(n);
	    if (e == null) throw new Xcept(comp,
		n + " not found");
	    if (! (e instanceof Comp)) throw new Xcept(e,
		"property not valid here"); 
	    l.add(e);
	    return l;
	}

	// set Access for list of components
	protected void setAccess(Comp.List list, int acc) throws Xcept {
	    for (int i=0; i<list.size(); i++) 
		list.comp(i).setAccess(acc);
	}

	// create component from parser
	protected void parseComp(String n, Expr.List e)
	throws Xcept {
	    // already defined ??
	    NamedExpr ne = comp.getNamed(n);
	    if (ne != null && ! (ne instanceof Comp)) 
		throw new Xcept(ne,
		    "incompatible property definition"); 
	    Comp c = (Comp) ne;

	    // nope, create new
	    if (c == null) {
 	    	c = wtempl.createComp(comp, n, e);
	        c.templ = wtempl;
			identifiers_HT.put(c.name,scan.getLineNo());
			c.builtin = false;
	    }

	    // check agreement,  set access
	    if (c.templ != wtempl || c.args != e) throw new Xcept(c,
		"Incompatible definitions for " + c.fullName() + 
		" c.templ=" + c.templ + " wtempl=" + wtempl +
		" cargs=" + c.args + " e=" + e);	
	    c.setAccess(waccess);   
	    comp = c;

	}   

	// done with 1 component within statement
   	protected void doneComp() throws Xcept {
		String childName = new String(comp.name);
		//		System.out.println("ModelReader:doneComp: name: "+comp.name+"," +scan.getLineNo());
		if(!identifiers_HT.containsKey(comp.name)) {
			identifiers_HT.put(comp.name,scan.getLineNo());
		}
		//		comp.dump(System.out,"dump: ");
	    comp = comp.parent;

	} 

	// new integral
	protected Expr makeIntegral(Range r, Expr u)
	throws Xcept {
 //	System.out.println("*******ModelReader:makeIntegral: "+u.toString()+", "+scan.getLineNo() );
		identifiers_HT.put(u.toString().trim(),scan.getLineNo());
	    return new Integral(comp, r, u);
	}
	protected Expr makeIntegral(Expr u)
	throws Xcept {
		identifiers_HT.put(u.toString().trim(),scan.getLineNo());
	    return new Integral(comp, u);
	}

	// new summation
	protected Summation makeSum(Range r, Expr u)
	throws Xcept {
		identifiers_HT.put(u.toString().trim(),scan.getLineNo());
	    return new Summation(comp, r, u);
	}
	protected Summation makeSum(Expr u)
	throws Xcept {
		identifiers_HT.put(u.toString().trim(),scan.getLineNo());
	    return new Summation(comp, u);
	}

	// start event for current component
	protected void beginEvent(Expr trig) throws Xcept {
	    wevent = new Event(comp, trig);
	}

	// done with event
	protected void endEvent() {
	    wevent = null;
	}

	// add antimony block
	protected void addAntimony(String text) throws Xcept {
	    Expr.List args = new Expr.List(1);
	    args.add( new StringConst(text));
	    new AntimonySys(comp, "antimony", args);
	}

	// add default property
	protected void addDefProp(String name, String stype)
	throws Xcept {
	    int type = -1;
	    if (stype.equals("string")) 
	    	type = Expr.STRING;
	    else throw new Xcept(this,
	    	"Property " + name + 
		": only string type is currently supported.");
		identifiers_HT.put(name,scan.getLineNo());
	    defProps.add(new CompProp(this, name, type));
	}

	// add eqn for current component
	protected void addEqn(Expr rhx, Unit u) throws Xcept {
	    if (u != null && !rhx.isConst()) 
	    	throw new Xcept(comp,
		"Constant expression required with unit");
	    Expr e = comp;
	    if (e.dataType() != rhx.dataType()) throw new Xcept(e, rhx,
		"mismatched dataTypes");
	    Eqn eqn = new Eqn(comp.parent, null, e, IExpr.EQ, rhx);
	    eqn.pos = currPos();
		String[] mod_eqn = eqn.toString().split("builtin",1); // some eqns are considered 'builtin'
		String identifier = "";
		if(mod_eqn.length>1) { 
			identifier = mod_eqn[1];
			identifier = identifier.replace(";","");
			identifier = identifier.trim();
		}
		else identifier = eqn.toString();
 //	System.out.println("*******ModelReader:addEqn: "+identifier+", "+scan.getLineNo() );
        identifiers_HT.put(identifier,scan.getLineNo());
	    eqn.builtin = false;
	    if (u != null)    
	    	setCompUnit(u); 
	}

	// add statement into current component
	protected void addStmt(Expr e) throws Xcept {
		// Find last semicolon line number and use as statment line number.
		// Line number should be when statement starts... if blank lines between next statement 
		// then line number reported can be 2, 3 or more lines off.
		identifiers_HT.put(e.toString(),(scan.getSemiColLineNumb()));
	    // inside event?
	    if (wevent != null) {
		wevent.addEqn(e);
		return;
	    }

	    // see if any CompProps 
 	    Expr.List exprs = new Expr.List(4);
	    e.addNamedExpr(exprs);
	    CompProp p = null;
	    for (int i=0; i<exprs.size(); i++) 
		if (exprs.expr(i) instanceof CompProp)
		    p = (CompProp) exprs.expr(i);

	    // CompProp setting
	    if (p != null) {
		p.setStmt(e);

	    // equation?
	    } else if (e instanceof CompareExpr) {
	    	Eqn eqn = Eqn.create(comp, sd, e, currPos()); 
	    	eqn.builtin = false;

	    // function call?
	    } else if (e instanceof XFuncCall) {
		((XFuncCall) e).setSD(sd);
		comp.voidFuncCalls.add(e);

	    // error!
	    } else throw new Xcept(e,
		"Equation, relation or void function call expected");
	}

	// done with component statement (pop)
	protected void doneTempl() {
	    wtempl = null;
	    waccess = Comp.PUBLIC;
	} 

	// return current MML position
	public XceptPos currPos() {
	    return new XceptPos(filename, scan.getLineNo(),
		scan.getCharNo(), scan.tokText());
	}

	// get relative Comp from name
	public Expr compByName(String n) throws Xcept {
	    return compByName(comp, n);
	}

	// relative function Call from name & args
	public Expr funcCall(String n, Expr.List elist) throws Xcept {
	    return funcCall(comp, n, elist);
	}

	public Hashtable getCommentsHT() throws Xcept { return comments_HT; }
	
	public Hashtable getIdentifiersHT() throws Xcept { return identifiers_HT; }

	// class override
	public void classOverride(String cname, String ocname) throws Xcept {
	    throw new Xcept("MML override is not currently supported");
	}
}
