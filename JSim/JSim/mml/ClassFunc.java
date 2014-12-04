/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// external function from pre-compiled java class

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.PrintStream;
import java.lang.reflect.*;
import JSim.jruntime.*; 


public class ClassFunc extends XFunc {
	private int dataType; // must match internal
	protected Class<?> clss;	// java class
	private ClassLoader classLoader;
	private Method m_getInfo;

	// constructor
	public ClassFunc(String n, int dtype, 
	String clssName, ClassLoader cloader) throws Xcept {
	    super(n, XFunc.CLASS);
	    dataType = dtype;
	    classLoader = cloader;
	    if (clssName != null)
	    	loadClass(clssName);
	}
	
	// load class
	public void loadClass(String clssName) throws Xcept {
	    if (m_getInfo != null) throw new Xcept(this, 
	    	"Duplicate class=name specification");
		
	    // verify class
	    try {
	    	if (classLoader == null) {
		    Util.verbose(
			"No custom class loader is available for ClassFuncs");
	    	    clss = Class.forName(clssName);
		} else 
		    clss = classLoader.loadClass(clssName);
	    	if (! RTXFunc.class.isAssignableFrom(clss)) throw new Xcept(this,
		   "Class " + clssName + " must extend JSim.jruntime.RTXFunc");
	    	Class[]  carr = new Class[] { Expr.List.class };
	    	m_getInfo = clss.getMethod("getInfo", carr);
	    } catch (Exception e) {
		throw new Xcept(this, e.getMessage());
	    }	
	}

	// add codes
	public void addCode(String n, String code) throws Xcept {
	    if (n.equals("class")) 
	    	loadClass(code);
	    else throw new Xcept(this,
	    	"Unrecognized code " + n);
	}    	
			
	// create function call
	public XFuncCall createCall(Expr.List args) throws Xcept {
	    Object[] oarr = new Object[] { args };
	    try {
	    	RTXFunc.Info info = (RTXFunc.Info) m_getInfo.invoke(null, oarr);
		if (info.dataType != dataType) throw new Xcept(
		    "declared dataType does not match internal dataType");
	    	return new XFuncCall(this, args, info.dataType, info.ninputs);

	    } catch (Exception e) {
		String msg = e.getMessage();
		if (e instanceof InvocationTargetException) 
		    msg = ((InvocationTargetException) e).getTargetException().getMessage();
		throw new Xcept(this, msg);
	    }
	}

	// validate
	public void validate() throws Xcept {
	    if (m_getInfo == null) throw new Xcept(this,
	    	"No class=name specified");
	}

	// query
	public Class clss() { return clss; }
	public String className() { return clss.getName(); }
	public int dataType() { return dataType; }

	// same function definition?
	public boolean sameAs(XFunc f) {
	    if (! (f instanceof ClassFunc)) return false;
	    ClassFunc f1 = (ClassFunc) f;
	    if (clss != f1.clss) return false;
	    return super.sameAs(f1);
	}

	// write flat 
	public void writeFlat(PrintStream out) {
	    String fp = (dataType == Expr.VOID) ? 
	    	"procedure" : "real function";
	    out.println("class " + fp + " " + name + "{");
	    out.println("\tclass=\"" + clss.getName() + "\";");
	    out.println("\treentrant=\"" + isReentrant() + "\";");
	    out.println("}");
	}


}
