/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Java code for F&P

package JSim.jcode;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import JSim.plan.*;
import java.io.*;
import java.lang.reflect.*;

public class JFPWriter extends JExprWriter {
	private JPlanWriter planWriter;
	private XFunc.NList funcs;

	// constructor
	public JFPWriter(JPlanWriter planWriter) throws Xcept { 
	    super(null); 
	    this.planWriter = planWriter;
	    funcs = flatModel().neededFuncs();
	}

	// set C SourceFunc libraries to match jsbatch output
	//    do this only in test harness !!!
	public void setSourceLibs() throws Xcept {
	    String libName = "JSfunc1001";
	    String funcName = libName;
	    for (int i=0; i<funcs().size(); i++) {
		if (! (funcs().xfunc(i) instanceof SourceFunc)) continue;
		SourceFunc f = (SourceFunc) funcs().xfunc(i);
		if (f.libName() != null) continue;
		f.setLibrary(libName, funcName);
	    }
	}

	// function declarations
	public void writeDecl() throws Xcept {
	    println("// F&P declarations");
	    for (int i=0; i<funcs().size(); i++) {
		XFunc f = funcs().xfunc(i);
		println("public RTXFunc " + 
		    funcName(f) + ";");
	    }
	    for (int i=0; i<funcCalls().size(); i++) {
		XFuncCall fc = (XFuncCall) funcCalls().expr(i);
		println("public RTXFuncCall " + 
		    funcCallName(fc) + ";");
	    }
	    println("");
	}

	// function initialization
	public void writeInit() throws Xcept {
	    println("// F&P initialization");
	    for (int i=0; i<funcs().size(); i++) {
		XFunc f = funcs().xfunc(i);
		if (f instanceof ClassFunc) 
		    writeFunc((ClassFunc) f);
		else if (f instanceof CommonFunc)
		    writeFunc((CommonFunc) f);
		else throw new Xcept(f,
		    "JPlanWriter does not support this XFunc type");
	    }
	    for (int i=0; i<funcCalls().size(); i++) {
		XFuncCall fc = (XFuncCall) funcCalls().expr(i);
		println(funcCallName(fc) + 
		    " = new RTXFuncCall(" + 
		    funcName(fc.func()) + ");");
		for (int j=0; j<fc.args().size(); j++) 
		    writeFuncArg(fc, fc.args().arg(j), j);
	    }
	    println("");
	}
	    
	// XContext methods for F&P
	public void writeMethods() throws Xcept {
	    println("// ctxt methods for F&P");
	    for (int i=0; i<funcCalls().size(); i++) {
		XFuncCall fc = (XFuncCall) funcCalls().expr(i);
		for (int j=0; j<fc.args().size(); j++) { 
		    XFuncArg arg = fc.args().arg(j);
		    println("protected double " + funcCallName(fc)
		    	+ "_" + j + "_getRealVal() throws Xcept {");
		    indentIn();
		    println("return " + fmt(arg.base()) + ";");
		    indentOut();
		    println("}");
		}
	    }
	}
		    
	// write ClassFunc instantiation
	public void writeFunc(ClassFunc f) throws Xcept {
	    println(funcName(f) + " = new " +
		f.className() + "(this, \"" +
		f.name() + "\");");
	}

	// write CommonFunc instantiation
	public void writeFunc(CommonFunc f) throws Xcept {
	    if (f.libName() != null) {
		String className = nativeClassName(f.libName());
	    	println(funcName(f) + " = new " +
		    className + "(this, \"" +
		    f.name() + "\", " + className + "." + 
		    nativeFuncInx(f) + 
		    ");");
	    } else 
	    	println(funcName(f) + " = new " +
		    funcClassName(f) + "(this, \"" +
		    f.name() + "\");");
	}

	// RTRealDomain[] String
	public String domStr(Expr.List doms) {
	    String s = "new RTRealDomain[] {";
	    for (int i=0; i<doms.size(); i++) {
		if (i!=0) s = s + ",";
		Var v = (Var) doms.get(i);
		s = s + vstruct(v);
	    }
	    return s + "}";
	}

	// write func argument addition
	public void writeFuncArg(XFuncCall fc, XFuncArg arg, int argInx) 
	throws Xcept {

	    // arg.base is Var
	    Expr base = arg.base();
	    if (base instanceof Var) {
	    	Var v = (Var) base;
	    	println("new RTXFuncArg(" + funcCallName(fc) +
		    ", " + domStr(arg.argDoms()) + ", " + 
		    vstruct(v) + ", " + arg.isInput() + ");");
	        return;
	    }

	    // Expr arg 
	    String n = funcCallName(fc);
	    println("new RTXFuncArg(" + n +
		", " + domStr(arg.argDoms()) + ") {");
	    indentIn();
	    println("protected double getRealVal(RTContext ctxt) throws Xcept {");
	    indentIn();
	    println("return ((XContext) ctxt)." + n + "_" + 
	    	argInx + "_getRealVal();");
	    indentOut();
	    println("}");
	    indentOut();
	    println("};"); 
	}

	// write function class definitions (java source functions)
	public void writeJavaClasses() throws Xcept {
	    for (int i=0; i<funcs().size(); i++) {
		XFunc f = funcs().xfunc(i);
		if (f instanceof SourceFunc && f.lang() == XFunc.JAVA)  
		    writeJavaClass((SourceFunc) f);
	    }
	}

	// write Java class definition
	public void writeJavaClass(SourceFunc f) throws Xcept {
	    println("// " + funcClassName(f) + " definition");
	    println("public static class " + 
		funcClassName(f) + " extends RTXFunc {");
	    indentIn();

	    // top code
	    if (f.topCode() != null) {
	    	println("// copy topcode here");
	    	println(f.topCode());
	    	println("");
	    }

	    // constructor
	    println("public " + funcClassName(f) + 
		"(RTModel m, String n) throws Xcept " + 
		"{ super(m, n); }");

	    // [real][void]Calculate method
	    String rtype = (f.dataType() == Expr.REAL) ? 
	    	"double" : "void";
	    String fname = (f.dataType() == Expr.REAL) ? 
	    	"real" : "void";
	    println("public " + rtype + " " + 
	        fname + "Calculate(RealNData[] jsargs) throws Xcept {");
	    indentIn();
	    for (int i=0; i<f.nargs(); i++) 
		println("RealNData " + f.parName(i) + 
		    " = jsargs[" + i + "];");
	    if (f.mainCode() != null) {
	    	println("// copy maincode here");
	    	println(f.mainCode());
		println("");
	    }
	    indentOut();
	    println("}");

	    // bottom and out
	    if (f.bottomCode() != null) {
	    	println("// copy bottomcode here");
		println(f.bottomCode());
		println("");
	    }
	    indentOut();
	    println("}");
	    println("");
	}

	// write native function class definitions
	public void writeNativeFuncClasses() throws Xcept {
	    StringList libs;
	    libs = new StringList(funcs().size()); 
	    for (int i=0; i<funcs().size(); i++) {
		XFunc f1 = funcs().xfunc(i);
		if (f1.libName() == null) continue;
		String lib = f1.libName();
		if (libs.containSame(lib)) continue;
		libs.add(lib);
		XFunc.NList funcs = new XFunc.NList(funcs().size());
		funcs.add(f1);
		for (int j=i+1; j<funcs().size(); j++) {
		    f1 = funcs().xfunc(j);
		    if (f1.libName() == null) continue;
		    if (f1.libName().equals(lib)) funcs.add(f1);
		}
	    writeNativeClass(lib, funcs);
	    }
	}

	// write Native class definition
	public void writeNativeClass(String lib, XFunc.NList funcs) 
	throws Xcept {
	    println("// " + nativeClassName(lib) + " definition");
	    println("class " + 	nativeClassName(lib) + " extends RTXFunc {");
	    indentIn();
	    println("int which;");

	    // constructor
	    println("public " + nativeClassName(lib) + 
		"(RTModel m, String n, int w) throws Xcept " + 
		"{ super(m, n); which = w; }");

	    // voidCalculate method    
	    println("public void voidCalculate(RealNData[] args)" +
		" throws Xcept {");
	    indentIn();
	    println("switch (which) {");
	    for (int i=0; i<funcs.size(); i++) {
		CommonFunc f = (CommonFunc) funcs.xfunc(i);
		if (f.dataType() == Expr.REAL) continue;
	    	println("case " + nativeFuncInx(f) + ": " + 
		    "JS" + f.funcName() + "(args); break;");
	    }
	    println("default: throw new Xcept(this, " + 
		"\"Unknown native switch value\");");
	    println("}");
	    indentOut();
	    println("}");

	    // realCalculate method    
	    println("public double realCalculate(RealNData[] args)" +
		" throws Xcept {");
	    indentIn();
	    println("switch (which) {");
	    for (int i=0; i<funcs.size(); i++) {
		CommonFunc f = (CommonFunc) funcs.xfunc(i);
		if (f.dataType() != Expr.REAL) continue;
	    	println("case " + nativeFuncInx(f) + ": " + 
		    "return JS" + f.funcName() + "(args);");
	    }
	    println("default: throw new Xcept(this, " + 
		"\"Unknown native switch value\");");
	    println("}");
	    indentOut();
	    println("}");

	    // native method and inx definition
	    for (int i=0; i<funcs.size(); i++) {
		XFunc f = funcs.xfunc(i);
	        println("private static native double JS" +
		    f.funcName() + 
		    "(RealNData[] args) throws Xcept;");
	    	println("public static final int " + 
		    nativeFuncInx(f) +
		    " = " + (i+1) + ";");
	    }

	    // load library
	    println("static { System.loadLibrary(\"" + lib + "\"); }");

	    // finish class definition
	    indentOut();
	    println("}");
	    println("");
	}

	// write F&P call
	protected void writeCall(XFuncCall fc) throws Xcept {
	    println(funcCallName(fc) + ".voidVal(this);");
	}

	// write extern Java
	protected void writeExternJava() throws Xcept {
	    for (int i=0; i<flatModel().externJava.size(); i++) {
		println("// extern java\n");	
		println(flatModel().externJava.str(i));
	    } 
	}		

	// function name sub-routines
	private String funcName(XFunc func) {
	    return "JS" + func.name();
	}
	private String funcClassName(XFunc func) {
	    return "JS" + func.name() + "__class";
	}
	private String nativeClassName(String lib) { 
	    return "JSJS" + lib; 
	}
	private String nativeFuncInx(XFunc f) {
 	    return "JSX__" + f.funcName();
	}
	protected String funcCallName(XFuncCall fc) {
	    return "JS" + fc.func().name() + "__" + fc.seq();
	}

	// query
	public JPlanWriter planWriter() { return planWriter; }
	private Model flatModel() throws Xcept { 
	    return planWriter.mmlModel().getFlatModel(); 
	}
	private XFunc.NList funcs() { return funcs; }
	private Expr.List funcCalls() throws Xcept { 
	    return flatModel().funcCalls(); 
	}
	
}
