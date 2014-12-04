/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Native compiler

package JSim.lserver;

import java.io.*;
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class NativeCompiler {
	private LSServer server;	// for this server
	private String compile_C;	// C compiler, if any
	private String compile_CC;	// C++ compiler, if any
	private String compile_F;	// Fortran compiler, if any
	private int ct;			// # compiled created

	// constructor
	public NativeCompiler (LSServer s) {
	    server = s;
	    ct = 1001;
	}

	// compile
	public void compile(String modelName, SourceFunc func) throws Xcept {
	    // assign name - probably needs work
	    String libName = "JSfunc" + ct++;
	    String funcName = libName;
	    func.setLibrary(libName, funcName);

	    // write source file
	    File outfile = new File(server.buildDir, 
		libName + "." + suffix(func));
	    try {
	    	PrintWriter wrt = new PrintWriter(new FileWriter(outfile));
	    	switch(func.lang()) {
	    	case XFunc.C: 
	    	case XFunc.CC: 
		    writeC(wrt, func); 
		    break;
	    	case XFunc.FORTRAN:
		    throw new Xcept(func, "native compiler not yet supported");
	    	}
	        wrt.close();
	    } catch (IOException e) {
		throw new Xcept(func, 
		    "error writing native code:" + e.getMessage());
	    }

	    // find compiler, create command line
	    String cmd = "jscompile_" + suffix(func);
	    if (Util.isWin32()) cmd = cmd + ".bat";
	    try {
		cmd = UtilIO.pathFind(cmd, server.jsimPath());
	    } catch (Xcept e) {
		String lname = langName(func.lang());
		throw new Xcept(
		    "Model requires " + lname + 
		    " compiler, but none is available. " +
		    cmd + " not found in JSIMPATH=" + 
		    server.jsimPath());
	    }
	    cmd = (new File(cmd).getAbsolutePath());
	    String jhome = Util.jsimHome();
	    String bdir = server.buildDir.getAbsolutePath();
	    String q = "\"";
	    if (Util.isWin32()) {
	        jhome = q + jhome + q;
		cmd = q + cmd + q;
		bdir = q + bdir + q;
	    }
	    String[] cmdarr = new String[] {
		cmd, jhome, bdir, libName,
		(func.flags() == null) ? "" : func.flags()
	    };

	    // remove old library
	    File libFile = new File(server.buildDir, 
		System.mapLibraryName(libName));
	    if (libFile.exists()) 
		if (! libFile.delete()) throw new Xcept(func,
		    "Cannot delete old library " + libFile);
	    
	    // execute compiler command
	    Util.verbose("Executing native compiler: " + cmd);
	    String msg = null;
	    Process proc = null;
	    try {
	    	proc = Runtime.getRuntime().exec(cmdarr);
		int stat = proc.waitFor();
		if (stat != 0) 
		    msg = "Native compile terminated abnormally (status=" + 
			stat + ")";
	    } catch (Exception e) {
		msg = "Error during native compile: " + e.getMessage();
	    }
	    if (msg == null && !libFile.exists()) 
		msg = "Native compile failed to create " + libFile;
	    if (msg != null) {
		if (proc != null) 
		     msg = msg + "\n" + UtilIO.readText(proc.getInputStream()) +
			"\n" + UtilIO.readText(proc.getErrorStream());
		throw new Xcept(func, msg);
	    }
	}	

	// file suffix
	public String suffix(SourceFunc func) throws Xcept {
	    switch(func.lang()) {
	    case XFunc.C: return "c";
	    case XFunc.CC: return "C";
	    case XFunc.FORTRAN: return "F";
	    default: throw new Xcept(func, "invalid native compiler code");
	    }
	}

	// language name
	public String langName(int lang) throws Xcept {
	    switch(lang) {
	    case XFunc.C: return "C";
	    case XFunc.CC: return "C++";
	    case XFunc.FORTRAN: return "Fortran";
	    default: throw new Xcept("invalid native compiler");
	    }
	}

	// write C code
	public void writeC(PrintWriter wrt, SourceFunc func) throws Xcept {
	    wrt.println("/* JSim generated native library */");
	    wrt.println("");
	    wrt.println("#include \"jsimapi.h\"");
	    if (func.topCode() != null) {
	        wrt.println("");
	    	wrt.println(func.topCode());
	    	wrt.println("");
	    }
	    wrt.println("");
	    String jsfunc = (func.dataType() == Expr.VOID) ? 
		"JSIM_PROCEDURE" : "JSIM_REAL_FUNCTION";
	    wrt.println(jsfunc + "(" + func.libName() + "," +
		func.funcName() + ") {");
	    wrt.println("\tJSIM_INIT();");
	    for (int i=0; i<func.nargs(); i++) 
		wrt.println("\tJSimArg *" + func.parName(i) + 
		    " = JSIM_ARG(" + i + ");"); 
	    wrt.println(func.mainCode());
	    wrt.println("");
	    wrt.println("}");
	    if (func.bottomCode() != null) {
	    	wrt.println("");
	    	wrt.println(func.bottomCode());
	    	wrt.println("");
	    }
		
	}
}

