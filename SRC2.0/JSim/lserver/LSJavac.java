/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Java compiler for JSim models

package JSim.lserver;

import java.lang.reflect.*;
import java.io.*;
import JSim.util.*;

public class LSJavac {
	private LSServer server;
	private String classpath;
	private String jsimJavac; // jsim.javac property value

	// Sun-specific init
	private static final String SUN_MAIN = "sun.tools.javac.Main";
	private Constructor cons; // smain constructor
	private Method m_compile; // compile method in above

       // constructor
	public LSJavac (LSServer s) throws Xcept {
	    server = s;

	    // construct compile time classpath
	    classpath =
		System.getProperty("java.class.path") +
		File.pathSeparator + server.buildDir +
		File.pathSeparator + server.jsimPath();

	    // load jsimJavac property
	    jsimJavac = System.getProperty("jsim.javac");

	    // initialize
	    if (useSun())
	    	initSun();
	    else
	        initOther();
	}

	// init Sun compiler
	private void initSun() throws Xcept {

	    // find sun distrib compiler class
	    try {
	    	Class<?> clss = server.classLoader.loadClass(SUN_MAIN);
		Class[] cargs = new Class[] { 
		    OutputStream.class, String.class };
		cons = clss.getConstructor(cargs);
		String[] st = new String[0]; // kludge
		Class[] margs = new Class[] { st.getClass() };
		m_compile = clss.getMethod("compile", margs);
	    } catch (ClassNotFoundException e) {
	    	throw new Xcept("Java compiler missing: " + 
		    " no " + SUN_MAIN + " in CLASSPATH=" + 
		    server.jsimPath());
	    } catch (NoSuchMethodException e) {
	    	throw new Xcept(SUN_MAIN + 
		    " missing required compile method and/or constructor");
	    }
	}

	// init other compiler
	private void initOther() throws Xcept {
	    File fjavac = new File(jsimJavac);
	    if (! fjavac.exists()) throw new Xcept(
	    	"Java compiler JSIMJAVAC=" + jsimJavac + " not found");
	}

	// compile
	public void compile(File javaFile, PrintStream log) 
	throws Xcept {
	    try {
	    	if (useSun())
		    compileSun(javaFile, log);
		else
		    compileOther(javaFile, log);
	    } catch (InvocationTargetException e) {
	    	throw new Xcept("" + e.getTargetException());
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}
	
	// compile using Sun javac.Main
	private void compileSun(File javaFile, PrintStream log) 
	throws Exception {
	    Util.verbose("==== Compiling model " + javaFile + 
	    	" via " + SUN_MAIN);
	    Object[] cargs = new Object[] { log, "javac" };
	    Object main = cons.newInstance(cargs);
	    String[] jcargs = {
		"-nowarn", // repress class deprecated msg 
		"-classpath",
		classpath,
		javaFile.getPath() 
	    };
	    Object[] margs = new Object[] { jcargs };
	    Boolean ret = (Boolean) m_compile.invoke(main, margs);
	    if (! ret.booleanValue()) throw new Xcept(
	    	"Java compilation failed for " + javaFile);
	}

	// compile using 
	private void compileOther(File javaFile, PrintStream log) 
	throws Exception {
	    Util.verbose("==== Compiling model " + javaFile + 
	    	" via " + jsimJavac);
	    String[] cmdarr = {
	    	jsimJavac,
		"-nowarn", // repress class deprecated msg 
		"-classpath",
		classpath,
		javaFile.getPath() 
	    };
	    Process proc = null;
	    proc = Runtime.getRuntime().exec(cmdarr);
	    String errText = UtilIO.readText(proc.getErrorStream());
	    int stat = proc.waitFor();
	    if (stat != 0) throw new Xcept(
		"Java compile terminated abnormally (status=" + 
		stat + ") " + errText);
	}

	// use Sun compiler?
	private boolean useSun() { return Util.isBlank(jsimJavac); }
	
}


