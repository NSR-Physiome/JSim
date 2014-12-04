/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// top level control of batch application

package JSim.text;

import java.io.*;	// File and PrintStream
import java.net.URL;

import JSim.util.*;
import JSim.jruntime.*;

public class JSXWrap {
	public final static int FORTRAN = 0;
	public final static int C = 1;

	// instance vars
	public int lang;
	public String model;

	// constructor
	public JSXWrap(int l, String m) throws Exception {
	    lang = l;
	    model = m;

	    if (! Util.onlyLettersAndDigits(model)) 
		throw new Xcept("model name may contain " +
		    "only letters and digits");
	    String res = "jsxwrapc.txt";
	    URL url = getClass().getResource(res);
	    if (url == null) throw new Xcept(
		"XSim wrapper resource \"" + res + "\" not found");
	    String txt = UtilIO.readText(url);
	    txt = txt.replaceAll("proto", model);

	    String fname = "jsx" + model + ".c";
	    File f = new File(fname);
	    UtilIO.writeText(f, txt);
	}

	// main-line
	public static void main(String[] args)
	throws Exception {
	    if (args.length < 3) usage();
	    if (args[0].equals("-c")) throw new Xcept(
		"C language wrappers not yet supported");
	    if (! args[0].equals("-f")) usage();
	    new JSXWrap(FORTRAN, args[1]);
	    System.exit(0);
	}

	// usage message
	public static void usage() {
	    System.err.println(
		"Usage: jsxwrap -f|-c model objects/libraries");
	    System.exit(1);
	}
}

