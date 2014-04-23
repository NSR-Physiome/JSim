/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// generate zero-finder module based on exp() taylor series

package JSim.tests;
import JSim.util.*;

public class FzeroExp {

	// generate .mod code
	public static void main(String[] args) throws Exception {
	    if (args.length != 2) throw new Xcept(
	    	"Usage: FzeroExp n linear/simplex/ggopt");
	    int n = Util.toInt(args[0]);
	    String flag = args[1];
	    if (! flag.equals("linear") 
	    && ! flag.equals("simplex")
	    && ! flag.equals("ggopt")) throw new Xcept(
	    	"Usage: FzeroExp n linear/simplex/ggopt");
	    println("math main {");
	    for (int i=0; i<n; i++) {
	    	println("\treal a" + i + ";");
		if (flag.equals("simplex"))
		    println("\ta" + i + "<5; a" + i + ">-5;");
	    }
	    for (int t=1; t<=n; t++) {
	    	print("\t");
		int fact = 1;
		for (int i=0; i<n; i++) {
		    if (i > 0) print(" + ");
	    	    print("a" + i + "*" + t + "^" + i +
		    	"/" + fact);
		    fact *= i+1;
		}
		print(" = exp(-" + t + ")");
		if (! flag.equals("linear"))
		    print("*(1 + a0*a1/1e8)");
		println(";");
	    }
	    println("}");
	}

	// print subroutines
	public static void println(String s) {
	    System.out.println(s);
	}
	public static void print(String s) {
	    System.out.print(s);
	}
}
