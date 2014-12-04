/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Test program for RTXMatlab

package JSim.tests; import JSim.jruntime.matlab.*;

public class RTXMatlabTest {

	public static final void main(String args) throws Exception {
	    RTXMatlab engine = new RTXMatlab();

	    double aval[][] = new double[1][1];
	    aval[0][0] = 7;
	    engine.setValue("a", aval);

	    engine.runCommand("b = a^2;");

	    double bval[][] = engine.getValue("b");

	    System.out.println("b=" + bval[0][0]);
	}
}

