/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
/* EXAMPLE CODE ONLY 
    This program shows how to call the JSim engine for some basic functions:
	reads a project file
	compiles a model
	sets an input variable
	runs the model
	queries an output variable and prints its values    
*/

package JSim.tests;	
import java.io.*;	
import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.project.*;

public class Example1 {

	// mainline
	public static final void main(String[] args)
	throws Exception {

	    // parse command line
	    if (args.length != 5) throw new Xcept(
		"Example2 usage: project-file model input-variable input-value output-variable");
	    JSReadable projFile = new JSReadable(args[0]);
	    String modelName = args[1];
	    String uname = args[2];
	    String uvalue = args[3];
	    String vname = args[4];

	    // load application server and project
	    PApplication appl = new PApplication();
	    Project proj = new Project("proj", appl);
	    proj.importXML(projFile);

	    // find and compile model
	    PModel pmodel = (PModel) proj.child(modelName);
	    PJob pjob = new PModelBuildJob(pmodel);
	    pjob.simpleRun();
	    ASModel rt = pmodel.rt();

	    // set input variable
	    ASVar u = rt.getASVar(uname);
	    u.setAssign(uvalue);

	    // run model
	    pjob = new PModelRunJob(pmodel);
	    pjob.simpleRun();
	    
	    // query output variable
	    ASVar v = rt.getASVar(vname);
	    Data data = rt.getData(0, v);

	    // print output in PrettyFormat
	    DataFormat fmt = new PrettyDataFormat();
	    DataWriter wrt = fmt.createWriter();
	    Data.List list = new Data.List(1);
	    list.add(data);
	    wrt.writeData(System.out, list);
	}

}
