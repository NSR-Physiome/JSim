/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  Data curve import function

package JSim.project; 

import JSim.project.*;
import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*;

public class FuncGenData extends FuncGenSlave {

	// controls
	public ChoiceControl ndim;
	public PNamedControl dataSet;
	public DataControl name;

	// state
	private RealNData ndata; // loaded at run-time

	// constructor
	public FuncGenData(FuncGen p, String n, String d) 
	throws Xcept {
	    super(p,n,d);
	}
	
	// make controls - don't use defaults
	protected void makeControls() throws Xcept {
	    ndim = new ChoiceControl(this, "ndim", 0,
		new String[] { "1", "2", "3" });
	    dataSet = new PNamedControl(this, "dataSet", project(), 
		new Class[] { PDataSet.class });
	    name = new DataControl(this, "name", dataSet);
	    ndata = null;
	}

	// query
	public int ndim() { return ndim.val()+1; }

	// get required data
	public Data getSourceData() {
	    return name.getData(0);
	}
	    
	// set source data
	public void setSourceData(Data data) {
	    name.setSourceData(data);
	}
}
