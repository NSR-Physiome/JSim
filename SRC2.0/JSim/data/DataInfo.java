/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// serializable JSim data

package JSim.data;

import java.io.*;

import JSim.util.*;
import JSim.expr.*;

public abstract class DataInfo implements Serializable {
	public String name;
	public String desc;
	public String group;
	public Unit.Info unit;
	public Data.Subset subset;

	// constructors
	public DataInfo() { }
	public DataInfo(Data data) {
	    name = data.name;
	    desc = data.desc;
	    if (data.unit != null)
		unit = new Unit.Info(data.unit);
	    group = data.group;
	    subset = data.subset;
	}
}

