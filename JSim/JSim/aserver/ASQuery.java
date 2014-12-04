/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// parsed data query to model

package JSim.aserver;

import JSim.util.*;
import JSim.data.*;

public interface ASQuery {

	public String unitString();
	
	public int ndim();

	public boolean isConst();
	
	public double constRealVal() throws Xcept; // ???NEEDED, NaN

	public StringList getDomainNames();
}

