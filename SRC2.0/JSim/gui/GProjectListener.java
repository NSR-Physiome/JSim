/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// listen to top-level GProject changes

package JSim.gui; 

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public interface GProjectListener {

  	// job has started
	void jobStarted(PJob pjob);
	
	// job has stopped (may have failed!)
	void jobStopped(PJob pjob);
	
	// content added or removed
	void projectContentChanged();

}

