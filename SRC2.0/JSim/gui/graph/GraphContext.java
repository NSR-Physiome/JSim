/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Context for graph,  allows saving 

package JSim.gui.graph;

import java.awt.*;
import java.awt.print.*;
import javax.swing.*;

public interface GraphContext {

	// user has updated editable labels
	public void updateEditables(GraphLayout.Editables e);

	// user has selected axis ranges (e.g. XY clip rectangle)
	public void updateAxisRanges(GraphLayout layout,
	    double x1, double y1, double x2, double y2);

	// set system help key
	public void setHelp(JComponent jcomp, String key);

	// is help system active?
	public boolean helpActive();

	// show help for key,  or send nulls to hide help
	public void showHelp(Rectangle r, String key);
}

