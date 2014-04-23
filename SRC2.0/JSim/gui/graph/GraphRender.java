/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Graph renderer,  keeps track of single graph

package JSim.gui.graph;

import java.io.File;
import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;

public interface GraphRender extends Printable {

	// return top-level Swing component
	//     so that it can be embedded in larger application
	public JComponent jcomp();

	// draw entire or updated layout
	//   if subsets==null || l!=stored layout
	//      draw all data, i.e.   clear area, 
	//          draw axes, labels & all data points)
	//   else 
	//      during first refresh
	//          data only most subset[] data i.e.
	//             do not clear area, draw axes or labels
	//             and draw only GraphData points for 	
	//             which subset[i] != null)
	//      on subsequent refreshes, however,  draw all data     
	public void setGraphLayout(GraphLayout l, 
	GraphData.Subset[] subsets) throws Exception;

	// change axis bounds only
	public void changeAxisBounds(double[] xbounds,
	double[] ybounds,  double[] zbounds) throws Exception;

	// print composite
	public int printComposite(Graphics g, PageFormat pf, int inx, 
                              int idx, int nrow, int ncol)
        throws PrinterException;

	// export Encapsulated Post-Script
	public void exportEPS(File f) throws Exception;

	// mouse listener (jcomp() may not work properly
	public void addMouseListener(MouseListener l);
}

