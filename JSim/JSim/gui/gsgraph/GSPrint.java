/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// printer functionality for GSGraph
 
package JSim.gui.gsgraph;
 
import java.io.*;
import java.awt.*;
import java.awt.print.*;
import javax.swing.*;

import JSim.util.*;
import JSim.gui.graph.*;

import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.*;
import org.jibble.epsgraphics.EpsGraphics2D;

public class GSPrint {
	private GSGraph graph;
	private GSGeom geom;

	// constructor
	public GSPrint(GSGraph g) {
	    graph = g;
	    geom = graph.geom();
	}

	// print one graph on a (possibly) composite page
	public int printComposite(Graphics g, PageFormat pf, 
	int inx, int idx, int nrow, int ncol)  {
	    if (inx > 0) return(Printable.NO_SUCH_PAGE);

	    // original size may be modified by printLegend
	    int wpt = geom.layerDim.width;     
	    int hpt = geom.layerDim.height;
	    Graphics2D g2d = (Graphics2D) g;
		g2d.setFont(geom.font);
		FontMetrics metrics = g2d.getFontMetrics();
		   
	    // if only one graph on page, get additional legend
	    String[] legend = null;
	    int ct = 0;
	    if (nrow == 1 && ncol == 1) {
	        legend = (graph.graphLayout() == null) ?
	        null : graph.graphLayout().printLegend;
	        ct = (legend == null) ? 0 : legend.length;

	        // leave room for additional legend
	        hpt += (int) (metrics.getHeight()*ct);
	    }

	    // translate to appropiate cell        
	    int row = idx / ncol;
	    int col = idx % ncol;    
	    g2d.translate(pf.getImageableX(), pf.getImageableY());
	    g2d.translate(col*wpt, row*hpt);   

	    // if 1st in series, scale to avail space preserving aspect ratio
	    if (idx == 0) { 
	        double aw = pf.getImageableWidth() / ncol;    
	        double ah = pf.getImageableHeight() / nrow;
	        double sf = Math.min(aw/wpt, ah/hpt);
	        g2d.scale(sf, sf);
	    }

	    // if only one graph on page, add printLegend
	    // has to be done first, else legend will be clipped
	    if (nrow == 1 && ncol == 1 && ct > 0) {
	        for (int i=0; i<ct; i++) {
	            g2d.drawString(legend[i],(float) 0,
				(float) (geom.layerDim.height
				+metrics.getHeight()*(i+1)-metrics.getDescent()));
	        }
	    }

	    // draw into graphics
	    graph.draw(g2d);

	    // translate back to starting position before next graph
	    g2d.translate(-col * wpt, -row * hpt);
	    g2d.translate(-pf.getImageableX(), -pf.getImageableY());

	    return(Printable.PAGE_EXISTS);
	}

	// export EPS using epsgraphics toolkit (for menu item)
	public void exportEPS(File f) throws IOException {

	    Graphics2D g2d = new EpsGraphics2D();
	    graph.draw(g2d);
	    String epsout = g2d.toString();

	    Color saveColor = graph.getBackground();

	    if (!saveColor.equals(Color.white)) {
	        graph.setBackground(Color.white);
	    }

	    RepaintManager currentManager = 
	        RepaintManager.currentManager(graph);
	    currentManager.setDoubleBufferingEnabled(false);

	    // write to a file
	    try {
	        BufferedWriter out = 
	            new BufferedWriter(new FileWriter(f));
	            out.write(epsout);
	            out.newLine();
	            out.close();
	    } catch (IOException e) { 
	        System.out.println("Error writing file: " + e);
	    }

	    currentManager.setDoubleBufferingEnabled(true);
	    graph.setBackground(saveColor);

	    // Graphics context no longer needed: get rid of it
	    g2d.dispose();
	}
}
