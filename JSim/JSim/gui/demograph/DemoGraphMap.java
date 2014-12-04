/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// colormap renderer for DemoGraph

package JSim.gui.demograph;

import java.awt.*;
import JSim.util.*;
import JSim.gui.graph.*;

public class DemoGraphMap {
	private DemoGraph graph;
	private Color[] cols;
	private static final int NCOL = 64;

	// constructor
	public DemoGraphMap(DemoGraph t) {
	    graph = t;
	}

	// draw colormap
	public void paint(Graphics g, int w, int h) {
	    int sp = 4;
	    int x1 = w/10;
	    int x2 = w-x1;
	    int y1 = h/10;
	    int y2 = h-y1;
	    GraphData.Subset[] subsets = graph.getClearSubsets();

	    // layout check
	    if (layout() == null) {
	    	g.setColor(Color.white);
	    	g.fillRect(0,0,w,h);
		g.drawString("No data", w/2, h/10);
		return;
	    }

	    // background clear, border
	    if (subsets == null) {
	    	g.setColor(Color.white);
	    	g.fillRect(0,0,w,h);
	    	g.setColor(Color.blue);
	    	g.drawRect(sp,sp,w-1-sp-sp,h-1-sp-sp);
	    	g.setColor(Color.black);
	    	g.drawString("Colormap", w/2, h/10);
	    	g.setColor(Color.red);
	    	g.drawRect(x1, y1, x2-x1, y2-y1);
	    }

	    // calculate cell draw limits
	    if (layout().data == null ||
		layout().data.length<1) return;
	    GraphData data = layout().data[0];
	    if (data.x == null) return; // empty
	    GraphData.Subset dataSS = (subsets != null) ?
		subsets[0] : null;
	    int xct = data.x.length;
	    int yct = data.y.length;
	    int zct = data.z.length;
	    if (xct*yct != zct) {
	    	g.drawString("zct error", w/2, h/10);
		return;
	    }
	    int xlo = 0;
	    int xhi = xct;
	    int ylo = 0;
	    int yhi = yct;
	    if (dataSS != null) {
		if (dataSS.y) {
		    ylo = dataSS.lox;
		    yhi = dataSS.hix;
		} else { 
		    xlo = dataSS.lox;
		    xhi = dataSS.hix;
		}
	    }


	    // draw cells
	    int xsize = (x2-x1)/xct+1;
	    int ysize = (y2-y1)/yct+1;
	    for (int x=xlo; x<xhi; x++) {
		int xpos = x1 + (int) ((x2-x1)*x/(xct+0.0));
		for (int y=ylo; y<yhi; y++) {
		    int ypos = y1 + (int) ((y2-y1)*y/(yct+0.0));
		    int z = y*xct + x;
		    double zval = data.z[z];  
		    if (Double.isNaN(zval)) continue;
		    double zfrac = (zval - layout().zaxis.min)
			/ (layout().zaxis.max - layout().zaxis.min);
		    Color col = color(zfrac);
		    g.setColor(col);
		    g.fillRect(xpos, ypos, xsize, ysize);
		}
	    }
	}

	// drawing color
	public Color color(double frac) {
	    if (cols == null) {
		cols = new Color[NCOL];
		for (int i=0; i<NCOL; i++) {
		    int r = 0;
		    int g = (int) (i*255.0/NCOL); 
		    int b = 255-g; 
		    cols[i] = new Color(r, g, b);
		}
	    }
	    int n = (int) (frac*NCOL);
	    if (n<0) n=0;
	    if (n>=NCOL) n=NCOL-1;
	    return cols[n];
	}

	// query
	public GraphLayout layout() { 
	    return graph.layout;
	}
}


