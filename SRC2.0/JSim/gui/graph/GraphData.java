/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Data for graph
 
package JSim.gui.graph;

import java.awt.*;
import java.util.ArrayList;

public class GraphData {

	// shape to render (XY plots only)
	public static final int SHAPE_NONE = 0; // none
	public static final int SHAPE_CIRCLE = 1; // 
	public static final int SHAPE_TRIANGLE = 2; // 
	public static final int SHAPE_SQUARE = 4; // 
	public static final int SHAPE_DIAMOND = 5; // 
	public static final int SHAPE_STAR = 3; // 
	public static final int SHAPE_ASTERISK = 9; // 

	// line connect styles 
	public static final int LINE_NONE = 0; // no line
	public static final int LINE_SOLID = 1; // solid line 
	public static final int LINE_SHORTDASH = 2; //  
	public static final int LINE_LONGDASH = 3; //  
	public static final int LINE_DOT = 4; //  
	public static final int LINE_DOTDASH = 5; //  
	public static final int LINE_DOTDOTDASH = 6; //  
	public static final int LINE_DOTDOTDOTDASH = 7; //  
	public static final int LINE_SDASHLDASH = 8; //  
	public static final int LINE_DOTSDASHLDASH = 9; //  
	public static final int LINE_DOTSDASHDOTLDASH = 10; //  

	// line thicknesses
	public static final int LINE_THIN = 0;
	public static final int LINE_MEDIUM = 1;
	public static final int LINE_THICK = 2;

	// colorMap options for contour plots
	public static final int COLORMAP_NONE = 0; // no colormap
	public static final int COLORMAP_AREA_FILL = 1; // 
	public static final int COLORMAP_RASTER = 2; // 

	// palette options for contour & surface plots
	public static final int PALETTE_GRAYSCALE = 0;
	public static final int PALETTE_RED = 1;
	public static final int PALETTE_GREEN = 2;
	public static final int PALETTE_BLUE = 3;
	public static final int PALETTE_HEAT = 4;
	public static final int PALETTE_RAINBOW = 5;
	public static final int PALETTE_PET = 6;

	// instance data
	public String label; // legend label
	public double[] x; // X data
	public double[] y; // Y data
	public double[] z; // Z data, if required
	public boolean isConstant; // data constant over axis range?
	public Color color; // AWT color to render
	public int shape; // render shape
	public int size; // point size for symbol 
	public int line; // line style, see above
	public int thickness; // line thickness, see above
	public int colorMap; // 2D data colormap, see above
	public int palette; // colormap palette, see above
	public String help; // help database key,  or null
	public Subset subset; // if non-null, plot only this subset

	// get X/Y flipped Z data
	public double[] zflip() {
	    if (x==null || y==null || z==null)
		return null;
	    if (x.length * y.length != z.length)
		return null;
	    double zflip[] = new double[z.length];
	    //int minor = y.length;
	    int minor = x.length;
	    //int major = x.length;
	    int major = y.length;
	    for (int i=0; i<z.length; i++) {
		int j1 = i / minor;
		int j2 = i % minor;
		int j = j2*major + j1;
		zflip[j] = z[i];
	    }
	    return zflip;
	}

	// GraphData.Subset - subset of data from GraphData
	public static class Subset {
	    public int lox;  // low index to overdraw inclusive
	    public int hix;  // high index to overdraw exclusive
	    public boolean y; // lox, hix apply to xdata or ydata?
	    // data indices to draw are for (i=lox; i<hix; i++)
	    //     y=false applies to xdata (2D or 3D)
	    //     y=true applies to ydata (3D only)

	    // make copy
	    public Subset copy() {
		Subset s = new Subset();
		s.lox = lox;
		s.hix = hix;
		s.y = y;
		return s;
	    }

	    // string rep
	    public String toString() {
		return "" + lox + "-" +  hix;
	    }
	}
	
	// GraphData.List 
	public static class List extends ArrayList<GraphData> {
	    public List() { super(); }
	    public GraphData gdata(int i) { 
		return (GraphData) get(i);
	    }
	}
}

