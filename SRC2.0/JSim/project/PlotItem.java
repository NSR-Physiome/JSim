/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// One Plot item

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

public class PlotItem extends PNamed {

	// controls
	public PNamedControl dataSrc; // model or dataset
	public PlotDataControl   expr; // expr to plot
	public ChoiceControl color;
	public ChoiceControl size;
	public ChoiceControl shape;
	public ChoiceControl line;	// line style
	public ChoiceControl thickness;	// line thickness
	public ChoiceControl colorMap; 
	public ChoiceControl palette;
	public BooleanControl show;
	public PlotDataControl xExpr; // X axis expr,  if any
	public PlotDataControl yExpr; // Y axis expr,  if any (3D only)
	public StringControl label; // data curve label
	public RealControl labelX; // label X pos
	public RealControl labelY; // label Y pos

	// constructor
	public PlotItem(PNamed p, String n, int inx) throws Xcept {
	    super(p, n);
	    dataSrc = new PNamedControl(this, "src", project(), 
		new Class[] { PModel.class, PDataSet.class });
	    expr = new PlotDataControl(this, "expr", dataSrc, false);
	    color = new ChoiceControl(this, "color", inx % 13,
					  new String[] { "black", "red", "green", "blue", "orange", 
             "turquoise","violet2", "indigo", "gray", "forest", "salmon", 
			 "brown", "violet" } );	    
	    size = new ChoiceControl(this, "size", 1,
		new String[] { "tiny", "small", "normal", "big" } );	    
	    shape = new ChoiceControl(this, "shape", 0,
		new String[] { "none", "circle", "triangle", "start", "square", "diamond" } );	    
	    line = new ChoiceControl(this, "line", 1,
		new String[] { "none", "solid", "shortdash", "longdash",
		    "dot", "dotdash", "dotdotdash", "dotdotdotdash",
		    "sdashldash", "dotsdashldash", "dotsdashdotldash" } );	    
	    thickness = new ChoiceControl(this, "thickness", 
	        appl().defaultPlotLineThickness(),
		new String[] { "thin", "medium", "thick" } );	    
	    colorMap = new ChoiceControl(this, "colorMap", 0,
		new String[] { "none", "area_fill", "raster" } );	    
	    palette = new ChoiceControl(this, "palette", 4,
		new String[] { "grayscale", "red", "green", "blue", "heat", "rainbow", "PET" } );	    
	    
	    show = new BooleanControl(this, "show", true);
	    xExpr = new PlotDataControl(this, "xExpr", dataSrc, true);
	    yExpr = new PlotDataControl(this, "yExpr", dataSrc, true);
	}

	// query
	public String diagInfo() { return "PlotItem " + name; }
	public String xmlLabel() { return "plotitem"; }
	public boolean isLive() {
	    PNamed p = dataSrc.pnamed();
	    if (! (p instanceof PModel)) return false;
	    PModel pmodel = (PModel) p;
	    return pmodel.isRunning();
	}
	public boolean isActive() {
	    return expr.valid() && show.val();
	}
	public Plot plot() { return (Plot) parent; }
	public int nQueryStores() {
	    PNamed dsrc = dataSrc.pnamed();
	    return (dsrc instanceof PModel) ?
	    	((PModel) dsrc).nQueryStores() : 1;
	}
	public long timeStamp() {
	    PNamed p = dataSrc.pnamed();
	    if (! (p instanceof PModel)) return 0;
	    PModel pmodel = (PModel) p;
	    return pmodel.timeStamp;
	}
	    
	// add plot exprs for model to list
	public void addExprs(PModel pmodel, StringList list)
	throws Xcept {
	    if (! show.val()) return;
	    if (expr.base() != pmodel) return;
	    ASQuery e = expr.getExpr();
	    if (e != null) list.add(e.toString());
	}

	// get all data for this plot item
	public PlotData.List getDataList() throws Xcept {
	    Data.List list = new Data.List(1);
	    try {
	    	addRunData(list);
	    } catch (Xcept e) {
		// no ugly messages for now,  later?
	    }
	    return list;
	}

	// add single or multiple run data to list
	private void addRunData(Data.List list) throws Xcept {
	    int n = nQueryStores();
	    for (int i=0; i<n; i++) {
		PlotData data = getPlotData(i);
		if (data == null) continue;
		list.add(data);
	    }
	}

	// get data for single model run
	public PlotData getPlotData(int run) throws Xcept {
	    if (! show.val()) return null;
	    Data data = expr.getData(run);
	    if (data == null) return null;
	    Data xdata = xExpr.getData(run);
	    if (data.ndim() == 1) 
		return new PlotData(data, 
		    new Data[] { xdata });
	    if (data.ndim() == 2) {
		Data ydata = yExpr.getData(run);
		return new PlotData(data, 
		    new Data[] { xdata, ydata });
	    }
	    return new PlotData(data);
	}

	// revalidate, fills in blank dataSrc if expr is valid
	public void revalidate() {
	    super.revalidate();

	    // if valid expression with default dataSrc, update
	    if (expr.isBlank()) return;
	    if (! dataSrc.isBlank()) return;
	    PNamed p = dataSrc.pnamed();
	    if (p == null) return;
	    try {
		dataSrc.setVal(p.name());
	    } catch (Xcept e) {
		// nothing in particular to do
	    }
	}
}

