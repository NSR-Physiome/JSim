/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Data item for Nested graph

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import java.util.*;
import org.w3c.dom.*;

public class PNestedItem extends PNamed {

	// controls
	public PNamedControl dataSrc; // model or dataset
	public PNestedDataControl   expr; // main data
	public ChoiceControl color;
	public ChoiceControl size;
	public ChoiceControl shape;
	public ChoiceControl line;	// line style
	public ChoiceControl thickness;	// line thickness
	public ChoiceControl colorMap; 
	public ChoiceControl palette;
	public BooleanControl show;
	public PNestedDataControl xExpr; // X axis expr,  if any

	// local 
	private LinkedHashMap<String, PNestedAxis.Local> localAxes;
	private PNestedItemSliders sliders;

	// constructor
	public PNestedItem(PNested p, String n, int inx) throws Xcept {
	    super(p, n);
	    dataSrc = new PNamedControl(this, "src", project(), 
		new Class[] { PModel.class, PDataSet.class });
	    expr = new PNestedDataControl(this, "expr", dataSrc);
	    color = new ChoiceControl(this, "color", inx % 13,
		new String[] { "black", "red", "orange", "yellow",
		    "green", "blue", "indigo", "violet2",
		    "gray", "forest", "salmon", "brown", "violet" } );	    
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
	    xExpr = new PNestedDataControl(this, "xExpr", dataSrc);

	    // init axes (Z needed ???)
	    localAxes = new LinkedHashMap<String, PNestedAxis.Local>();
	    for (int i=0; i<PNestedAxis.ALLIDS.length; i++) {
	        String id = PNestedAxis.ALLIDS[i];
	    	localAxes.put(id, new PNestedAxis.Local(this, id));
	    }
	    sliders = new PNestedItemSliders(this, "sliders");
	}

	// is domain name assigned to an active axis
	public boolean isDomainAssigned(String dom) {
	    for (int i=0; i<PNestedAxis.ALLIDS.length; i++) {
	        String id = PNestedAxis.ALLIDS[i];
		PNestedAxis.Local paxis = localAxis(id);
		if (! paxis.isActive()) continue;
		if (paxis.domain.val().equals(dom)) return true;
	    }
	    return false;
	}   

	// slider values
	public void setSliderValue(String domainID, double value) 
	throws Xcept {
	    sliders.setSliderValue(domainID, value);
	}
	public double getSliderValue(String domainID) {
	    return sliders.getSliderValue(domainID);
	}

	// query
	public String diagInfo() { return "NestedItem " + name; }
	public String xmlLabel() { return "nesteditem"; }
	public PNestedAxis.Local localAxis(String id) { 
	    return localAxes.get(id); }
	public StringList getDomainNames() throws Xcept {
	    return expr.getDomainNames();
	    // should use xExpr too
	}
	public boolean isModel() {
	    return dataSrc.pnamed() instanceof PModel;
	}
	public boolean isDataSet() {
	    return dataSrc.pnamed() instanceof PDataSet;
	}
	public PModel pmodel() {
	    if (! isModel()) return null;
	    return (PModel) dataSrc.pnamed();
	}
	public PDataSet pdataset() {
	    if (! isDataSet()) return null;
	    return (PDataSet) dataSrc.pnamed();
	}
	public Data getData(int store) {
	    return expr.getData(store);
	    // if xExpr valid,  make PlotData instead
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

