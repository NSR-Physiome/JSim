/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Single Project Plot 

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.Element;

public class Plot extends PNamed {

	// constants
	public static final int RUN_MODE = 0;
	public static final int SENS_MODE = 1;

	public static final int DFFDPP = 0;
	public static final int DFDP = 1;
	
	public static final int X_AXIS = 0; // for confAxis below
	public static final int Y_AXIS = 1;
	public static final int Z_AXIS = 2;

	public static final int XY_PLOT = 0; // for style below
	public static final int CONTOUR = 1; 
	public static final int SURFACE = 2; 

	public static String FOOTER_TIME = "%TIME";
	public static String TITLE_DEFAULT = "[Title]";

	// controls
	public IntControl nItems;
	public StringControl title; // title text
	public RealControl titleX;  // title X position
	public RealControl titleY;  // title Y position
	public StringControl footer; // footer text
	public RealControl footerX;  // footer X position
	public RealControl footerY;  // footer Y position
	public BooleanControl showLegend;
	public BooleanControl showTitle;
	public BooleanControl showFooter;
	public RealControl legendX;  // legend X posiiton
	public RealControl legendY;  // legend Y posiiton
	public ChoiceControl style;
	public IntControl confItem;
	public ChoiceControl confAxis;

	// axes
	public PlotAxis xaxis;
	public PlotAxis yaxis;
	public PlotAxis zaxis;

	private PlotItem.List items;
	private PlotZoom.Stack zoomStack;

	// constructor
	public Plot(PlotPage p, String n) throws Xcept {
	    super(p, n);
	    items = new PlotItem.List(4);

	    nItems = new IntControl(this, "nItems", 1) {
		public void updateOther() throws Xcept {
		    reconfig();
		}
	    };		
	    title = new StringControl(this, "title", TITLE_DEFAULT);
	    titleX = new RealControl(this, "titleX", 0.5);
	    titleY = new RealControl(this, "titleY", 1);
	    footer = new StringControl(this, "footer", FOOTER_TIME);
	    footerX = new RealControl(this, "footerX", 0);
	    footerY = new RealControl(this, "footerY", 0);
	    showLegend = new BooleanControl(this, "showLegend", true);
	    showTitle = new BooleanControl(this, "showTitle", true);
	    showFooter = new BooleanControl(this, "showFooter", true);
	    legendX = new RealControl(this, "legendX", 1);
	    legendY = new RealControl(this, "legendY", 1);
	    style= new ChoiceControl(this, "style", 0,
		new String[] { "XY plot", "contour", "surface" }) {
		public void updateOther() throws Xcept {
		    if (val()==XY_PLOT && confAxis.val()==Z_AXIS)
			confAxis.setVal(X_AXIS);
		}
	    };
	    confItem = new IntControl(this, "confItem", 0) {
		public void updateOther() throws Xcept {
		    if (val()>=nItems.val()) 
			nItems.setVal(val()+1);
		}
	    };		
	    confAxis = new ChoiceControl(this, "confAxis", 1,
		new String[] { "X", "Y", "Z" });

	    // need PPlotAxis children
	    xaxis = new PlotAxis(this, "xaxis");
	    yaxis = new PlotAxis(this, "yaxis");
	    zaxis = new PlotAxis(this, "zaxis");
	    zoomStack = new PlotZoom.Stack(this);

	    reconfig();
	}

	// change nitems
	public void reconfig() throws Xcept {
	    int ct = nItems.val();
	    if (ct<1) ct=1;
	    int ct0 = items.size();

	    // add missing plot children
	    for (int i=ct0; i<ct; i++) 
		items.add(new PlotItem(this, "item" + i, i));

	    // remove extra plot children
	    for (int i=ct0-1; i>=ct; i++) {
		PlotItem item = item(i);
		int inx = items.indexOf(item);
		if (inx <= 0) throw new Xcept(
		    "Internal error removing plot item " + i);
		items.remove(item);
		remove(item);
	    }

	    if (confItem.val() >= ct)
		confItem.setVal(0);
	}

	// remove blank plot items from end of list
	public void trimPlotItems() throws Xcept {
	    int ct=items.size();
	    while (ct>1 && item(ct-1).expr.isBlank()) ct--;
	    nItems.setVal(ct);
	    reconfig();
	}

	// query
	public PlotPage page() { return (PlotPage) parent(); }
	public PlotZoom.Stack zoomStack() { return zoomStack; }
	public String diagInfo() { return "Plot " + name; }
	public String xmlLabel() { return "plot"; }
	public int nItem() { return items.size(); }
	public PlotItem item(int i) { 
	    return (PlotItem) items.get(i); 
	}
	public PlotItem confItem() { return item(confItem.val()); }
	public PlotAxis confAxis() { 
	    switch (confAxis.val()) {
	    case 0: return xaxis;
	    case 1: return yaxis;
	    case 2: return zaxis;
	    }
	    return null;
	}
	public boolean hasBlankItems() {
	    for (int i=0; i<items.size(); i++) 
		if (item(i).expr.isBlank()) return true;
	    return false;
	}
	public PNamed.List activeDatasrc() {
	    PNamed.List list = new PNamed.List(4);
	    for (int i=0; i<items.size(); i++) {
		PlotItem item = item(i);
		if (item.isActive()) 		
	            list.addUniq(item.dataSrc.pnamed()); 
	    }
	    return list;
	}
	public int naxes() {
	    return (style.val() == XY_PLOT) ? 2 : 3;
	}

	// insert exprs into plot: HACK FOR DEMO MODE!!!
	public void insertExprs(PModel pmodel, StringList exprs)
	throws Xcept {
	    int ct = exprs.size();
	    if (ct > nItem()) {
	    	nItems.setVal(ct);
		reconfig();
	    }
	    for (int i=0; i<ct; i++) {
	        PlotItem item = item(i);
		item.dataSrc.setVal(pmodel.name());
		item.expr.setVal(exprs.str(i));
	    }
	}   

	// add raw data to list
	public void addRawData(Data.List list) throws Xcept {
	    for (int i=0; i<nItem(); i++) {
		PlotItem item = item(i);
		PlotData.List plotlist = item.getDataList();
		for (int j=0; j<plotlist.size(); j++) {
		    PlotData data = (PlotData) plotlist.get(j); 
		    data.addData(list);
		}
	    }
	}

	// add plot exprs for model to list
	public void addExprs(PModel pmodel, StringList list)
	throws Xcept {
	    for (int i=0; i<nItem(); i++) 
		item(i).addExprs(pmodel, list);
	}	

	// add plot data to list
	public void addPlotData(Data.List list) throws Xcept {
	    for (int i=0; i<nItem(); i++) {
		PlotItem item = item(i);
		PlotData.List plotlist = item.getDataList();
		list.addAll(plotlist);
	    }
	}
	
	// import XML element into matching PNamed child
	public PNamed importXMLChild(Element c) {

	    // ignore legacy fields (before 1.6.58)
	    String cname = c.getAttribute("name");
	    if (cname.equals("updateFreq")) return null;

	    return super.importXMLChild(c);
	}

	// revalidate
	public void revalidate() {
	    super.revalidate();
 	    try {
		if (confItem.val() > nItem())
		    confItem.setVal(0);
		if (confAxis.val() == Z_AXIS &&
		    style.val() == XY_PLOT)
		    confAxis.setVal(0);
	    } catch (Xcept e) { }
	}
}

