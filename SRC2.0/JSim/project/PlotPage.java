/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Single Project Plot Page

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.Element;

public class PlotPage extends PNamed {

	// controls
	public IntControl confPlot; // plot # being configured
	public IntControl nRow;	// # rows of plots in page
	public IntControl nCol; // # cols of plots in page
	public BooleanControl liveUpdate; // update pages during run?
	public BooleanControl showConf; // show config area?
	public BooleanControl showXRule; // vertical rule
	public RealControl xRule; // vertical rule position (0-1)
	public BooleanControl showYRule; // horiz rule
	public RealControl yRule; // horiz rule position (0-1)
	public BooleanControl showMinMaxSliders; // minmax sliders
	public RealControl animeDuration; // total #secs for animation
	public RealControl animeRate; // animation frames/sec
	public BooleanControl animeLoop; // looping animation
	public BooleanControl animeFwd; // forward=true, backward=false

	// private state
	private Plot.List plots;

	// constructor
	public PlotPage(PNamed p, String n) throws Xcept {
	    super(p, n);
	    plots = new Plot.List(4);
	    addDescCntl();
	    confPlot = new IntControl(this, "confPlot", 0);
	    nRow = new IntControl(this, "nRow", 1) {
		public void updateOther() throws Xcept {
		    reconfig();
		}
	    };
	    nCol = new IntControl(this, "nCol", 1) {
		public void updateOther() throws Xcept {
		    reconfig();
		}
	    };
	    liveUpdate = new BooleanControl(this, "liveUpdate", false);
	    showConf = new BooleanControl(this, "showConf", true);
	    showXRule = new BooleanControl(this, "showXRule", false);
	    xRule = new RealControl(this, "xRule", 0.5);
	    showYRule = new BooleanControl(this, "showYRule", false);
	    yRule = new RealControl(this, "yRule", 0.5);
	    showMinMaxSliders = new BooleanControl(this, 
		"showMinMaxSliders", false);
	    animeDuration = new RealControl(this, "animeDuration", 10);
	    animeRate = new RealControl(this, "animeRate", 10);
	    animeLoop = new BooleanControl(this, "animeLoop", false);
	    animeFwd = new BooleanControl(this, "animeDir", true);
	    reconfig();
	}

	// change rows & cols
	public void reconfig() throws Xcept {
	    int ct = nRow.val() * nCol.val();
	    int ct0 = plots.size();

	    // add missing plot children
	    for (int i=ct0; i<ct; i++) 
		plots.add(new Plot(this, "plot" + i));

	    // remove extra plot children
	    for (int i=ct0-1; i>=ct; i--) {
		Plot plot = plot(i);
		int inx = plots.indexOf(plot);
		if (inx <= 0) throw new Xcept(
		    "Internal error removing plot " + i);
		plots.remove(plot);
		remove(plot);
	    } 

	    if (confPlot.val() >= ct)
		confPlot.setVal(0);
	}

	// query
	public String diagInfo() { return "PlotPage " + name; }
	public String xmlLabel() { return "plotpage"; }
	public int nPlot() { return plots.size(); }
	public Plot plot(int i) { 
	    return (Plot) plots.get(i); 
	}
	public Plot plot(int row, int col) throws Xcept {
	    int i = nRow.val()*col + row;
	    return (Plot) plots.get(i); 
	}
	public Plot confPlot() { return plot(confPlot.val()); }

	// add plot exprs for given model
	public void addExprs(PModel pmodel, StringList list) 
	throws Xcept {
	     for (int i=0; i<nPlot(); i++)
	     	plot(i).addExprs(pmodel, list);
	}

	// add raw data to list
	public void addData(Data.List list) throws Xcept {
	    for (int i=0; i<nPlot(); i++)
		plot(i).addRawData(list);
	}

	// import XML element into matching PNamed child
	public PNamed importXMLChild(Element c) {

	    // ignore legacy fields (before 1.6.45)
	    String cname = c.getAttribute("name");
	    if (cname.equals("showXScroll")) return null;
	    if (cname.equals("showYScroll")) return null;

	    return super.importXMLChild(c);
	}

	// revalidate
	public void revalidate() {
	    super.revalidate();
	    try {
	    	if (confPlot.val() >= nPlot()) 
		    confPlot.setVal(0);
	    } catch (Xcept e) { }
	}
}

