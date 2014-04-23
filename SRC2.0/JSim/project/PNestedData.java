/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Nested graph data

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import java.util.*;

public class PNestedData {
	private PNested pnested;
	private int style; // XY_PLOT, CONTOUR, SURFACE
	private int scaling; // GLOBAL, PAGE, LOCAL 
	private StringList axisIDs; // axes active for 
	private ArrayList<PNestedDataItem> itemDatas; // active item data
	private int nxgraphs, nygraphs; 
	private ArrayList<Slider> sliders; 
	private StringList innerAxisIDs;
	private DataRange[] commonRanges; // for GLOBAL/PAGE scaling
	private DataRange[][] graphRanges; // for LOCAL scaling

	// constants
	public static final String[] ALLIDS = PNestedAxis.ALLIDS;
	public static final String NO_VARIATION = PNestedDomainControl.NO_VARIATION;
	public static final String INNER_LOOP = PNestedDomainControl.INNER_LOOP;
	public static final String OUTER_LOOP = PNestedDomainControl.OUTER_LOOP;

	// constructor
	public PNestedData(PNested pnested) throws Xcept {
	    this.pnested = pnested;
	    style = pnested.style.val();
	    scaling = pnested.scaling.val();
	    axisIDs = new StringList();
	    for (int i=0; i<ALLIDS.length; i++) {
	    	String id = ALLIDS[i];
		if (gaxis(id).isActive()) 
		    axisIDs.add(id);
	    }
	    nxgraphs = gX2().isActive() ? gX2().nsubAxes.val() : 1;
	    nygraphs = gY2().isActive() ? gY2().nsubAxes.val() : 1;
	    if (nxgraphs < 1) nxgraphs = 1;
	    if (nygraphs < 1) nygraphs = 1;	    
	    innerAxisIDs = new StringList();
	    innerAxisIDs.add(PNestedAxis.X1);
	    innerAxisIDs.add(PNestedAxis.Y1);
	    if (style != PNested.XY_PLOT)
	    	innerAxisIDs.add(PNestedAxis.Z);
	    initRanges();	
	    
	    // load items
	    itemDatas = new ArrayList<PNestedDataItem>();
	    sliders = new ArrayList<Slider>();
	    for (int i=0; i<pnested.items.size(); i++) {
	    	PNestedItem item = pnested.items.get(i);
		if (! item.show.val()) continue;
		if (! item.expr.valid()) continue;
		PNestedDataItem itemData = new PNestedDataItem(this, item);
		if (! itemData.hasData()) continue;
		itemDatas.add(itemData);
		addSliders(itemData);
	    }
	}

	// initialize range tracking
	private void initRanges() {
	    if (scaling == PNested.LOCAL) {
	    	graphRanges = new DataRange[nxy()][nInnerAxes()];
	    	for (int n=0; n<nxy(); n++) 
	    	    graphRanges[n] = makeRanges();
	    } else 
	    	commonRanges = makeRanges();
	}
	private DataRange[] makeRanges() {
	    DataRange[] ranges = new DataRange[nInnerAxes()];
	    for (int i=0; i<nInnerAxes(); i++) 
		ranges[i] = makeRange(innerAxisIDs.get(i));
	    return ranges;
	}
	private DataRange makeRange(String axisID) {
	    PNestedAxis.Global gaxis = gaxis(axisID);
	    boolean isLog = gaxis.log.val();
	    DataRange range = new DataRange(isLog);
	    if (! gaxis.autoscale.val()) {
	    	range.expand(gaxis.dataMin.val(), gaxis.dataMax.val());
		range.setAutoscaleOff();
	    }
	    return range;
	}
			
	// add sliders for item
	private void addSliders(PNestedDataItem itemData) {
	    StringList doms = itemData.getSliderDomains();
	    for (int i=0; i<doms.size(); i++) {
	    	String dom = doms.get(i);
		for (int j=0; j<sliders.size(); j++) {
		    Slider s = sliders.get(j);
		    if (! s.isCompatible(itemData, dom)) continue;
		    s.add(itemData, dom);
		    dom = null;
		    break;
		}
		if (dom != null) {
		    Slider s = new Slider(itemData, dom);
		    sliders.add(s);
		}
	    }
	}
	
	// get graph data
	public Data.List getGraphData() {
	    Data.List dlist = new Data.List();
	    for (int i=0; i<itemDatas.size(); i++) {
	    	PNestedDataItem itemData = itemDatas.get(i);
		for (int j=0; j<itemData.nGraphData(); j++) {
		    Data data = itemData.getGraphData(j);
		    if (data == null) continue;
		    dlist.add(data);
		}
	    }
	    return dlist;
	}
	
	// get graph attributes
	public ArrayList<PNestedDataItem.Attr> getGraphAttrs() {
	    ArrayList<PNestedDataItem.Attr> alist = new 
	        ArrayList<PNestedDataItem.Attr>();
	    for (int i=0; i<itemDatas.size(); i++) {
	    	PNestedDataItem itemData = itemDatas.get(i);
		for (int j=0; j<itemData.nGraphData(); j++) {
		    Data data = itemData.getGraphData(j);
		    if (data == null) continue;
		    alist.add(itemData.getGraphAttr(j));
		}
	    }
	    return alist;
	}
		
	// public query
	public PNested pnested() { return pnested; }
	public int style() { return style; }
	public int scaling() { return scaling; }
	public boolean hasData() { return ! itemDatas.isEmpty(); }
	public int nx() { return nxgraphs; }
	public int ny() { return nygraphs; }
	public int nxy() { return nxgraphs * nygraphs; }
	public int xinx(int n) { return n % nxgraphs; }
	public int yinx(int n) { return n / nxgraphs; }
	public int inx(int x, int y) { return y*nxgraphs + x; }
	public StringList axisIDs() { return axisIDs; }
	public ArrayList<Slider> getSliders() { return sliders; }
	public DataRange[] getCommonRanges() { return commonRanges; }
	public DataRange[] getGraphRanges(int n) { 
	    return (scaling == PNested.LOCAL) ? graphRanges[n] : commonRanges;
	}
	public StringList innerAxisIDs() { return innerAxisIDs; }
	public int nInnerAxes() { return innerAxisIDs.size(); }	
	public String getAxisDomain(String axisID) { 
	    for (int i=0; i<itemDatas.size(); i++) {
	    	String s = itemDatas.get(i).getAxisDomain(axisID);
		if (s != null) return s;
	    }
	    return null;
	}
	public double getAxisDomainValue(String axisID, int inx) { 
	    for (int i=0; i<itemDatas.size(); i++) {
	    	double v = itemDatas.get(i).getAxisDomainValue(axisID, inx);
		if (! Double.isNaN(v)) return v;
	    }
	    return Double.NaN;
	}

	// private query
	private PNestedAxis.Global gaxis(String id) { return pnested.globalAxis(id); }
	private PNestedAxis.Global gX1() { return gaxis(PNestedAxis.X1); }
	private PNestedAxis.Global gX2() { return gaxis(PNestedAxis.X2); }
	private PNestedAxis.Global gY1() { return gaxis(PNestedAxis.Y1); }
	private PNestedAxis.Global gY2() { return gaxis(PNestedAxis.Y2); }
	private PNestedAxis.Global gZ() { return gaxis(PNestedAxis.Y2); }

	// Slider class
	public class Slider {
	    private String name; // public name
	    private ArrayList<PNestedDataItem> itemDatas;
	    private StringList domainIDs;
	    private double value;
	    private double[] grid;
	    
	    // constructor
	    public Slider(PNestedDataItem itemData, String domainID) {
	    	itemDatas = new ArrayList<PNestedDataItem>();
		domainIDs = new StringList();
		add(itemData, domainID);
		grid  = itemData.getSliderGrid(domainID);
		value = itemData.item().getSliderValue(domainID);
		if (Double.isNaN(value)) value = grid[0];
	    }
	    
	    // add item
	    private void add(PNestedDataItem itemData, String domainID) {
	        itemDatas.add(itemData);
		domainIDs.add(domainID);
		recalcName();
	    }
	    
	    // recalculate name
	    private void recalcName() {
	    	PNestedItem item = itemDatas.get(0).item();
		name = item.dataSrc.pnamed().name() + ": " + 
		    domainIDs.get(0);
	    }

	    // is compatible?
	    private boolean isCompatible(PNestedDataItem itemData, 
	    String domainID) {
	    	if (domainIDs.isEmpty()) return true;
		String domainID0 = domainIDs.get(0);
		if (! domainID.equals(domainID0)) return false;
		PNamed dataSrc = itemData.item().dataSrc.pnamed();
		PNestedDataItem itemData0 = itemDatas.get(0);
		PNamed dataSrc0 = itemData0.item().dataSrc.pnamed();
		if (dataSrc != dataSrc0) return false;
		// if (dataSrc instanceof PModel) return true; //acceletator		 
		double[] grid1  = itemData.getSliderGrid(domainID);
		return isGridSame(grid, grid1);
	    }

	    // is grid same?
	    private boolean isGridSame(double[] a, double[] b) {
	    	if (a.length != b.length) return false;
		for (int i=0; i<a.length; i++) 
		    if (a[i] != b[i]) return false; // NaNs ???
		return true;
	    }

	    // update slider value
	    public void updateValue(double value) throws Xcept {
	        this.value = value;
		initRanges();
	    	for (int i=0; i<itemDatas.size(); i++) {
	    	    PNestedDataItem itemData = itemDatas.get(i);
		    String domainID = domainIDs.get(i);
		    itemData.item().setSliderValue(domainID, value);
	    	    itemData.sliderChanged();
	        }
	    }

	    // query
	    public String getName() { return name; }
	    public double getValue() { return value; }
	    public double[] getGrid() { return grid; }
	}    
		
	    
}
