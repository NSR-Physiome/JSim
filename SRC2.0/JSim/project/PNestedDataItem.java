/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Nested graph data for one PNestedItem

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import JSim.expr.*;
import JSim.aserver.*;
import java.util.*;

public class PNestedDataItem {
	private PNestedData parent;  // parent
	private PNestedItem item;    // for this item
	private double[] outerGrid, innerGrid;  // loop grids
	private GridData outerGridData, innerGridData; // if length>1
	private Data[] baseData;  // unprocessed item data
	private int baseDataDim;  // ndim for any baseData[i]
	private boolean hasData;  // any data available?
	private StringList domains;  // active domains of variance
	private StringList sliderDomains; // domains controlled by sliders
	private Hashtable<String, String> axisDomains, domainAxes; // domain<->axis maps
	private StringList sparseDomains; // domain ordering in sparse space
	private int[] sparseInxs; // sparse domain indexes (+/-)
	private Hashtable<String, double[]> sparseDomainGrids; // grids for sparse domains
	private int sparseNdim; // # sparse dimensions
	private int sparseNpts; // # points in sparse space
	private double[][] sparseGrids; // grids in sparse sparse order
	private Hashtable<String, double[]> sliderGrids; // grids for sliders
	private StringList denseDomains; // domains to sample for each curve
	private int denseNdim;  // # dense domains
	private int[] denseInxs; // indexes of dense domains (+/-)
	private Data[] graphData; // indexed in sparce space
	private Attr[] graphAttrs; // attrs for data in sparce space

	// constants
	public static final double[] GRID1 = new double[] { 1 };
	public static final String NO_VARIATION = PNestedDomainControl.NO_VARIATION;
	public static final String INNER_LOOP = PNestedDomainControl.INNER_LOOP;
	public static final String OUTER_LOOP = PNestedDomainControl.OUTER_LOOP;
	public static final int INNER_LOOP_INX = -1;
	public static final int OUTER_LOOP_INX = -2;
	public static final int LOOPS_DIM = 2; // inner & outer, for now

	// constructor
	public PNestedDataItem(PNestedData parent, PNestedItem item) 
	throws Xcept {
	    this.parent = parent;
	    this.item = item;
	    loadBaseData();
	    if (! hasData) return;
	    makeDomainMaps();
	    makeGraphData();
	}

	// slider value was changed,  reextract data curves
	protected void sliderChanged() throws Xcept {
	    for (int i=0; i<sliderDomains.size(); i++) {
	    	String domainID = sliderDomains.get(i);
		double v = item.getSliderValue(domainID);
		if (Double.isNaN(v)) continue;
		double[] grid = new double[] { v };
		sparseDomainGrids.put(domainID, grid);
	    }
	    sparseNpts = 1;
	    for (int i=0; i<sparseNdim; i++) {
	    	String domainID = sparseDomains.get(i);
	        sparseGrids[i] = sparseDomainGrids.get(domainID);
		sparseNpts *= sparseGrids[i].length;
	    }
	    makeGraphData(); // sure, why not?
	}

	////  -------------- LOAD BASE DATA ----------------
	
	// load baseData
	private void loadBaseData() throws Xcept {
	    // loop config
	    innerGrid = outerGrid = GRID1;
	    if (item.isModel() && 
	    item.pmodel().rt().storeMode() == ASModel.LOOPS) {
		ASInfo.Loops info = (ASInfo.Loops) 
		    item.pmodel().rt().getJobInfo();
		outerGrid = makeGrid(info.counts[0]);
		innerGrid = makeGrid(info.counts[1]);
		if (nouter() > 1)
		    outerGridData = new RegularGridData("outerLoop", null,
		    	1, nouter(), nouter());
		if (ninner() > 1)
		    innerGridData = new RegularGridData("innerLoop", null,
		    	1, ninner(), ninner());
	    }

	    // load data
	    baseData = new Data[nloops()];
	    for (int i=0; i<baseData.length; i++) { 
	    	Data data = item.getData(i);
		if (data == null) continue;
	    	baseData[i] = data;
		hasData = true;
	    }
	}

	// make loop grid
	public static double[] makeGrid(int n) {
	    if (n == 1) return GRID1; 
	    double[] grid = new double[n];
	    for (int i=0; i<n; i++) { grid[i] = i+1; }
	    return grid;
	}

	//// ---------------- CREATE DOMAIN/AXIS MAPS --------------

	// create domain/axis maps
	private void makeDomainMaps() throws Xcept {

	    // active domains
	    domains = new StringList(item.getDomainNames());
	    if (nouter() > 1) domains.add(OUTER_LOOP);
	    if (ninner() > 1) domains.add(INNER_LOOP);

	    // domain/axis maps
	    sliderDomains = new StringList(domains);
	    sliderGrids = new Hashtable<String, double[]>();
	    domainAxes = new Hashtable<String, String>();
	    axisDomains = new Hashtable<String, String>();
	    sparseDomains = new StringList();
	    denseDomains = new StringList();
	    sparseDomainGrids = new Hashtable<String, double[]>();
	    for (int i=0; i<axisIDs().size(); i++) {
	    	String axisID = axisIDs().get(i);
	    	PNestedAxis.Local laxis = laxis(axisID);
		if (laxis.isDependent()) continue;
	    	if (! laxis.domain.valid) throw new Xcept(
	    	    laxis.domain.validMsg());
	    	String domainID = laxis.domain.val();
	    	if (domainID.equals(NO_VARIATION)) continue;
		if (domainAxes.get(domainID) != null) throw new Xcept(
		    "Duplicate axis assignment for domain " + domainID);
		axisDomains.put(axisID, domainID);
		domainAxes.put(domainID, axisID);
		sliderDomains.remove(domainID);
		if (laxis.isDense()) {
		    denseDomains.add(domainID);
		    continue;
		}
		double[] grid = getSparseGrid(laxis);
		sparseDomains.add(domainID);
		sparseDomainGrids.put(domainID, grid);
	    }

	    // add domainGrids for sliders
	    for (int i=0; i<sliderDomains.size(); i++) {
		String domainID = sliderDomains.get(i);
		int inx = makeDomainInx(domainID);
		double[] dgrid = getCombinedGrid(inx);
		sliderGrids.put(domainID, dgrid);
		double v = item.getSliderValue(domainID);
		v = Util.bound(v, dgrid);
		double[] grid = new double[] { v };
		sparseDomains.add(domainID);
		sparseDomainGrids.put(domainID, grid);
	    }
	    
	    // initialize sparse & dense space, ncurves
	    sparseInxs = makeDomainInxs(sparseDomains);
	    denseInxs = makeDomainInxs(denseDomains);
	    sparseNdim = sparseDomains.size();
	    denseNdim = denseDomains.size();
	    sparseGrids = new double[sparseNdim][];
	    sparseNpts = 1;
	    for (int i=0; i<sparseNdim; i++) {
	    	String domainID = sparseDomains.get(i);
	        sparseGrids[i] = sparseDomainGrids.get(domainID);
		sparseNpts *= sparseGrids[i].length;
	    }
	    baseDataDim = baseData[0].ndim();

Util.verbose("PNestedDataItem " + item.expr.val());
Util.verbose("  baseDataDim=" + baseDataDim);
Util.verbose("  domainAxes=" + domainAxes);
Util.verbose("  axisDomains=" + axisDomains);
Util.verbose("  sparseDomains=" + sparseDomains + 
  " inxs=" + Util.pretty(sparseInxs));
for (int i=0; i<sparseNdim; i++) 
  Util.verbose("    Domain " + sparseDomains.get(i) + " grid=" +
  Util.pretty(sparseGrids[i]));
Util.verbose("  denseDomains=" + denseDomains + 
  " inxs=" + Util.pretty(denseInxs));
	}

	// make array of domain indexes
	private int[] makeDomainInxs(StringList ids) throws Xcept {
	    int[] inxs = new int[ids.size()];
	    for (int i=0; i<inxs.length; i++)
		inxs[i] = makeDomainInx(ids.str(i));
	    return inxs;
	}

	// get domain index matching domainID
	private int makeDomainInx(String id) throws Xcept {
	    if (id.equals(INNER_LOOP)) return INNER_LOOP_INX;
	    if (id.equals(OUTER_LOOP)) return OUTER_LOOP_INX;
	    Data data = baseData[0];
	    if (id.startsWith("grid "))
	    	return Util.toInt(id.substring(5));
	    for (int i=0; i<data.ndim(); i++) {
	    	GridData grid = data.grid(i);
		if (grid.desc() != null && grid.desc().equals(id)) 
		    return i;
		if (grid.name() != null && grid.name().equals(id)) 
		    return i;
	    }
	    throw new Xcept("makeDomainInx(): no such grid <" + id + 
	    	"> in " + data.legend());
	}

	// get sparse axis domain grid
	private double[] getSparseGrid(PNestedAxis.Local laxis) throws Xcept {
	    String domainID = laxis.domain.val();
	    int ginx = makeDomainInx(domainID);
	
	    Data data0 = baseData[0];
	    if (data0 == null) throw new Xcept(
	    	"No data for getGridValues");

	    // calculate internal grid
	    double[] grid = getCombinedGrid(ginx);
	    if (laxis.isDense()) return grid; // ??? not used ???
	    
	    // sparse grid sampling
	    int ct = laxis.maxSparseSamples();
	    if (grid.length < ct) ct = grid.length;
	    double dmin = grid[0];
	    double dmax = grid[grid.length-1];
	    if (! laxis.autoscale.val()) {
	    	dmin = laxis.dataMin.val();
		dmax = laxis.dataMax.val();
	    }
	    if (dmin > dmax) throw new Xcept(
	    	"domain min>max" + domainID);
	    switch(laxis.sampleMethod.val()) {
	    case PNestedAxis.Local.LINEAR:
	    	return makeLinearSamples(dmin, dmax, ct);
	    case PNestedAxis.Local.LOG:
	    	return makeLogSamples(dmin, dmax, ct);
	    case PNestedAxis.Local.LIST:
	    	return makeListSamples(laxis.sampleList.val(), ct);
	    default: throw new Xcept(
	    	"Local Axis sampleMethod unknown");  	    
	    }
	}
		    	
	// linear samples
	private double[] makeLinearSamples(double dmin, double dmax,
	int ct) {
	    double[] grid = new double[ct];
	    grid[0] = dmin;
	    for (int i=1; i<ct; i++) 
	    	grid[i] = dmin + i*(dmax-dmin)/(ct-1);
	    return grid;
	}
	
	// log samples
	private double[] makeLogSamples(double dmin, double dmax, 
	int ct) throws Xcept {
	    if (dmin == 0) dmin = dmax * 1e-4;
	    if (dmin < 0) throw new Xcept(
	    	"Log samples require non-negative data grid");
	    double[] linGrid = makeLinearSamples(Math.log(dmin),
	    	Math.log(dmax), ct);
	    double[] grid = new double[ct];
	    for (int i=0; i<ct; i++) 
	    	grid[i] = Math.exp(linGrid[i]);
	    return grid;
	}
	
	// list samples
	private double[] makeListSamples(String list, int ct) 
	throws Xcept {
	    StringTokenizer stok = new StringTokenizer(list, ",");
	    int n = stok.countTokens();
	    if (n < 1) throw new Xcept("Empty samples list");
	    if (ct < n) n = ct;
	    double[] grid = new double[n];
	    for (int i=0; i<n; i++) {
	    	String tok = stok.nextToken();
		grid[i] = Util.toDouble(tok);
		if (i>0 && grid[i] < grid[i-1]) throw new Xcept(
		    "Samples list out of numeric order");
	    }
	    return grid;
	}

	// combine all loop grids for slider grid
	private double[] getCombinedGrid(int inx) throws Xcept {
	    if (inx == INNER_LOOP_INX) return innerGrid;
	    if (inx == OUTER_LOOP_INX) return outerGrid;
	    GridData grid =  baseData[0].grid(inx);
	    for (int i=0; i<baseData.length; i++) {
	        if (baseData[i] == null) continue;
		grid = grid.combine(baseData[i].grid(inx));
	    }
	    return grid.samples();
	}


	////----------------  GENERATE GRAPH DATA -----------------------

	// create graphData
	private void makeGraphData() throws Xcept {
	    graphData = new Data[sparseNpts];
	    graphAttrs = new Attr[sparseNpts];
	    int[] inxs = new int[sparseNdim];
	    double[] gvals = new double[baseDataDim];
	    int[] lvals = new int[LOOPS_DIM];
	    if (parent.scaling() == PNested.GLOBAL)
	    	updateGlobalRanges();
	    for (int i=0; i<sparseNpts; i++) {
	    	loadQueryVals(i, sparseGrids, sparseInxs, inxs, gvals, lvals);
		String label = makeDataLabel(inxs);
		graphAttrs[i] = new Attr(inxs);
		graphData[i] = makeData(label, gvals, lvals);
		if (parent.scaling() == PNested.LOCAL)
		     updateGraphRanges(graphData[i], graphAttrs[i]);
		else 
		     updatePageRanges(graphData[i]);
	    }		
	}

	// create data label
	private String makeDataLabel(int[] inxs) throws Xcept {
	    StringBuffer buf = new StringBuffer();
	    for (int i=0; i<sparseNdim; i++) {
	    	double val = sparseGrids[i][inxs[i]];
	    	if (i > 0) buf.append(" ");
		buf.append(sparseDomains.get(i) + "=" 
		    + Util.pretty(val));
	    }
	    return buf.toString();
	}

	// load query vals from nth sample
	// n = sample number in grids space
	// domInxs[] = domainInx for each grid
	// inxs[] = decoded coords in grids space (optional)
	// gvals[] = data grid value query (length baseDataDim)
	// lvals[] = loops indexes (length LOOPS_DIM)
	private void loadQueryVals(int n, double[][] grids, int[] domInxs,
	int[] inxs, double[] gvals, int[] lvals) {
	    for (int i=grids.length-1; i>=0; i--) {
		int ct = grids[i].length;
	    	int inx = n % ct;
		if (inxs != null) inxs[i] = inx;
		double gval = grids[i][inx];
		int domInx = domInxs[i];
	    	if (domInx>=0) 
	    	   gvals[domInx] = gval;
	    	else 
	    	    lvals[domInx+LOOPS_DIM] = (int) gval - 1;
		n /= ct;
	    }
	}

	// create data curve with sparse values
	private Data makeData(String label, double[] gvals, int[] lvals) 
	throws Xcept {
	    Util.verbose("  make data: " + label);

	    // make samples
	    int nsamples = 1;
	    double[][] denseGrids = new double[denseNdim][];
	    ArrayList<GridData> gridList = new ArrayList<GridData>();
	    int loopInx = loopInx(lvals);
	    for (int i=0; i<denseNdim; i++) {
	    	denseGrids[i] = getDenseGridSamples(denseInxs[i], loopInx);
	    	GridData grid = getDenseGridData(denseInxs[i], loopInx);
		if (grid != null) gridList.add(grid);
		nsamples *= denseGrids[i].length;
	    }
	    double[] samples = new double[nsamples];
	    for (int i=0; i<nsamples; i++) {
		loadQueryVals(i, denseGrids, denseInxs, null, gvals, lvals);
		int j = (denseNdim == 2) ? inxHack(denseGrids, i) : i;		    
		samples[j] = getSample(gvals, lvals);
	    }

	    // create RealNData
	    String desc = baseData[0].desc();
	    Unit unit = baseData[0].unit();
	    GridData[] grids = new GridData[gridList.size()];
	    for (int i=0; i<gridList.size(); i++) 
	    	grids[i] = gridList.get(i);
	    RealNData data = new RealNData(desc, unit, grids, samples);
	    data.setGroup(label);
	    return data;
	}

	// hack 2D data index: Data package uses different inx encoding
	private int inxHack(double[][] grids, int i) {
	    int n = grids[0].length;
	    int m = grids[1].length;
	    int x = i % m;
	    int y = i / m;
	    int j = x*n + y;
	    return j;
	}

	// get dense grid, always non-null, perhaps of length 1
	private double[] getDenseGridSamples(int inx, int loopInx) throws Xcept {
	    switch (inx) {
	    case OUTER_LOOP_INX:
	    	return outerGrid;
	    case INNER_LOOP_INX:
	    	return innerGrid;
	    default:
	        Data bdata = baseData[loopInx];
		if (bdata == null)
		    bdata = baseData[0];
	    	return bdata.grid(inx).samples();
	    }
	}

	// get dense grid data,  may be null if GridSamples of length 1
	private GridData getDenseGridData(int inx, int loopInx) throws Xcept {
	    switch (inx) {
	    case OUTER_LOOP_INX:
	    	return outerGridData;
	    case INNER_LOOP_INX:
	    	return innerGridData;
	    default:
	        Data bdata = baseData[loopInx];
		if (bdata == null)
		    bdata = baseData[0];
	    	return bdata.grid(inx);
	    }
	}

	// get sample
	private double getSample(double[] gvals, int[] lvals) throws Xcept {
	    int l = loopInx(lvals);
	    if (l < 0 || l >= baseData.length) return Double.NaN;
	    Data data = baseData[l];
	    if (data == null) return Double.NaN;
	    return data.realVal(gvals);
	}

	//// ----------------- RANGE UPDATES ---------------------------

	// update global ranges from baseData
	private void updateGlobalRanges() throws Xcept {
	    DataRange[] ranges = parent.getCommonRanges();
	    int depInx = parent.nInnerAxes()-1;
	    for (int loopInx=0; loopInx<baseData.length; loopInx++) {
	    	Data data = baseData[loopInx];
		if (data == null) continue;
		data.calcExtrema();
	    	ranges[depInx].expand(data);
		for (int i=0; i<denseNdim; i++) {
	    	    GridData grid = getDenseGridData(denseInxs[i], loopInx);
		    ranges[i].expand(grid);
		}
	    }
	}
		
	// update page ranges from graphData
	private void updatePageRanges(Data data) throws Xcept {
	    data.calcExtrema();
	    DataRange[] ranges = parent.getCommonRanges();
	    int depInx = parent.nInnerAxes()-1;
	    ranges[depInx].expand(data);
	    for (int i=0; i<data.ndim(); i++) {
		Data idata = data.grid(i);
		if (data instanceof PlotData) 
		    idata = ((PlotData) data).data(i);
		ranges[i].expand(idata);
	    }
	}

	// update graph ranges from graphData
	private void updateGraphRanges(Data data, Attr attr) throws Xcept {
	    data.calcExtrema();
	    int depInx = parent.nInnerAxes()-1;
	    for (int n=0; n<nxy(); n++) {
	    	if (attr.x2inx >= 0 && attr.x2inx != xinx(n)) continue;
	    	if (attr.y2inx >= 0 && attr.y2inx != yinx(n)) continue;
	    	DataRange[] ranges = parent.getGraphRanges(n);
		ranges[depInx].expand(data);
		for (int i=0; i<data.ndim(); i++) {
		    Data idata = data.grid(i);
		    if (data instanceof PlotData) 
			idata = ((PlotData) data).data(i);
		    ranges[i].expand(idata);
		}

//System.err.println("graph #" + n + " ranges: x1=" + ranges[0] + " y1=" + ranges[1]);
//System.err.println("  x1=" + Util.pretty(data.data(0).samples()));
//if (naxes>1) System.err.println("  y1=" + Util.pretty(data.data(1).samples()));
	    }
	}

	// simple public query
	public PNestedItem item() { return item; }
	public boolean hasData() { return hasData; }
	public int nx() { return parent.nx(); }
	public int ny() { return parent.ny(); }
	public int nxy() { return parent.nxy(); }
	public int xinx(int n) { return parent.xinx(n); }
	public int yinx(int n) { return parent.yinx(n); }
	public Data getGraphData(int i) { return graphData[i]; }
	public Attr getGraphAttr(int i) { return graphAttrs[i]; }
	public StringList getSliderDomains() { return sliderDomains; }
	public double[] getSliderGrid(String domainID) {
	    return sliderGrids.get(domainID);
	}
	protected String getAxisDomain(String axisID) {
	    return axisDomains.get(axisID);
	}
	public double getAxisDomainValue(String axisID, int inx) {
	    String domainID = axisDomains.get(axisID);
	    if (domainID == null) return Double.NaN;
	    double[] grid = sparseDomainGrids.get(domainID);
	    if (grid == null || inx<0 || inx >= grid.length)
	    	return Double.NaN;
	    return grid[inx];
	}

	// simple private query
	private PNested pnested() { return parent.pnested(); }
	private int ninner() { return innerGrid.length; }
	private int nouter() { return outerGrid.length; }
	private int nloops() { return innerGrid.length * outerGrid.length; }
	private int loopInx(int[] lvals) { return ninner()*lvals[0] + lvals[1]; }
	private StringList axisIDs() { return parent.axisIDs(); }
	public int nGraphData() { return (graphData == null) ? 0 : graphData.length; }
	private PNestedAxis.Local laxis(String id) { return item.localAxis(id); }
	private PNestedAxis.Local lX1() { return laxis(PNestedAxis.X1); }
	private PNestedAxis.Local lX2() { return laxis(PNestedAxis.X2); }
	private PNestedAxis.Local lY1() { return laxis(PNestedAxis.Y1); }
	private PNestedAxis.Local lY2() { return laxis(PNestedAxis.Y2); }
	private PNestedAxis.Local lZ() { return laxis(PNestedAxis.Z); }

	// Data attributes
	public class Attr {
	    public int x2inx; // -1 if for all
	    public int y2inx; // -1 if for all
	    public int color;
	    public int size;
	    public int shape;
	    public int line;
	    public int thickness;
	    public int colorMap;
	    public int palette;
	    
	    // constructor
	    public Attr(int[] inxs) {
	    	x2inx = calcInx(PNestedAxis.X2, inxs);
	    	y2inx = calcInx(PNestedAxis.Y2, inxs);
		color = calcInx(item.color, PNestedAxis.COLOR, inxs, false);
		size = calcInx(item.size, PNestedAxis.SIZE, inxs, false);
		shape = calcInx(item.shape, PNestedAxis.SHAPE, inxs, false);
		line = calcInx(item.line, PNestedAxis.LINE, inxs, true);
		thickness = calcInx(item.thickness, PNestedAxis.THICKNESS, inxs, false);
		colorMap = item.colorMap.val();
		palette = item.palette.val();		    
	    }		

	    // calc X2/Y2 index
	    private int calcInx(String axisID, int[] inxs) {
	        String domainID = axisDomains.get(axisID);
		if (domainID == null) return -1;
		int dinx = sparseDomains.indexOf(domainID);
		return inxs[dinx];
	    }
	    
	    // cal Attr index
	    private int calcInx(ChoiceControl c, String axisID, int[] inxs,
	    boolean avoidZero) {
		int retInx = c.val();
	        String domainID = axisDomains.get(axisID);
		if (domainID == null) return retInx;
		int dinx = sparseDomains.indexOf(domainID);
		retInx += inxs[dinx];
		int ct = c.nLabels();
		if (avoidZero) ct--;
		retInx %= ct; // overflow protection
		if (avoidZero) retInx++;
		return retInx;
	    }

	    // query
	    public String toString() {
	    	return "x2inx=" + x2inx + " y2inx=" + y2inx + 
		    " color=" + color + " line=" + line
		    + " size=" + size + " shape=" + shape 
		    + " thick=" + thickness;
 	    }
	}
}

