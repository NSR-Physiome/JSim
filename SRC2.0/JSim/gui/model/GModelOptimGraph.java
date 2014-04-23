/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model optimization results graph

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;
import JSim.gui.graph.*;

public class GModelOptimGraph extends GNode 
implements ComponentListener {

	// state
	private JLabel l_hdr;
	private int view;  // which data to view
	private PModelOptimGraph pgraph; // graph parms
	private GBooleanControl g_log; // log/linear control
	private GraphRender render; // graphics

	// actions
	public GAction store, exportFile, print;
	public static final String[] viewNames = {
	    "Parameters", 
	    "Normalized pars", 
	    "RMS error", 
	    "Data to match",
	    "Residuals (unweighted)",
	    "Residuals (weighted)",
	    "Point weights"
	};

	// constructor
	public GModelOptimGraph(GModelOptim g) {
	    super(g, g.gmodel().pmodel());
	    view = PModelOptimGraph.PARAMETERS;
	    pgraph = pgraph();

	    // create widgets
	    JRootPane root = new JRootPane();
	    g_log = new GBooleanControl(this, null) {
	    	public Control cntl() {
		   return pgraph.log;
		}
	    };
	    g_log.addAuxNode(this);
	    render = gappl().newGraphRender(null);
	    JPanel panel = new JPanel(new BorderLayout());
	    Box box = new Box(BoxLayout.X_AXIS);
	    box.add(new JLabel(" Log"));
	    box.add(g_log.jcomp());
	    panel.add(box, BorderLayout.NORTH);
	    panel.add(render.jcomp(), BorderLayout.CENTER);
//	    root.getContentPane().add(render.jcomp());
	    root.setContentPane(panel);
	    setJComp(root);
	    root.addComponentListener(this);

	    // store dataset in project action
	    store = new GAction(this, "Store project dataset ...") {
		public void doit() throws Xcept {
		    Data.List dlist = getData();
		    gproject().pushTree();
		    String msg = "Store optimization data under name";
		    Project proj = gproject().project();
		    String n = proj.newChildName("data", true);
		    String nname = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, "Enter name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().datasetIcon(),
			    null, n);
		    gproject().popTree();
		    if (nname == null) return;
		    PDataSet dset = new PDataSet(proj, nname);
		    dset.importData(dlist);
		    gproject().add(dset);
		    gproject().refresh();
		}
	    };		

	    // export file
	    exportFile = new GAction(this, "Export ...");

	    // print page
	    print = new GAction(this, "Print graph ...") {
		public void doit() throws Xcept {
		    PrinterJob pjob = PrinterJob.getPrinterJob();
		    pjob.setJobName("Optimization Graph");
		    if (! pjob.printDialog()) 
			throw new Xcept("Printing canceled");		    
		    gproject().message("Sending graph to printer...");
		    pjob.setPrintable(render);
		    try {
			pjob.print();
		    } catch (PrinterException e) {
			throw new Xcept(e.toString());
		    }
		}
	    };

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    // File menu
	    menu = newMenu("File");
	    menu.add(store.item());
//	    menu.add(exportFile.item());  // not yet ready
	    menu.add(print.item());
	    mbar.add(menu);

 	    // View menu
	    menu = newMenu("View");
	    ButtonGroup group = new ButtonGroup();
	    for (int i=0; i<viewNames.length; i++) {
		GAction gact = new GViewAction(this, 
		   viewNames[i], i);
	    	JRadioButtonMenuItem b_view = gact.radio();
		if (i==0) b_view.setSelected(true);
	    	menu.add(b_view);
	    	group.add(b_view);
		if (i==2) menu.addSeparator();
	    }
	    mbar.add(menu);

	    // Run button / menu
	    mbar.add(gmodel().optimRun.button("Run"));
	    gmodel().addRunsMenu(mbar);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);
	}	    

	// Component Listener
	public void componentHidden(ComponentEvent e) { }
	public void componentMoved(ComponentEvent e) { }
	public void componentResized(ComponentEvent e) { refresh(); }
	public void componentShown(ComponentEvent e) { }

	// refresh
	public void refresh() {
	    setTabLabel(l_hdr);
	    try {
	    	showData(getData());
	    } catch (Exception e) {
	        String msg = (e instanceof Xcept) ?
		    ((Xcept) e).cleanMessage() : e.toString();
		showError(msg);
	    }
	    super.refresh();
	}

	// get data
	private Data.List getData() throws Xcept {
	    OptimResults results = 
		gmodel().pmodel().rt().optimResults();
	    return getData(results, view, pgraph.log.val());
	}
	
	// get data from OptimResults
	protected static Data.List getData(OptimResults results, int view,
	boolean isLog) throws Xcept {
	    if (results == null) throw new Xcept(
		"ptimzation results not yet available");
	    switch (view) {
	    case PModelOptimGraph.PARAMETERS:
		return results.logX(false);
	    case PModelOptimGraph.NORMALIZED_PARS:
		return results.logX(true);
	    case PModelOptimGraph.RMS_ERROR:
	    	Data.List dlist = new Data.List(1);
		dlist.add(results.logErr());
		return dlist;
	    case PModelOptimGraph.DATA_TO_MATCH: // load data to match and best fit solutions
		if (results.bestCompare == null) throw new Xcept(
		    "Data comparison unavailable");
		dlist = results.bestCompare.data().copy();
		dlist.addAll(results.bestCompare.refs());
		return dlist;
	    case PModelOptimGraph.UNWEIGHTED_RESIDUALS:
		if (results.bestCompare == null) throw new Xcept(
		    "Data comparison unavailable");
		Data.List list = results.bestCompare.residuals(false);
		return (isLog ? abs(list) : list);
	    case PModelOptimGraph.WEIGHTED_RESIDUALS:
		if (results.bestCompare == null) throw new Xcept(
		    "Data comparison unavailable");
		list = results.bestCompare.residuals(true);
		return (isLog ? abs(list) : list);
	    case PModelOptimGraph.POINT_WEIGHTS:
		if (results.bestCompare == null) throw new Xcept(
		    "Data comparison unavailable");
		return results.bestCompare.pointWgts();
	    }
	    throw new Xcept("view not yet supported");
	}

	// return absolute value Data.List
	private static Data.List abs(Data.List list) throws Xcept {
	    Data.List alist = new Data.List();
	    for (int i=0; i<list.size(); i++) {
	    	Data data = list.data(i);
		data = data.copy();
		alist.add(data);
		if (! (data instanceof RealNData)) continue;
		RealNData ndata = (RealNData) data;
		double[] samples = ndata.samples();
		for (int j=0; j<samples.length; j++) 
		    samples[j] = Math.abs(samples[j]);		
	    }
	    return alist;
	}

	// show layout
	public void showLayout(GraphLayout layout) {
	    try {
		render.setGraphLayout(layout, null);
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	}

	// error
	public void showError(String msg) {
	    GraphLayout l = new GraphLayout();
	    l.fg = Color.black;
	    l.bg = Color.white;
	    l.fontSize = glook().fontSize();
	    l.title = "Optimization Results";
	    l.titleX = 0.2;
	    l.errorMsg = msg;
	    l.style = Plot.XY_PLOT;
	    l.xaxis = new GraphLayout.Axis(); // useless
	    l.yaxis = new GraphLayout.Axis(); // useless
	    showLayout(l);
	}

	// show data list
	public void showData(Data.List dlist) throws Xcept {
	    if (dlist == null || dlist.size() < 1 || dlist.allNaN()) 
		throw new Xcept("no data available");

	    // create GraphLayout
	    GraphLayout l = makeGraphLayout(glook(),
	    	view, pgraph.log.val(), dlist, 0);
	    showLayout(l);
	}

	// create GraphLayout from Data.List
	public static GraphLayout makeGraphLayout(GLook glook, 
	int view, boolean isLog, Data.List dlist, int optn) 
	throws Xcept {
	    GraphLayout l = new GraphLayout();
	    l.fg = Color.black;
	    l.bg = Color.white;
	    l.fontSize = glook.fontSize();
	    String soptn = (optn < 1) ? "" : (" #" + optn);
	    l.title = "Optimization" + soptn + " Results: ";
	    if (view < viewNames.length) 
	    	l.title = l.title + viewNames[view];
	    l.showTitle = true;
	    l.titleX = 0.4;
	    l.style = Plot.XY_PLOT;
	    l.showLegend = true;

	    // figure X axis parms
	    l.xaxis = new GraphLayout.Axis();
	    l.xaxis.auto = true;
	    l.xaxis.min = l.xaxis.max = Double.NaN;
	    l.xaxis.label = "x";
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		data.calcExtrema();
		switch (data.ndim()) {
		case 0: 
		    break;
		case 1:
		    GridData xgrid = data.grid(0);
//		    xgrid.calcExtrema();
		    if (Double.isNaN(l.xaxis.min) || xgrid.min() < l.xaxis.min) 
		    	l.xaxis.min = xgrid.min();
		    if (Double.isNaN(l.xaxis.max) || xgrid.max() > l.xaxis.max) 
		    	l.xaxis.max = xgrid.max();
		    l.xaxis.label = xgrid.legend();
		    break;
		default:
		    throw new Xcept(data,
			"2D and large dimensional data not supported");
		}
	    }
	    if (Double.isNaN(l.xaxis.min) || Double.isNaN(l.xaxis.max)) {
	    	l.xaxis.min = 0;
		l.xaxis.max = 1;
	    }

	    // load GraphData array
	    l.yaxis = new GraphLayout.Axis(); 
	    l.yaxis.label = viewNames[view];
	    l.yaxis.min = l.yaxis.max = Double.NaN;
	    l.yaxis.log = isLog;
	    l.data = new GraphData[dlist.size()];
	    for (int i=0; i<dlist.size(); i++) {
		RealNData data = (RealNData) dlist.data(i);
		GraphData gdata = new GraphData();
		gdata.label = data.legend();

		// load x, y data
		double ymin = Double.NaN;
		double ymax = Double.NaN;
		switch (data.ndim()) {
		case 0:
		     gdata.x = new double[] { l.xaxis.min, l.xaxis.max };
		     gdata.y = new double[] { data.realVal(0), data.realVal(0) };
		     ymin = ymax = data.realVal(0);
		     break;
		case 1:
		     GridData xgrid = data.grid(0);
		     gdata.x = xgrid.samples();
		     gdata.y = data.samples();
		     ymin = l.yaxis.log ? data.minpos() : data.min();
		     ymax = data.max();
		     break;
		}

		// expand y range
		if (Double.isNaN(l.yaxis.min) || ymin < l.yaxis.min)
		    l.yaxis.min = ymin;
		if (Double.isNaN(l.yaxis.max) || ymax > l.yaxis.max)
		    l.yaxis.max = ymax;

	 	// set rendering
		gdata.color = glook.plotColor(i);
		gdata.shape = GraphData.SHAPE_DIAMOND;
		gdata.size = 4;
		gdata.line = GraphData.LINE_SOLID;

		// set rendering: Data to Match view pairs colors
		if (view == 3) {
		    int n = dlist.size()/2;
		    if (i>=n) {
			gdata.color = glook.plotColor(i-n);
			gdata.shape = GraphData.SHAPE_SQUARE;
			gdata.line = GraphData.LINE_NONE;
		    } else {
		    	gdata.shape = GraphData.SHAPE_NONE;
		    }
		}			

		l.data[i] = gdata;
	    }
	    return l;
	}
	    
	// get graph controls
	private PModelOptimGraph pgraph() {
	    PModelOptim poptim = gmodel().pmodel().optim();
	    return poptim.graph(view);
	}
	    	
	// view selection Action
	public class GViewAction extends GAction {
	    private int w; // PM
	    public GViewAction(GNode n, String s, int ww) {
		super(n, s);
		w = ww;
	    }
	    public void doit() throws Xcept {
		view = w;
		pgraph = pgraph();
		refresh();
	    }
	}
}


