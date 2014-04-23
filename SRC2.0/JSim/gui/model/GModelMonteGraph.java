/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model Monte Carlo results graph

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
import JSim.gui.gsgraph.*;

public class GModelMonteGraph extends GNode  {
	PModelMonte pmonte;
	private JLabel l_hdr;
	private JPanel panel; // content panel
	private JPanel cpanel; // control panel
	private JPanel graph; // render or hist graphics
	private GSHistogram ghist; // histogram graphics
	private GraphRender render; // plot graphics
	private JLabel jmsg;  // error message 
	private JLabel l_par, l_xpar, l_ypar, 
	    l_optn, l_log, l_bins;  
	private GMenuControl gpar1, gpar2;
	private GControl goptn, gbins;
	private GBooleanControl glog;

	// actions
	public GAction store, exportFile, print;
	public static final String[] viewNames = {
	    "Parameter Histograms",
	    "2-Parameter Scatter Plots",
	    "Optimization #n: Parameters", 
	    "Optimization #n: Normalized pars", 
	    "Optimization #n: RMS error", 
	    "Optimization #n: Data to match",
	    "Optimization #n: Residuals (unweighted)",
	    "Optimization #n: Residuals (weighted)",
	    "Optimization #n: Point weights"
	};

	// constructor
	public GModelMonteGraph(GModelMonte g) {
	    super(g, g.gmodel().pmodel());
	    pmonte = g.gmodel().pmodel().monte();

	    // create widgets
	    JRootPane root = new JRootPane();
	    setJComp(root);
	    panel = new JPanel(new BorderLayout());
	    root.setContentPane(panel);

	    // store dataset in project action
	    store = new GAction(this, "Store in project dataset ...") {
		public void doit() throws Xcept {
		    Data.List dlist = getData();
		    gproject().pushTree();
		    String msg = "Store graph data under name";
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
		    Printable printable = getPrintable();
		    PrinterJob pjob = PrinterJob.getPrinterJob();
		    pjob.setJobName("Monte Carlo Graph");
		    if (! pjob.printDialog()) 
			throw new Xcept("Printing canceled");		    
		    gproject().message("Sending graph to printer...");
		    pjob.setPrintable(printable);
		    try {
			pjob.print();
		    } catch (PrinterException e) {
			throw new Xcept(e.toString());
		    }
		}
	    };

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    root.setJMenuBar(mbar);
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    // File menu
	    menu = newMenu("File");
	    menu.add(store.item());
	    menu.add(print.item());
	    mbar.add(menu);

 	    // View menu
	    menu = newMenu("View");
	    ButtonGroup group = new ButtonGroup();
	    int view = pmonte.graphView.val();
	    for (int i=0; i<viewNames.length; i++) {
		if (i==PModelMonte.GRAPH_OPTIM) menu.addSeparator();
		GAction gact = new GViewAction(this, 
		   viewNames[i], i);
	    	JRadioButtonMenuItem b_view = gact.radio();
		if (i==view) b_view.setSelected(true);
	    	menu.add(b_view);
	    	group.add(b_view);
	    }
	    mbar.add(menu);

	    helpLinks().addHelpMenu(this, mbar);

	    // control panel
	    cpanel = new JPanel(new GridLayout(1,4));
	    panel.add(cpanel, BorderLayout.NORTH);
	    jmsg = new JLabel("MESSAGE HERE");
	    panel.add(jmsg, BorderLayout.SOUTH);
	    l_par = new JLabel("Parameter", SwingConstants.CENTER);
	    l_xpar = new JLabel("X Parameter", SwingConstants.CENTER);
	    l_ypar = new JLabel("Y Parameter", SwingConstants.CENTER);
	    l_optn = new JLabel("Optimization #", SwingConstants.CENTER);
	    l_log = new JLabel("log", SwingConstants.RIGHT);
	    l_bins = new JLabel("# bins", SwingConstants.RIGHT);
	    gpar1 = new GMenuControl(this, pmonte.graphPar1);
	    gpar1.addAuxNode(this);
	    gpar2 = new GMenuControl(this, pmonte.graphPar2);
	    gpar2.addAuxNode(this);
	    goptn = GControl.create(this, pmonte.graphOptNo);
	    goptn.addAuxNode(this);
	    glog = new GBooleanControl(this, pmonte.graphLog);
	    glog.addAuxNode(this);
	    gbins = GControl.create(this, pmonte.graphBins);
	    gbins.addAuxNode(this);

	    // graphics
	    graph = new JPanel(new GridLayout(1,1));
	    panel.add(graph, BorderLayout.CENTER);
	    render = gappl().newGraphRender(null);
	    ghist = new GSHistogram();

	    reconfig();
	}

	// set graphic widget based on project view
	public void reconfig() {
	    int view = pmonte.graphView.val();
	    cpanel.removeAll();
	    graph.removeAll();

	    // get data
	    MoptData moptData = getMoptData();
	    if (moptData == null) {
	    	showError("No Monte Carlo data available");
		return;
	    }

	    // has data,  show config widgets
	    switch (view) {
	    case PModelMonte.GRAPH_HISTOGRAM:
	    	cpanel.add(l_par);
	    	cpanel.add(gpar1.jcomp());
	    	cpanel.add(l_bins);
	    	cpanel.add(gbins.jcomp());
	    	graph.add(ghist);
		break;
	    case PModelMonte.GRAPH_SCATTER:
	    	cpanel.add(l_xpar);
	    	cpanel.add(gpar1.jcomp());
	    	cpanel.add(l_ypar);
	    	cpanel.add(gpar2.jcomp());
	    	graph.add(render.jcomp());    
		break;
	    default:
	    	cpanel.add(l_optn);
	    	cpanel.add(goptn.jcomp());
	    	cpanel.add(l_log);
	    	cpanel.add(glog.jcomp());
	    	graph.add(render.jcomp());
		break;
	    }

	    // par pick lists,  default pars in blank
	    StringList pickList = new StringList(moptData.parNames());
	    gpar1.setPickList(pickList);
	    gpar2.setPickList(pickList);
	    if (pickList.size() > 0) {
	        try {
	            if (pmonte.graphPar1.isBlank())
	    	    	pmonte.graphPar1.setVal(pickList.str(0));		
	            if (pmonte.graphPar2.isBlank()) {
		    	int i = (pickList.size() > 1) ? 1 : 0;
	    	    	pmonte.graphPar2.setVal(pickList.str(i));
		    }
		} catch (Xcept e) {
		    System.err.println("" + e);
		}
	    }
	}

	// refresh
	public void refresh() {
	    int view = pmonte.graphView.val();

	    // get data
	    MoptData moptData = getMoptData();
	    if (moptData == null) {
	    	showError("No Monte Carlo data available");
		return;
	    }
	    if (graph.getComponentCount() == 0) reconfig();
	    
	    // show appropriate controls and graph
	    try {
	    	switch (view) {
	    	case PModelMonte.GRAPH_HISTOGRAM:
		    refreshHistogram(moptData);
		    break;
	    	case PModelMonte.GRAPH_SCATTER:
		    refreshScatter(moptData);
		    break;
	    	default:
		    refreshOptim(moptData);
		    break;
		}
	    } catch (Exception e) {
		System.err.println("" + e);
		showError(e.toString());		
	    }

	    panel.repaint();
	    super.refresh();
	}

	// get moptData or null
	private MoptData getMoptData() {
	    try {
	        return gmodel().pmodel().rt().getMoptData();
	    } catch (Xcept e) {
		return null;
	    }
	}	

	// get data to graph
	private Data.List getData() throws Xcept {
	    MoptData moptData = getMoptData();
	    if (moptData == null) throw new Xcept(
	    	"Graph data not available");
	    Data.List dlist = new Data.List(2);
	    switch (pmonte.graphView.val()) {
	    case PModelMonte.GRAPH_HISTOGRAM:
	    	String pname = pmonte.graphPar1.stringVal();
	    	if (Util.isBlank(pname)) throw new Xcept(
	    	    "No parameter selected");
		dlist.add( moptData.parData(pname));
		break;
	    case PModelMonte.GRAPH_SCATTER:
	    	if (pmonte.graphPar1.isBlank()
	    	|| pmonte.graphPar2.isBlank()) throw new Xcept(
	    	    "No parameters selected");
	        dlist.add(moptData.parData(pmonte.graphPar1.stringVal()));
	        dlist.add(moptData.parData(pmonte.graphPar2.stringVal()));
		break;
	    default:
	    	int optn = pmonte.graphOptNo.val() - 1;
	    	OptimResults res = moptData.optimResults(optn);
	    	if (res == null) throw new Xcept("Optimization results #" + 
	    	    (optn+1) + " not available");
	        int view = pmonte.graphView.val() - PModelMonte.GRAPH_OPTIM;
	        dlist = GModelOptimGraph.getData(res, view, pmonte.graphLog.val());
		break;
		}
	    return dlist;
	}

	// refresh histogram
	private void refreshHistogram(MoptData moptData) throws Xcept {
	    jmsg.setText("");
	    Data.List dlist = getData();
	    RealNData pdata = (RealNData) dlist.data(0);
	    pdata.calcExtrema();
	    String pname = pmonte.graphPar1.stringVal();

	    HistLayout l = new HistLayout();
	    l.title = "Parameter Histogram: " + pname;
	    l.nbins = pmonte.graphBins.val();
	    l.samples = pdata.samples();
	    l.sampleMin = pdata.min();
	    l.sampleMax = pdata.max();
	    l.barColor = glook().bg();
	    l.barSpacing = 1;

	    ghist.setLayout(l);
	}
	    	
	// refresh scatter plot
	private void refreshScatter(MoptData moptData) throws Xcept {
	    jmsg.setText("");
	    Data.List dlist = getData();
	    RealNData xdata = (RealNData) dlist.data(0);
	    RealNData ydata = (RealNData) dlist.data(1);
	    xdata.calcExtrema();
	    ydata.calcExtrema();
	    
	    // create scatter layout
	    GraphLayout l = new GraphLayout();
	    l.fg = Color.black;
	    l.bg = Color.white;
	    l.fontSize = glook().fontSize();
	    l.title = "Monte Carlo 2-Parameter Scatter";
	    l.titleX = 0.5;
	    l.showTitle = true;
	    l.style = Plot.XY_PLOT;

	    // axes
	    l.xaxis = new GraphLayout.Axis(); // useless
	    l.xaxis.min = xdata.min();
	    l.xaxis.max = xdata.max();
	    l.xaxis.log = false;
	    l.xaxis.label = xdata.legend();
	    l.xaxis.showLabel = true;
	    l.yaxis = new GraphLayout.Axis(); // useless
	    l.yaxis.min = ydata.min();
	    l.yaxis.max = ydata.max();
	    l.yaxis.log = false;
	    l.yaxis.label = ydata.legend();
	    l.yaxis.showLabel = true;

	    // scatter data
	    l.data = new GraphData[1];
	    GraphData gdata = new GraphData();
	    l.data[0] = gdata;
	    gdata.label = ydata.legend();
	    gdata.x = xdata.samples();
	    gdata.y = ydata.samples();
	    gdata.color = Color.black;
	    gdata.shape = GraphData.SHAPE_DIAMOND;
	    gdata.size = 4;
	    gdata.line = GraphData.LINE_NONE;
	    showLayout(l);	    
	}
	    	
	// refresh optimization plot
	private void refreshOptim(MoptData moptData) throws Xcept {
	    jmsg.setText("");
	    int optn = pmonte.graphOptNo.val() - 1;
	    int view = pmonte.graphView.val() - PModelMonte.GRAPH_OPTIM;
	    Data.List dlist = getData();
	    GraphLayout layout = GModelOptimGraph.makeGraphLayout(glook(),
	    	view, pmonte.graphLog.val(), dlist, optn+1);
	    showLayout(layout);
	}
	    	
	// error
	public void showError(String msg) {
	    jmsg.setText(msg);
	    panel.repaint();
	}

	// show layout
	public void showLayout(GraphLayout layout) {
	    try {
		render.setGraphLayout(layout, null);
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	}

	// get Printable object
	public Printable getPrintable() throws Xcept {
	    if (graph.getComponentCount() != 1) throw new Xcept(
	    	"Monte Carlo graph is not currently printable");
	    Printable p = (pmonte.graphView.val() == PModelMonte.GRAPH_HISTOGRAM) ?
	    	ghist : render;
	    return (Printable) p;
	}

	// view selection Action
	public class GViewAction extends GAction {
	    private int w; // which view
	    public GViewAction(GNode n, String s, int ww) {
		super(n, s);
		w = ww;
	    }
	    public void doit() throws Xcept {
		pmonte.graphView.setVal(w);
		reconfig();
		refresh();
	    }
	}
}


