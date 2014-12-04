/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// plot page

package JSim.gui;

import java.io.File;
import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;
import org.w3c.dom.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.graph.*;

public class GPlotPage extends GNode {

	// state
	private PlotPage page;
	private JRootPane root;
	private GTabs tabs;
	public GPlotConf conf;
	public GPlotGraph graph;
	public GPlotText text;
	private JPanel content;
	private JLabel l_hdr;

	// actions, menus
	public GAction clonePlot, removePlot, importPlot,
	    exportData, store, printGraph, printText, exportEPS,
	    newPrintGraph,
	    zoomIn, zoomOut, zoomAuto;
	public GShowAction a_show[];
	public JRadioButtonMenuItem b_rows[], b_cols[], b_show[];

	// constants for show[] arrays (in menu appearance order)
	private static final int CONF = 0;
	private static final int XRULE = 1;
	private static final int YRULE = 2;
	private static final int MINMAX = 3;
	private static final int TITLE = 4;
	private static final int LEGEND = 5;
	private static final int XAXIS = 6;
	private static final int YAXIS = 7;
	private static final int FOOTER = 8;
	private static final int NSHOW = 9;  // array size

	// constructor from PNotes
	public GPlotPage(GNode p, PlotPage pg) throws Xcept {
	    super(p, pg);
	    page = pg;

	    // create widgets
	    root = new JRootPane();
	    content = new JPanel(null) {
		public void doLayout() { reconfig(); }
	    };
	    root.setContentPane(content);
	    setJComp(root);
	    makeContent();	// why needed ???
	    gproject().addTab(this);
	}

	// make content
	public void makeContent() {
	    super.makeContent();
	    tabs = new GTabs(this, page);
	    tabs.setTabPlacement(SwingConstants.BOTTOM);
	    graph = new GPlotGraph(tabs, page);
	    tabs.addTab("Graph", graph);
	    text = new GPlotText(tabs, page);
	    tabs.addTab("Text", text);
	    conf = new GPlotConf(this, page); // after graph create, for icons
	    JComponent jconf = conf.jcomp();
	    jconf.setSize(jconf.getPreferredSize());
	    tabs.jcomp().setLocation(0, jconf.getHeight());
	    content.add(conf.jcomp());
	    content.add(tabs.jcomp());

	    // create actions
	    clonePlot = new GAction(this, "Clone this page ...") {
		public void doit() throws Xcept {
		    gproject().cloneNode(gnode);
		}
	    };
	    removePlot = new GAction(this, "Remove this page ...") {
		public void doit() throws Xcept {
		    gproject().removeNode(gnode);
		}
	    };
	    exportData = new GAction(this, "Export data file ...") {
		public void doit() throws Xcept {
		    Data.List data = text.exportData();
		    if (data.size() == 0) throw new Xcept(
			"no data available to export");
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.DATASET);
		    if (info == null) return;
		    DataWriter wrt = info.dataFormat.createWriter();
		    wrt.setPrecision(info.doublePrecision ?
			Util.DOUBLE_PRECISION : Util.SINGLE_PRECISION);
		    wrt.writeFile(info.file, data);
		    gproject().message("Exported data file " + 
			UtilIO.prettyPath(info.file) + " in " + 
			info.dataFormat.shortName() + " format.");
		    if (wrt.warning() != null) 
			gproject().warning("Warning: " + wrt.warning());
		}
	    };

	    // import plot
	    importPlot = new GAction(this, "Import plot ...") {
	    	public void doit() throws Xcept {
		    ImportablePlot cplot = new ImportablePlot(currPlot());
		    ImportablePlot[] iplots = getImportablePlots();
		    if (iplots.length == 0) throw new Xcept(
		    	"No other project plots to import from");
		    ImportablePlot iplot = (ImportablePlot) JOptionPane.showInputDialog(
			    jcomp(), 
			    "Import " + cplot + " configuration from:", 
			    "Import plot", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().plotpageIcon(),
			    iplots, null);
		    if (iplot == null) return;
		    Document doc = UtilXML.createDoc("plotxfer");
		    Element elem = iplot.plot.exportXML(doc);
		    currPlot().importXML(elem);
		    page.revalidate();
		    refresh();
		};
	    };

	    // print page graphics
	    printGraph = new GAction(this, "Print graph(s) ...") {
		public void doit() throws Xcept {
		    PrinterJob pjob = PrinterJob.getPrinterJob();
		    pjob.setJobName(page.name());
		    if (! pjob.printDialog()) 
			throw new Xcept("Printing canceled");
		    gproject().message("Sending graph to printer...");
		    graph.print(pjob);
		}
	    };

	    // print page graphics
	    printGraph = new GAction(this, "Print graph(s) ...") {
		public void doit() throws Xcept {
		    PrinterJob pjob = PrinterJob.getPrinterJob();
		    pjob.setJobName(page.name());
		    if (! pjob.printDialog()) 
			throw new Xcept("Printing canceled");
		    gproject().message("Sending graph to printer...");
		    graph.print(pjob);
		}
	    };

	    // new print page
	    newPrintGraph = new GAction(this, "New Print graph ...") {
		public void doit() throws Xcept {
		    Printable printable = getPrintable();
		    PrinterJob pjob = PrinterJob.getPrinterJob();
		    pjob.setJobName("Plot page print");
		    if (! pjob.printDialog()) 
			throw new Xcept("Printing canceled");		    
		    gproject().message("Sending plot page to printer...");
		    pjob.setPrintable(printable);
		    try {
			pjob.print();
		    } catch (PrinterException e) {
			throw new Xcept(e.toString());
		    }
		}
	    };

	    // print page text
	    printText = new GAction(this, "Print text ...") {
		public void doit() throws Xcept {
		    String title = gproject().title() + " Plot Text";
		    String content = text.pageText();
		    gnode.printText(title, content);
		}
	    };

	    // export EPS graphics
	    exportEPS = new GAction(this, "Export EPS graphics ...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.EPS);
		    if (info == null) return;
		    File f = info.file;
		    graph.exportEPS(f);
		}
	    };

	    // store dataset in project action
	    store = new GAction(this, "Store project dataset ...") {
		public void doit() throws Xcept {
		    Data.List dlist = new Data.List(4);
		    page.addData(dlist);
		    if (dlist.size() == 0) {
			gnode.warning("No data in plot");
		        return;
		    }
		    gproject().pushTree();
		    String msg = "Store plot data under name";
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

	    // show actions
	    a_show = new GShowAction[NSHOW];
	    for (int i=0; i<NSHOW; i++) 
		a_show[i] = new GShowAction(this, i);

	    // zoom functions
	    zoomAuto = new GAction(this, "Autoscale all axes") {
		public void doit() throws Xcept {
		    Plot plot = ((GPlotPage) gnode).page.confPlot();
		    plot.zoomStack().zoomAuto();
		    gnode.refresh();
		}
		public boolean sbEnabled() {
		    Plot plot = ((GPlotPage) gnode).page.confPlot();
		    return plot.zoomStack().canZoomAuto();
		}
	    };
	    zoomOut = new GAction(this, "Zoom out") {
		public void doit() throws Xcept {
		    Plot plot = ((GPlotPage) gnode).page.confPlot();
		    plot.zoomStack().zoomOut();
		    gnode.refresh();
		}
		public boolean sbEnabled() {
		    Plot plot = ((GPlotPage) gnode).page.confPlot();
		    return plot.zoomStack().canZoomOut();
		}
	    };
	    zoomIn = new GAction(this, "Zoom in") {
		public void doit() throws Xcept {
		    Plot plot = ((GPlotPage) gnode).page.confPlot();
		    plot.zoomStack().zoomIn();
		    gnode.refresh();
		}
		public boolean sbEnabled() {
		    Plot plot = ((GPlotPage) gnode).page.confPlot();
		    return plot.zoomStack().canZoomIn();
		}
	    };


	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(gproject().newPlot.item());
	    menu.add(clonePlot.item());
	    menu.add(importPlot.item());
	    menu.add(removePlot.item());
	    menu.addSeparator();
	    menu.add(store.item());
	    menu.add(exportData.item());
	    menu.add(printGraph.item());
	    menu.add(newPrintGraph.item());
	    menu.add(printText.item());
	    menu.add(exportEPS.item());
	    mbar.add(menu);

	    JMenu rowMenu = newMenu("Reset # rows ...");
	    ButtonGroup group = new ButtonGroup();
	    b_rows = new JRadioButtonMenuItem[4];
	    for (int i=0; i<b_rows.length; i++) {
		GConfAction act = new GConfAction(this, true, i+1);
		b_rows[i] = act.radio();
		rowMenu.add(b_rows[i]);
		group.add(b_rows[i]);
	    }

	    JMenu colMenu = newMenu("Reset # columns ...");
	    group = new ButtonGroup();
	    b_cols = new JRadioButtonMenuItem[4];
	    for (int i=0; i<b_cols.length; i++) {
		GConfAction act = new GConfAction(this, false, i+1);
		b_cols[i] = act.radio();
		colMenu.add(b_cols[i]);
		group.add(b_cols[i]);
	    }

	    menu = newMenu("View");
	    b_show = new JRadioButtonMenuItem[NSHOW];
	    for (int i=0; i<NSHOW; i++) {
		if (i == CONF) {
		    JMenuItem l = new JMenuItem("For all plots:");
		    l.setFont(glook().bigFont());
		    l.setForeground(glook().disabledColor());
		    menu.add(l);
		    menu.add(rowMenu);
		    menu.add(colMenu);
		}
		if (i == TITLE) {
		    menu.addSeparator();
		    JMenuItem l = new JMenuItem("For active plot only:");
		    l.setFont(glook().bigFont());
		    l.setForeground(glook().disabledColor());
		    menu.add(l);
		}
		b_show[i] = a_show[i].radio();
	   	menu.add(b_show[i]);
	    }
	    mbar.add(menu);

	    // zoom
	    // 1 Dec 2006: Mac OS Firefox Java 1.5 plugin 
	    // JMenu must not contain any JLabels to work properly 
	    // replaced existing JLabels w/ JMenuItems
	    menu = newMenu("Zoom");
	    JMenuItem l = new JMenuItem("For active plot only:");
	    l.setFont(glook().bigFont());
	    l.setForeground(glook().disabledColor());
	    menu.add(l);    
	    menu.add(zoomAuto.item());
	    menu.add(zoomOut.item());
	    menu.add(zoomIn.item());
	    mbar.add(menu);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);
	}

	// live update
	public void liveUpdate() {
	    GNode gnode = tabs.selectedNode();
	    if (gnode instanceof GPlotGraph)
	    	((GPlotGraph) gnode).liveUpdate();
	}

	// is text visible
	public boolean textVisible() {
	    return tabs.selectedNode() == text;
	}

	// config action
	public class GConfAction extends GAction {
	    private boolean isRows; // rows, columns
	    private int ct; // 1,2,3,4
	    public GConfAction(GNode n, boolean r, int c) {
		super(n, "" + c);
		isRows = r;
		ct = c;
	    }
	    public void doit() throws Xcept {
		IntControl cntl = isRows ?
		    page.nRow : page.nCol;
		cntl.setVal(ct);
		refresh();
	    }
	}

	// GTheme has been updated
	public void lookUpdated() {
	    reconfig();
	}

	// refresh, set menu item state
	public void refresh() {
	    if (refreshing) return;
	    if (needsContent) makeContent();
	    setTabLabel(l_hdr);
	    int n = page.nRow.val()-1;
	    if (n>=0 && n<b_rows.length)
	       b_rows[n].setSelected(true);
	    n = page.nCol.val()-1;
	    if (n>=0 && n<b_cols.length)
	       b_cols[n].setSelected(true);
	    reconfig();
	    for (int i=0; i<NSHOW; i++) 
		b_show[i].setSelected(a_show[i].val());
	    super.refresh();
	}

	// resize configurator
	public void reconfig() {
	    conf.reconfig();
	    boolean showConf = a_show[CONF].val();
	    JComponent jconf = conf.jcomp();
	    JComponent jtabs = tabs.jcomp();
	    int w = content.getWidth();
	    int h = content.getHeight();
	    jconf.setVisible(showConf);
	    int ytabs = 0;
	    int htabs = h;
	    if (showConf) {
		Dimension dim = jconf.getPreferredSize();
		dim.width = w;
		jconf.setSize(dim);
	    	ytabs = dim.height; 
		htabs -= ytabs;
	    }
	    jtabs.setLocation(0, ytabs);
	    jtabs.setSize(w, htabs);
	}

	// get Printable object
	public Printable getPrintable() throws Xcept {
	    GraphRender render = graph().gplot(0).render();
	    return (Printable) render;
	}

	// query
	public PlotPage page() { return page; }
	public GPlotGraph graph() { return graph; }
	public Plot currPlot() {
	    String n = "plot" + page.confPlot.val();
	    return (Plot) page.child(n);
	} 

	// get importable plots (all but current)
	public ImportablePlot[] getImportablePlots() 
	throws Xcept {
	    Plot currPlot = currPlot();
	    ArrayList<ImportablePlot> iplots = new ArrayList<ImportablePlot>();
	    PNamed.List pages = gproject().project().children(PlotPage.class);
	    for (int i=0; i<pages.size(); i++) {
	    	PlotPage page = (PlotPage) pages.get(i);
		PNamed.List plots = page.children(Plot.class);
		for (int j=0; j<plots.size(); j++) {
		    Plot plot = (Plot) plots.get(j);
		    if (plot == currPlot) continue;
		    iplots.add(new ImportablePlot(plot));
		}
	    }
	    return iplots.toArray(new ImportablePlot[0]);
	}

	// importable plot
	public static class ImportablePlot {
	    public Plot plot;
	    public ImportablePlot(Plot plot) {
	    	this.plot = plot;
	    }
	    public String toString() {
	    	String n = plot.name().substring(4);
		try {
		    n = " - plot " + (Util.toInt(n) + 1);
		} catch (Xcept e) {
		    n = " - plot???";
		}
		if (plot.page().nPlot() == 1)
		    n = "";
		return plot.parent().name() + n;
	    }
	}

	// show action
	public static class GShowAction extends GAction {
	    int which;
	    private static final String[] names = {
		"Show configurator",
		"Show vertical rule",
		"Show horizontal rule",
		"Show axis-bounds slider",
		"Show title",
		"Show legend",
		"Show X-axis label",
		"Show Y-axis label",
		"Show footer"
	    };
		
	    // constructor
	    public GShowAction(GNode g, int w) {
		super(g, names[w]);
		which = w;
		setEnabled(); // needs which set first!!!
	    }

	    // do action
	    public void doit() throws Xcept {
		BooleanControl cntl = cntl();
		boolean b = cntl.val();
		cntl.setVal(!b);
		GPlotPage gpage = (GPlotPage) gnode;
		if (which == CONF)
		    gpage.reconfig();
		else
		    gpage.refresh();
	    }

	    // value
	    public boolean val() { return cntl().val(); }

	    // get control
	    private BooleanControl cntl() {
		PlotPage page = ((GPlotPage) gnode).page;
		Plot plot = page.confPlot();
		switch (which) {
		case CONF: return page.showConf; 
		case XRULE: return page.showXRule;
		case YRULE: return page.showYRule; 
		case MINMAX: return page.showMinMaxSliders;
		case TITLE: return plot.showTitle;
		case LEGEND: return plot.showLegend;
		case XAXIS: return plot.xaxis.showLabel;
		case YAXIS: return plot.yaxis.showLabel;
		case FOOTER: return plot.showFooter;
		}
		return null;
	    }
	}

	// GSlaveAction
	public static class GSlaveAction extends GAction {
	    public GAction master;

	    // constructor
	    public GSlaveAction(GPlotPage gpage, GAction a) {
		super(gpage, a.name());
		master = a;
	    }

	    // do action
	    public void doit() throws Xcept {
		master.doit();
	    }

	    // s/b enabled
	    public boolean sbEnabled() {
		return ((GPlotPage) gnode).textVisible();
	    }
	}
	    
}
