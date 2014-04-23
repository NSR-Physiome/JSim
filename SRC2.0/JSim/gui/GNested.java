/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// GUI for nested plot tab

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

public class GNested extends GNode {

	// state
	private PNested pnested;
	private JRootPane root;
	private GTabs tabs;
	public GNestedConf conf;
	public GNestedGraph graph;
	public GNestedText text;
	private JPanel content;
	private JLabel l_hdr;
	private PNestedData data; // latest nested data
	private String statusMsg; // blank or data exception msg
	private int zoomInx; // dataset to zoom into or UNZOOM
	private int zoomNx, zoomNy; // nx, ny if zoomed

	public static final int UNZOOM = -1;

	// actions, controls
	public GAction clonePNested, removePNested, 
	    exportData, store, printGraph;
	// public GAction printText, exportEPS; not yet implemented
	public GBooleanRadioControl g_xnesting, g_ynesting;
	public GBooleanRadioControl[] g_attrs, g_shows;

	// constructor from PNotes
	public GNested(GNode g, PNested p) throws Xcept {
	    super(g, p);
	    pnested = p;
	    zoomInx = -1;

	    // create widgets
	    root = new JRootPane();
	    content = new JPanel(new BorderLayout());
	    root.setContentPane(content);
	    setJComp(root);
	    makeContent();	
	    gproject().addTab(this);
	}

	// make content
	public void makeContent() {
	    super.makeContent();

	    tabs = new GTabs(this, pnested);
	    tabs.setTabPlacement(SwingConstants.BOTTOM);
	    graph = new GNestedGraph(tabs, pnested);
	    tabs.addTab("Graph", graph);
	    text = new GNestedText(tabs, pnested);
  	    tabs.addTab("Text", text);
	    conf = new GNestedConf(this, pnested); // after graph create, for icons
	    JComponent jconf = conf.jcomp();
//	    jconf.setSize(jconf.getPreferredSize());
	    tabs.jcomp().setLocation(0, jconf.getHeight());

	    // create actions
	    clonePNested = new GAction(this, "Clone this page ...") {
		public void doit() throws Xcept {
		    gproject().cloneNode(gnode);
		}
	    };
	    removePNested = new GAction(this, "Remove this page ...") {
		public void doit() throws Xcept {
		    gproject().removeNode(gnode);
		}
	    };

  	    exportData = new GAction(this, "Export data file ...") {
		public void doit() throws Xcept {
		    Data.List data = getDataList();
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


	    // print page graphics
	    printGraph = new GAction(this, "Print graph(s) ...") {
		public void doit() throws Xcept {
		    PrinterJob pjob = PrinterJob.getPrinterJob();
		    pjob.setJobName(pnested.name());
		    if (! pjob.printDialog()) 
			throw new Xcept("Printing canceled");
		    gproject().message("Sending graph to printer...");
		    graph.print(pjob);
		}
	    };
/*
	    // print page text
	    printText = new GAction(this, "Print text ...") {
		public void doit() throws Xcept {
		    String title = gproject().title() + " PNested Text";
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
*/
	    // store dataset in project action
	    store = new GAction(this, "Store project dataset ...") {
		public void doit() throws Xcept {
		    Data.List dlist = getDataList();
		    if (dlist.size() == 0) {
			gnode.warning("No data is currently available");
		        return;
		    }
		    gproject().pushTree();
		    String msg = "Store nested data under name";
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

	    // axis show controls
	    g_xnesting = new GBooleanRadioControl(this, pnested.xnesting,
	    	"Enable X-axis nesting");
	    g_xnesting.addAuxNode(this);
	    g_ynesting = new GBooleanRadioControl(this, pnested.ynesting,
	    	"Enable Y-axis nesting");
	    g_ynesting.addAuxNode(this);
	    g_attrs = new GBooleanRadioControl[PNestedAxis.ATTRIDS.length];
	    for (int i=0; i<g_attrs.length; i++) {
	    	String id = PNestedAxis.ATTRIDS[i];
	    	String text = "Enable " + id + " variation";
		g_attrs[i] = new GBooleanRadioControl(this,
		    pnested.globalAxis(id).show, text);
		g_attrs[i].addAuxNode(this);
	    }	    

	    // show controls
	    BooleanControl[] c_shows = new BooleanControl[] {
		pnested.showConf, 
		pnested.showTitle,
		pnested.showFooter
	    };
	    String[] l_shows = new String[] {
		"Show configurator",
		"Show title",
		"Show footer"
	    };
	    g_shows = new GBooleanRadioControl[c_shows.length];
	    for (int i=0; i<g_shows.length; i++) {
	    	g_shows[i] = new GBooleanRadioControl(this,
		    c_shows[i],  l_shows[i]);
		g_shows[i].addAuxNode(this);
	    }

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(gproject().newNested.item());
	    menu.add(clonePNested.item());
	    menu.add(removePNested.item());
	    menu.addSeparator();
	    menu.add(store.item());
	    menu.add(exportData.item());
	    menu.add(printGraph.item());
//	    menu.add(printText.item());
//	    menu.add(exportEPS.item());
	    mbar.add(menu);

	    menu = newMenu("Nesting");
	    menu.add(g_xnesting.jcomp());
	    menu.add(g_ynesting.jcomp());
	    for (int i=0; i<g_attrs.length; i++)
	    	menu.add(g_attrs[i].jcomp());
	    mbar.add(menu);

	    menu = newMenu("View");
	    for (int i=0; i<g_shows.length; i++)
	    	menu.add(g_shows[i].jcomp());
	    mbar.add(menu);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);
	}

	// is text visible
	public boolean textVisible() {
	    return tabs.selectedNode() == text;
	}

	// refresh, set menu item state
	public void refresh() {
	    if (refreshing) return;
	    if (needsContent) makeContent();
	    setTabLabel(l_hdr);
	    data = null;
	    statusMsg = "";
	    try {
	    	data = new PNestedData(pnested);
	    	if (data.nx() != zoomNx || data.ny() != zoomNy)
	    	    setUnzoom();
		text.needsRefresh = true;
	    } catch (Exception e) {
	    	gproject().warning(e);
		setStatus(e);
	    }
	    conf.reconfig();
	    boolean showConf = pnested.showConf.val();
	    JComponent jconf = conf.jcomp();
	    JComponent jtabs = tabs.jcomp();
	    content.removeAll();
	    if (showConf) 
	    	content.add(conf.jcomp(), BorderLayout.NORTH);
	    content.add(tabs.jcomp(), BorderLayout.CENTER);
	    for (int i=0; i<g_attrs.length; i++) {
	    	String id = PNestedAxis.ATTRIDS[i];
		g_attrs[i].jcomp().setEnabled(
		    pnested.globalAxis(id).isActivatable());
	    }
//	    graph.setMessage(statusMsg);
	    super.refresh();
	}

	// refresh data due to slider change
	protected void refreshData() {
	    graph.refresh();
	    text.refresh();
	}

	// query
	public PNested pnested() { return pnested; }

	// set stuff
	public void setStatus(Exception e) {
	    gproject().warning(e);
	    statusMsg = (e instanceof Xcept) ?
	    	((Xcept) e).cleanMessage() : e.getMessage();
	    if (Util.isBlank(statusMsg)) statusMsg = "" + e;
	}
	protected void setZoom(int inx) {
	    if (data == null || inx < 0) {
	    	setUnzoom();
	    } else {
	    	zoomInx = inx;
	    	zoomNx = data.nx();
		zoomNy = data.ny();
	    }
	}
	protected void setUnzoom() {
	    zoomInx = UNZOOM;
	}

	// query
	public PNestedData getData() { return data; }
	public Data.List getDataList() {
	    if (data == null) return new Data.List();
	    return data.getGraphData();
	}
	public ArrayList<PNestedDataItem.Attr> getAttrList() {
	    if (data == null) 
	    	return new ArrayList<PNestedDataItem.Attr>();
	    return data.getGraphAttrs();
	}
	public String statusMsg() { return statusMsg; }
	public boolean isZoom() { return zoomInx > UNZOOM; }
	public int zoomInx() { return zoomInx; }
}
