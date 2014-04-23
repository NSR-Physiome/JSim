/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// PDataSet viewer

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.graph.*;

public class GDataSetView extends GNode implements ListSelectionListener {

	// static state
	private static Data.List copyBuffer;

	// state
	private JRootPane root; // root
	private JPanel jpanel; // main panel
	private Dimension jpanelDim;
	private PDataSet dataset;
	private JLabel l_hdr, l_from, l_log;
	private JList jlist;
	private JScrollPane dscroll;
	private GraphRender graph;
	private JMenuItem jlegacy, jascii;
	private GDataSetFilter gfilter;
	private GBooleanControl g_log; // linear/log control
	private Box b_log; // log box

	// actions
	private GAction exportAll, exportSel, 
	    cut, copy, paste, rename, filter,
	    legacy, ascii;

	// constructor
	public GDataSetView(GNode p, PDataSet d) {
	     super(p, d);
	     dataset = d;

	    // root/jpanel 
	    root = new JRootPane();
	    jpanel = new JPanel(null) {
		public void doLayout() { reconfig(); } 
	    };
	    root.getContentPane().add(jpanel);
	    setJComp(root);

	    // filter gnode
	    gfilter = new GDataSetFilter(this, dataset);
	}

	// make content
	public void makeContent() {
	    needsContent = false;

	    // File menu ctions
	    exportAll = new GAction(this, "Export all data to file...") {
		public void doit() throws Xcept {
		    ((GDataSetView) gnode).exportSome(true);
		}
	    };
	    exportSel = new GAction(this, "Export selected data to file...") {
		public void doit() throws Xcept {
		    ((GDataSetView) gnode).exportSome(false);
		}
	    };

	    // Edit menu actions
	    cut = new GAction(this, "cut") {
		public void doit() throws Xcept {
		    copyBuffer = getSelectedData1();
		    for (int i=0; i<copyBuffer.size(); i++) {
			Data d = copyBuffer.data(i);
			dataset.removeData(d.name());
		    }
		    ((GDataSetView) gnode).reloadData();
		    gnode.gproject().project().revalidate();
		    gnode.gproject().refresh();
		}
	    };
	    copy = new GAction(this, "copy") {
		public void doit() throws Xcept {
		    copyBuffer = getSelectedData1();
		}
	    };
	    paste = new GAction(this, "paste") {
		public void doit() throws Xcept {
		    if (copyBuffer == null || copyBuffer.size()<1)
		       throw new Xcept("No data is paste buffer");
		    dataset.importData(copyBuffer);
		    ((GDataSetView) gnode).reloadData();
		    gnode.gproject().project().revalidate();
		    gnode.gproject().refresh();
		}
	    };
	    rename = new GAction(this, "rename") {
		public void doit() throws Xcept {
		    String errmsg = null;
		    int[] inxs = getSelectedIndices();
		    Data.List dlist = getSelectedData1();
		    for (int i=0; i<dlist.size(); i++) {
			Data data = dlist.data(i);
			String msg = "Rename data " + data.name();
			String nname = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, "Rename", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().datasetIcon(),
			    null, data.name());
			if (nname == null) break;
			dataset.rename(data, nname);
		    }
		    ((GDataSetView) gnode).reloadData();
		    gnode.gproject().project().revalidate();
		    gnode.gproject().refresh();
		    setSelectedIndices(inxs);	    
		    if (errmsg != null) throw new Xcept(dataset, errmsg);
		}
	    };
	    filter = new GAction(this, "filter") {
		public void doit() throws Xcept {
		    gfilter.show();
		}
	    };


	    // Encoding menu actions
	    legacy = new GAction(this, "legacy") {
		public void doit() throws Xcept {
		    ((GDataSetView) gnode).setEncoding("legacy");
		}
	    };
	    ascii = new GAction(this, "ascii") {
		public void doit() throws Xcept {
		    ((GDataSetView) gnode).setEncoding("ascii");
		}
	    };
	
	    // menubar - File Menu
	    JMenuBar mbar = new JMenuBar();

	    // header
	    l_hdr = new JLabel("", JLabel.RIGHT);
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);

	    // menubar - File Menu
	    JMenu menu = newMenu("File");
	    menu.add(exportAll.item());
	    menu.add(exportSel.item());
	    mbar.add(menu);

	    // Edit menu
	    menu = newMenu("Edit");
	    menu.add(cut.item());
	    menu.add(copy.item());
	    menu.add(paste.item());
	    menu.addSeparator();
	    menu.add(rename.item());
	    menu.add(filter.item());
	    mbar.add(menu);

	    // Encoding menu
	    menu = newMenu("Encoding");
	    jlegacy = legacy.item();
	    menu.add(jlegacy);
	    jascii = ascii.item();
	    menu.add(jascii);
	    mbar.add(menu);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);

	    // log scale
	    b_log = new Box(BoxLayout.X_AXIS);
	    l_log = new JLabel("Log");
	    b_log.add(l_log);
	    g_log = new GBooleanControl(this, dataset.log);
	    g_log.addAuxNode(this);
	    b_log.add(g_log.jcomp());
	    jpanel.add(b_log);

	    // from file
	    String filename = dataset.origFile.val();
	    l_from = new JLabel("from file: " + filename);
	    jpanel.add(l_from);
			
	    // scolled list
	    jlist = new JList();
	    jlist.setBackground(glook().dark());
	    jlist.addListSelectionListener(this);
	    dscroll = new JScrollPane(jlist);
	    jpanel.add(dscroll);

	    // graph
	    graph = gappl().newGraphRender(null);
	    jpanel.add(graph.jcomp());

	}

	// reconfigure jpanel
	private void reconfig() {

	    // no action if same size
	    Dimension dim = jpanel.getSize();
	    if (jpanelDim != null 
	    && jpanelDim.width == dim.width
	    && jpanelDim.height == dim.height)
		return;

	    // spacing parameters
	    jpanelDim = dim;
	    int sp = glook().fontSize()*2;
	    int y = 0;

	    // header
	    l_hdr.setFont(glook().bigFont());
	    l_hdr.setLocation(0,y);
	    setPref(l_hdr);

	    // log scale, from file
	    setPref(b_log);
	    String filename = dataset.origFile.val();
	    l_from.setVisible(! Util.isBlank(filename));
	    if (l_from.isVisible()) {
	    	l_from.setLocation(3*sp,y);
	    	setPref(l_from);
	    }
	    y += sp;
			
	    // reload list data
	    reloadData();

	    // scolled list
	    dscroll.setLocation(sp, y);
	    int w = Math.max(2*sp, dim.width-2*sp);
	    dscroll.setSize(w, sp*10);
	    y += sp*11;

	    // graph
	    graph.jcomp().setLocation(sp,y);
	    graph.jcomp().setSize(w, sp*10);
	    y += sp*10;
	}

	//  reload list data
	protected void reloadData() {

	    Data.List datas = dataset.dataList();
	    String[] sdata = new String[datas.size()];
	    for (int i=0; i<datas.size(); i++) {
		Data data = datas.data(i);
		String s = data.name() + data.legendGrids();
		if (! Util.isBlank(data.desc()) 
		&& ! data.name().equals(data.desc()))
		    s = s + " \"" + data.desc() + "\"";
		Unit u = data.unit();
		if (u != null && !Unit.same(u, Unit.scalar()))
		    s = s + " " + data.unit().pubName();
		if (data.ndim() == 0) {
		    try {
		    	s = s + " value=" + Util.pretty(data.realVal(0));
		    } catch (Xcept e) { }
		} else { 
		    s = s + " {" +  data.nsamples() + " samples";
		    if (hasNaNs(data)) s = s + "/nans";
		    if (hasInfs(data)) s = s + "/infs";
		    s = s + "}";
		}
		sdata[i] = s;
	    }
	    jlist.setListData(sdata);
	    int[] inxs = jlist.getSelectedIndices();
	    if (datas.size()>0 
	    && (inxs == null || inxs.length == 0))
		jlist.setSelectedIndex(0);
	}

	// does data have nans?
	private boolean hasNaNs(Data data) {
	    try {
	        double[] s = data.samples();
	        for (int i=0; i<s.length; i++)
	    	    if (Double.isNaN(s[i])) return true;
	        return false;
	    } catch (Xcept e) {
	    	return false;
	    }
	}

	// does data have infs?
	private boolean hasInfs(Data data) {
	    try {
	        double[] s = data.samples();
	        for (int i=0; i<s.length; i++)
	    	    if (Double.isInfinite(s[i])) return true;
	        return false;
	    } catch (Xcept e) {
	    	return false;
	    }
	}

	// set size to preferred size
	private void setPref(JComponent j) {
	    j.setSize(j.getPreferredSize());
	}

	// refresh
	public void refresh() {
	    super.refresh();
	    try {
		setTabLabel(l_hdr);
		refreshGraph();
		refreshMenu();
	    } catch (Exception e) {
		gproject().message(e);
	    }
	}

	// refresh graph
	private void refreshGraph() throws Exception {   

	    // message if no selected data
	    GraphLayout l = new GraphLayout();
	    l.fontSize = glook().fontSize();
	    Data.List sdatas = getSelectedData();
	    if (sdatas.size() == 0) {
	        l.errorMsg = "No data currently selected";
		graph.setGraphLayout(l, null);
		return;
	    }

	    // graph all selected 1D data in XYPLOT
	    l.showLegend = true;
	    l.xaxis = new GraphLayout.Axis();
	    l.yaxis = new GraphLayout.Axis();
	    l.yaxis.log = dataset.log.val();
	    l.data = new GraphData[sdatas.size()];
	    Data.List datas = dataset.dataList();
	    for (int i=0; i<sdatas.size(); i++) {
		Data data = sdatas.data(i);
		if (data.ndim() != 1) {
		    l.data = null;
		    l.errorMsg = "Preview not supported for selected " 
			+ data.ndim() + "D data";
		    graph.setGraphLayout(l, null);
		    return;
		}

		// adjust axes
		data.calcExtrema();
		double ymin = l.yaxis.log ? data.minpos() : data.min();
		GridData grid = data.grid(0);
		if (i==0 || grid.min() < l.xaxis.min) 
		    l.xaxis.min = grid.min();
		if (i==0 || ymin < l.yaxis.min) 
		    l.yaxis.min = ymin;
		if (i==0 || grid.max() > l.xaxis.max) 
		    l.xaxis.max = grid.max();
		if (i==0 || data.max() > l.yaxis.max) 
		    l.yaxis.max = data.max();

		// add data
		GraphData gd = new GraphData();	
		gd.label = data.name();
		gd.x = grid.samples();
		gd.y = data.samples();
		gd.color = glook().goodPlotColor(i);
		gd.shape = GraphData.SHAPE_DIAMOND;
		gd.line = GraphData.LINE_SOLID;
		gd.thickness = GraphData.LINE_THIN;
		gd.size = (int) (glook().fontSize() * 0.4);
		l.data[i] = gd;
	    }		

	    // update graph
	    graph.setGraphLayout(l, null);
	}

	// refresh menu
	private void refreshMenu() throws Xcept {
	    jlegacy.setIcon((dataset.encoding.val() == 0) ?
		glook().selectIcon() : null);
	    jascii.setIcon((dataset.encoding.val() == 1) ?
		glook().selectIcon() : null);
	}

	// get selected data
	protected Data.List getSelectedData() {
	    Data.List datas = dataset.dataList();
	    Data.List ret = new Data.List(8);
	    for (int i=0; i<datas.size(); i++) {
		if (! jlist.isSelectedIndex(i)) continue;
		ret.add(datas.data(i));
	    }
	    return ret;
	}
	private Data.List getSelectedData1() throws Xcept {
	    Data.List ret = getSelectedData();
	    if (ret.size() < 1) throw new Xcept(dataset, 
		"No data curves are currently selected");
	    return ret;
	}
	private Data.List getSelectedData1D() throws Xcept {
	    Data.List dlist = getSelectedData1();
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		if (data.ndim() != 1) throw new Xcept(data,
		    "Operation only supported for 1D data");
	    }
	    return dlist;
	}

	// save/restore selection
	protected int[] getSelectedIndices() {
	    return jlist.getSelectedIndices();
	}
	protected void setSelectedIndices(int[] s) {
	    jlist.setSelectedIndices(s);
	}

	// export all or selected
	protected void exportSome(boolean all) throws Xcept {
	    Data.List data = all ? 
		dataset.dataList() : getSelectedData();
	    if (data.size() == 0) throw new Xcept(
		"no data available to export");
	    GFileChooser.Info info = GFileChooser.select(
		this, false, GFileChooser.DATASET);
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

	// set encoding
	protected void setEncoding(String e) throws Xcept {
	    dataset.encoding.setVal(e);
	    refreshMenu();
	}

	// selection list changed
	public void valueChanged(ListSelectionEvent e) {
	    if (e.getValueIsAdjusting()) return;
	    refresh(); 
	}

	// theme updated
	public void lookUpdated() { reconfig(); } 

}

