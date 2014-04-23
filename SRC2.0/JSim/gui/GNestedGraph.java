/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// nested graphics

package JSim.gui;

import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.graph.*;
import JSim.gui.gsgraph.*;

public class GNestedGraph extends GNode implements MouseListener {
	private PContent content; // content panel
	private JPanel graphLabels;  // contains graphs/xlabels/ylabels
	private GNestedLabels x1labels, y1labels, x2labels, y2labels;
	private JPanel graphs; // graph array panel
	private JComponent zgraph; // single graph array
	private JPanel tpanel, fpanel; // title,footer panels
	private JLabel jtitle; // plot title
	private JLabel jmsg; // user message
	private JLabel jfooter; // footer text
	private ArrayList<GraphRender> renders; // for graphs
	private GraphRender zrender; // for zoom
	private int lastNx, lastNy; // last ndata layout parms
	private boolean lastShowTitle, lastShowFooter, lastZoom; // last shows
	private PNestedData ndata;
	private Data.List datas; 
	private ArrayList<PNestedDataItem.Attr> attrs; 
	private int naxes;  // # axes (2 or 3)
	
	// constructor
	public GNestedGraph(GNode g, PNested p) {
	    super(g, p);

	    content = new PContent(new BorderLayout());
	    content.setBackground(Color.white);
	    setJComp(content);
	    graphLabels = new JPanel(new BorderLayout());
	    graphLabels.setBackground(Color.white);
	    x1labels = new GNestedLabels(false);
	    y1labels = new GNestedLabels(true);
	    x2labels = new GNestedLabels(false);
	    y2labels = new GNestedLabels(true);
	    graphs = new JPanel();
	    graphs.setBackground(Color.white);
	    zrender = gappl().newGraphRender(null);
	    zrender.addMouseListener(this);
	    zgraph = zrender.jcomp();
	    
	    tpanel = new JPanel(new GridLayout(1,1));
	    tpanel.setBackground(Color.white);
	    jtitle = new JLabel("title", SwingConstants.CENTER);
	    jtitle.addMouseListener(this);
	    tpanel.add(jtitle);
	    jmsg = new JLabel("User message");
	    fpanel = new JPanel(new GridLayout(1,1));
	    fpanel.setBackground(Color.white);
	    jfooter = new JLabel("footer", SwingConstants.CENTER);
	    jfooter.addMouseListener(this);
	    fpanel.add(jfooter);
	    renders = new ArrayList<GraphRender>();
	    lastNx = lastNy = -1;  //  force reconfig 1st time
	}

	// refresh
	public void refresh() {
	    String title = pnested().title.val();
	    if (Util.isBlank(title)) title = PNested.TITLE_DEFAULT;
	    jtitle.setText(title);
	    String footer = pnested().footer.val();
	    if (Util.isBlank(footer)) footer = PNested.FOOTER_DEFAULT;
	    jfooter.setText(footer);
	    ndata = gnested().getData();
	    if (needsReconfig())
	    	reconfig();
	    
	    // no data: message only
	    if (ndata == null) {
	    	jmsg.setText(gnested().statusMsg());
		return;
	    }
	
	    // set labels
	    setLabels(x1labels, PNestedAxis.X1);	
	    setLabels(y1labels, PNestedAxis.Y1);
	    if (zoom()) {
	    	int x = ndata.xinx(zoomInx());
		int y = ndata.yinx(zoomInx());
	    	setZoomLabels(x2labels, ndata.nx(), PNestedAxis.X2, x);
	   	setZoomLabels(y2labels, ndata.ny(), PNestedAxis.Y2, y);
	    } else {	
	    	setLabels(x2labels, ndata.nx(), PNestedAxis.X2);
	    	setLabels(y2labels, ndata.ny(), PNestedAxis.Y2);
	    }

	    // set graph layouts
	    datas = gnested().getDataList();
	    attrs = gnested().getAttrList();
	    naxes = (ndata.style() == PNested.XY_PLOT) ? 2 : 3;
	    for (int rx=0; rx<nxy(); rx++) {
		try {
	    	    refresh(rx);
		} catch (Exception e) {
		    System.err.println("====GNestedGraph refresh #" + rx);
		    e.printStackTrace();
		    System.err.println("");
		}
	    }
	}

	// set X1/Y1 label
	private void setLabels(GNestedLabels glabs, String axisID) {
	    String s = ndata.getAxisDomain(axisID);
	    if (s == null) s = "";
	    glabs.setLabels(new String[] { s });
	}

	// set X2/Y2 labels
	private void setLabels(GNestedLabels glabs, int n, 
	String axisID) {
	    if (n < 1) return;
	    StringList labels = new StringList();
	    for (int i=0; i<n; i++) {
	    	double v = ndata.getAxisDomainValue(axisID, i);
		String s = PrettyFormat.sformat(v, 4);
		if (i==0)
		    s = ndata.getAxisDomain(axisID) + "=" + s;
		if (Double.isNaN(v)) s = "";
	    	labels.add(s);
	    }
	    glabs.setLabels(labels);
	} 

	// set single value X2/Y2 labels
	private void setZoomLabels(GNestedLabels glabs, int n, 
	String axisID, int inx) {
	    if (n < 1) return;
	    String dom = ndata.getAxisDomain(axisID);
	    double v = ndata.getAxisDomainValue(axisID, inx);
	    String s = (dom==null) ? "" :
	        (dom + "=" + PrettyFormat.sformat(v, 4));
	    glabs.setLabels(new StringList(s));
	}
	    
	// refresh one graph, r=GridLayout inx
	private void refresh(int r) throws Exception {
	    int n = dataInxForGraph(r);
	    int x = ndata.xinx(n);
	    int y = ndata.yinx(n);
	    GraphLayout l = new GraphLayout();
	    l.style = ndata.style();
	    l.showFooter = zoom();
	    if (zoom()) l.footer = "CLICK TO ZOOM OUT";
	    l.showLegend = zoom();
	    DataRange[] ranges = gnested().getData().getGraphRanges(n);
	    l.xaxis = makeAxis(ranges[0]);
	    l.yaxis = makeAxis(ranges[1]);
	    if (ranges.length > 2)
	    	l.zaxis = makeAxis(ranges[2]);
	    l.xaxis.showTics = zoom() || y==0;
	    l.yaxis.showTics = zoom() || x==0;
	    ArrayList<GraphData> gdatas = new ArrayList<GraphData>();
	    for (int i=0; i<datas.size(); i++) {
	        PNestedDataItem.Attr attr = attrs.get(i);
		if (attr.x2inx >= 0 && attr.x2inx != x) continue;
		if (attr.y2inx >= 0 && attr.y2inx != y) continue;
		Data data = datas.get(i);
		GraphData gd = makeGraphData(data, attr);
	    	if (gd != null) gdatas.add(gd);
	    }
	    l.data = new GraphData[gdatas.size()];
	    for (int i=0; i<gdatas.size(); i++)
	    	l.data[i] = gdatas.get(i);
	    GraphRender render = zoom() ? zrender : renders.get(r);
	    render.setGraphLayout(l, null);
	}

	// make a GraphLayout.Axis
	private GraphLayout.Axis makeAxis(DataRange r) {
	    GraphLayout.Axis axis = new GraphLayout.Axis();
	    axis.auto = r.isAutoscale();
	    axis.log = r.isLog();
	    axis.min = r.min();
	    axis.max = r.max();
	    if (Double.isNaN(axis.min)) {
	    	axis.min = 0;
		axis.max = 1;
	    }
	    if (axis.min == axis.max) {
	        double delta = Math.abs(axis.min)/10;
		if (delta < 1e-7) delta = 1e-7;
	    	axis.min -= delta;
		axis.max += delta;
	    }
	    return axis;
	} 
	
	// make GraphData from Data and Attr
	private GraphData makeGraphData(Data data,
	PNestedDataItem.Attr attr) throws Xcept {
	    if (data.ndim() > 0 && data.ndim()+1 != naxes)
	    	return null; // can't render data
	    GraphData gd = new GraphData();
	    gd.label = data.legend();
	    gd.color = glook().plotColor(attr.color);
	    gd.shape = attr.shape;
	    gd.size = plotSize(attr.size);
	    gd.line = attr.line;
	    gd.thickness = attr.thickness;
	    gd.colorMap = attr.colorMap;
	    gd.palette = attr.palette;
	    setSamples(data, gd); 
	    return gd;
	}

	// symbol size for plots / icons
	private int plotSize(int i) {
	    double f = 0.4;
	    switch(i) {
	    case 0:  f = 0.275; break;
	    case 1:  f = 0.4; break;
	    case 2:  f = 0.55; break;
	    case 3:  f = 0.7; break;
	    }
	    return (int) (glook().fontSize() * f);
	} 

	// put Data.samples into GraphData
	private void setSamples(Data data0, GraphData gd) 
	throws Xcept {
	    PlotData data = (data0 instanceof PlotData) ?
	    	(PlotData) data0 : new PlotData(data0);
	    int ndim = data.ndim();
	    double[] samp0 = data.data(0).samples();

	    // special case for 0D data
	    if (ndim == 0) {
		gd.isConstant = true;
		if (naxes == 2) {
		    gd.x = new double[2];
		    gd.y = new double[] { samp0[0], samp0[0] };
		} else {
		    gd.x = new double[2];
		    gd.y = new double[2];
		    gd.z = new double[] { 
		    	samp0[0], samp0[0], samp0[0], samp0[0]};
		}
		return;
	    }

	    // dimensional agreement
	    if (ndim+1 == naxes) {
	        gd.isConstant = false;
	    	gd.x = samp0;
	    	gd.y = data.data(1).samples();
	    	if (naxes > 2) 
		    gd.z = data.data(2).samples();
		return;
	    }

	    // axes/data dimension mismatch
	    throw new Xcept("Plot style supports 0D or " +
		(naxes-1) + "D data,  but \"" + 
		data.desc() + "\" is " + ndim + "D.");
	}	    


	// new reconfig() needed to set overall geometry?
	private boolean needsReconfig() {
	    if (zoom() != lastZoom) return true;
	    if (nx() != lastNx) return true;
	    if (ny() != lastNy) return true;
	    if (lastShowTitle != pnested().showTitle.val())
	    	return true;	
	    if (lastShowFooter != pnested().showFooter.val())
	    	return true;
	    return false;
	}

	// reconfig graphs content
	private void reconfig() {
	    lastZoom = zoom();
	    lastNx = nx();
	    lastNy = ny();
	    lastShowTitle = pnested().showTitle.val();
	    lastShowFooter = pnested().showFooter.val();

	    // config content
	    content.removeAll();    
	    if (lastShowTitle)
	    	content.add(tpanel, BorderLayout.NORTH);
	    content.add(graphLabels, BorderLayout.CENTER);
	    if (lastShowFooter)
	    	content.add(fpanel, BorderLayout.SOUTH);

	    // config graphLabels
	    graphLabels.removeAll();
	    if (zoom()) 
	    	graphLabels.add(zgraph, BorderLayout.CENTER);
	    else
	    	graphLabels.add(graphs, BorderLayout.CENTER);	        
	    if (ndata != null) {
	    	graphLabels.add(x1labels, BorderLayout.SOUTH);
	    	if (ndata.style() != PNested.XY_PLOT)
	    	    graphLabels.add(y1labels, BorderLayout.WEST);
	    	if (ndata.nx() > 1) graphLabels.add(x2labels,
	    	    BorderLayout.NORTH);
	    	if (ndata.ny() > 1) graphLabels.add(y2labels,
	    	    BorderLayout.EAST);
	    }

	    // config graph array
	    graphs.removeAll();
	    if (ndata == null) {
	       	graphs.setLayout(new FlowLayout());
		graphs.add(jmsg);
		return;
	    }	
	    graphs.setLayout(new GridLayout(ny(), nx()));
	    for (int i=0; i<nxy(); i++) {
	    	if (i >= renders.size()) {	
		    GraphRender r = gappl().newGraphRender(null);
		    renders.add(r);
		    r.addMouseListener(this);
		}
	        graphs.add(renders.get(i).jcomp());
	    }
	}

	// usused MouseListener methods
        public void mouseEntered(MouseEvent e) { } 
        public void mouseExited(MouseEvent e) { } 
        public void mousePressed(MouseEvent e) { } 
        public void mouseReleased(MouseEvent e) { } 

	// mouse click does stuff
	public void mouseClicked(MouseEvent e) {
	    StringControl c = null;
	    if (e.getSource() == jtitle) {
	    	editLabel(pnested().title);
		return;
	    }
	    if (e.getSource() == jfooter) {
	    	editLabel(pnested().footer);
		return;
	    }
	    if (e.getSource() == zrender) {
		gnested().setUnzoom();
		gnested().refresh();
		return;
	    }
	    if (nxy() < 2) return; // no zoom for small
	    for (int i=0; i<renders.size(); i++) {
	    	if (renders.get(i) != e.getSource()) continue;
		gnested().setZoom(dataInxForGraph(i));
		gnested().refresh();
		return;
   	    }
	}
	
	// edit title/footer
	private void editLabel(StringControl c) {
	    String ntext = JOptionPane.showInputDialog(
		"Enter new " + c.name(), 
		c.val());
	    if (! Util.isBlank(ntext)) try {
	    	c.setVal(ntext);
	    } catch (Xcept x) {
		System.err.println("" + x);
	    }
	    refresh();
	}

	// print page
	public void print(PrinterJob pjob)
	throws Xcept {
	    pjob.setPrintable(content);
	    try {
		pjob.print();
	    } catch (PrinterException e) {
		throw new Xcept(e.toString());
	    }
	}

	// simple query
	public GNested gnested() { 
	    return (GNested) ancestor(GNested.class); 
	}
	public PNested pnested() { return (PNested) pnamed(); }
	public int zoomInx() { return gnested().zoomInx(); }
	public boolean zoom() { return zoomInx() >= 0; }
	public int nx() {
	    if (ndata == null) return 0;
	    return zoom() ? 1 : ndata.nx();
	}
	public int ny() {
	    if (ndata == null) return 0;
	    return zoom() ? 1 : ndata.ny();
	}
	public int nxy() {
	    if (ndata == null) return 0;
	    return zoom() ? 1 : ndata.nxy();
	}
	public int dataInxForGraph(int rx) {
	    if (zoom()) return zoomInx();
	    int x = ndata.xinx(rx);
	    int y = ndata.ny() - 1 - ndata.yinx(rx);
	    return ndata.inx(x, y); 
	}

	// printable content class
	public static class PContent extends JPanel implements
	Printable{
	    public PContent(LayoutManager l) {
	    	super(l);
	    }
	    
	    // print
	    public int print(Graphics g, PageFormat pf, int pageIndex) {
		double ix = pf.getImageableX();
		double iy = pf.getImageableY();
		double ih = pf.getImageableHeight();
		double iw = pf.getImageableWidth();
		double cw = getWidth(); // component width
		double ch = getHeight(); // component width
		double sx = iw / cw;
		double sy = ih / ch;
		double s = Math.min(sx, sy);

	    	if (pageIndex > 0) return Printable.NO_SUCH_PAGE;
//System.err.println("GNestedGraph.print() ix=" + ix + " iy=" + iy
//+ " iw=" + iw + " ih=" + ih + " cw=" + cw + " ch=" + ch + " sx=" + sx + " sy=" + sy + " s=" + s);
	    	Graphics2D g2d = (Graphics2D) g;
	    	g2d.translate(ix, iy);
		g2d.scale(s, s);
		RepaintManager mgr = RepaintManager.currentManager(this);
	        mgr.setDoubleBufferingEnabled(false);
	    	print(g);
	    	mgr.setDoubleBufferingEnabled(true);
		g2d.scale(1/s, 1/s);
	    	g2d.translate(-ix, -iy);
	    	return Printable.PAGE_EXISTS;
	    }
	}

}

	
	
	
