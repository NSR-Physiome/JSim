import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.aserver.*;
import JSim.gui.plugin.GGraphic;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class MyGraphic extends GGraphic 
implements ActionListener, ComponentListener {
	private int POLLFREQ = 200; // msec
	private JPanel figure; 
	private PModel pmodel; // attached model
	private ASVar Qra, Qrv, Qla, Qlv;  // query vars
	private double Vra, Vrv, Vla, Vlv; // volumes (50-175)
	private Timer timer; 

	// constructor
	public MyGraphic(GGraphic.Callbacks cb) throws Exception {
	    super(cb);

	    // initialize graphics
	    final int rcell = radius(175);
	    figure = new JPanel(null) {
	    	public void paint(Graphics g) {
		    int w = getSize().width;
		    int h = getSize().height;
		    g.setColor(Color.white);
		    g.fillRect(0,0,w,h);
		    paint(g, Color.blue, rcell, rcell, Vra, "RA");
		    paint(g, Color.blue, rcell, rcell*3, Vrv, "RV");
		    paint(g, Color.red, rcell*3, rcell, Vla, "LA");
		    paint(g, Color.red, rcell*3, rcell*3, Vlv, "LV");
		    if (pmodel != null) {
		    	g.setColor(Color.black);
		        g.drawString(pmodel.name(), 0, 20);
		    }
		}
		public void paint(Graphics g, Color c, int xc, int yc, 
		double v, String name) {
		    if (Double.isNaN(v)) return;
		    if (v<10) v = 10;
		    if (v>200) v = 200;
		    int r = radius(v);
		    g.setColor(c);
		    g.fillOval(xc-r, yc-r, 2*r, 2*r);
		    g.setColor(Color.black);
		    g.drawString(name, xc, yc);
 		}
	    };
	    figure.setPreferredSize(new Dimension(4*rcell, 4*rcell));
	    figure.addComponentListener(this);

	    // initialize timer
	    timer = new Timer(POLLFREQ, this);
	}

	// radius
	public int radius(double v) {
	    return (int) (10 * Math.pow(v, .3333));
	}

	// query frame
	public JComponent getJComponent() { return figure; }
	
	// import state via XML
	public void importXML(Element e) {
	    pmodel = null;
	    if (e == null) {
		pmodel = callbacks().userModelSelect(figure);
	    } else {
		String name = e.getAttribute("modelName");
		PNamed p = callbacks().getProject().child(name);
		if (p instanceof PModel)
		    pmodel = (PModel) p;
	    }    
	}

	// export state via XML
	public void exportXML(Element e) {
	    e.setAttribute("modelName", 
	    	(pmodel == null) ? "" : pmodel.name());
	}

	// job started
	public void jobStarted(PJob pjob) {
	    if (pmodel == null) return;
	    if (pjob.pmodel() != pmodel) return;
	    if (! (pjob instanceof PModelRunJob)) return;
	    Qra = Qrv = Qla = Qlv = null;
	    ASModel asmodel = pmodel.rt();
	    try {
	    	Qra = asmodel.getASVar("Vra");
	    	Qrv = asmodel.getASVar("Vrv");
	    	Qla = asmodel.getASVar("Vla");
	    	Qlv = asmodel.getASVar("Vlv");
	    } catch (Xcept e) {
		System.err.println("" + e);
	    }
	    timer.start();
	}   

	// job stopped
	public void jobStopped(PJob pjob) {
	    if (pmodel == null) return;
	    if (pjob.pmodel() != pmodel) return;
	    if (! (pjob instanceof PModelRunJob)) return;
	    timer.stop();
	    Qra = Qrv = Qla = Qlv = null;
	}   

	// timer action performed
	public void actionPerformed(ActionEvent e) {
	    if (! figure.isShowing()) return;
	    try {
	    	Vra = getLatestVal(Qra);
	    	Vrv = getLatestVal(Qrv);
	    	Vla = getLatestVal(Qla);
	    	Vlv = getLatestVal(Qlv);
	    } catch (Xcept x) {
	    	System.err.println("Exception: " + x);
  	    }
	    figure.repaint();
//	    System.err.println("Vlv=" + Vlv);
	}

	// get latest value of model variable
	public double getLatestVal(ASVar v) throws Xcept {
	    if (v == null) return Double.NaN;
	    Data data = pmodel.rt().getData(0, v);
	    if (data == null || data.samples() == null) 
	        return Double.NaN;
	    double[] arr = data.samples();
	    int i = arr.length-1;
	    if (data.subset != null) i = data.subset.hix-1;
	    if (i<0) return Double.NaN;
	    return arr[i];
	}	

	// project content added/removed
	public void projectContentChanged() { 
	    // not relevant for MyGraphic
	}

	// ComponentListener events
	//    componentHidden and Shown don't seem to be called
	//    that's odd,  but resized works OK
	public void componentHidden(ComponentEvent e) { }
	public void componentShown(ComponentEvent e) { }
	public void componentMoved(ComponentEvent e) { }
	public void componentResized(ComponentEvent e) { 
	}

}

		    
