/*NSRCOPYRIGHT
	Copyright (C) 1999-2014 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Histogram JComponent

package JSim.gui.gsgraph;

import java.awt.*;
import java.awt.print.*;
import javax.swing.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSHistogram extends JPanel implements Printable {
	private HistLayout layout;
	private int[] bins;
	private int binMax;
	private int binsTot;
	
	// constructor
	public GSHistogram() {
	    super();
	    setBackground(Color.white);
	    setForeground(Color.black);
	}
	
	// set Layout
	public void setLayout(HistLayout layout) {
	    this.layout = layout;
	    bins = null;
	    repaint();
	}	

	// paint Component graphics
	public void paintComponent(Graphics g) {
	    super.paintComponent(g);
	    if (layout == null) return;
	    if (bins == null) loadBins();
	    FontMetrics fm = g.getFontMetrics();

	    // layout parms
	    int w = getWidth();
	    int h = getHeight();
	    int lm = w / 30;
	    int rm = lm;
	    int tm = h/30;
	    int bm = tm;
	    int htext = fm.getAscent();
	    int htic = htext/2;

	    // adjust left-margin if binMax large
	    int wbm = fm.stringWidth("0" + binMax);
	    lm = Math.max(lm, wbm);

	    // title
	    if (layout.title != null)
	    	g.drawString(layout.title, lm, 2*htext);

	    // bounding box 
	    int yceil = tm + 2*htext;
	    int ybase = h - bm - 2*htext;
	    int wbase = w - lm - rm;
	    int hbox = ybase - yceil;
	    g.drawRect(lm, yceil, wbase, hbox);

	    // tics
	    for (int i=0; i<=nbins(); i++) {
	    	int xtic = lm + (int) (wbase * i / nbins());
	    	g.drawLine(xtic, ybase, xtic, ybase+htic);
	    	g.drawLine(xtic, ybase, xtic, ybase+htic);
	    }

	    // extrema
	    int yext = h - bm;
	    String s = Util.pretty(layout.sampleMin);
	    g.drawString(s, lm, yext);
	    s = Util.pretty(layout.sampleMax);
	    int ws = fm.stringWidth(s);
	    g.drawString(s, w-rm-ws, yext);

	    // #opts label
	    int yct = yceil - htext/2;
	    s = "#opts=" + binsTot;
	    ws = fm.stringWidth(s);
	    g.drawString(s, w-rm-ws, yct);

	    // vertical scale min/max labels
	    s = "" + binMax;
	    ws = fm.stringWidth(s);
	    g.drawString(s, (lm-ws)/2, yceil + htext/2);
	    s = "0";
	    ws = fm.stringWidth(s);
	    g.drawString(s, (lm-ws)/2, ybase);
	    
	    // draw bin bars
	    g.setColor(layout.barColor);
	    int wbin = wbase / bins.length - layout.barSpacing;
	    for (int i=0; i<bins.length; i++) {
	    	int xbin = lm + layout.barSpacing + (int) (i * wbase/bins.length);
		int hbin = bins[i] * (hbox-1) / binMax; 
		g.fillRect(xbin, ybase-hbin, wbin, hbin);
	    }
	}

	// load bins
	private void loadBins() {
	    bins = new int[nbins()];
	    binMax = binsTot = 0;
	    double srange = layout.sampleMax - layout.sampleMin;
	    if (srange == 0) srange = 1;
	    for (int i=0; i<layout.samples.length; i++) {
	    	double s = layout.samples[i];
		if (s<layout.sampleMin) continue;
		if (s>layout.sampleMax) continue;
		if (Double.isNaN(s)) continue;
		int bin = (int) 
		    (nbins() * (s - layout.sampleMin) / srange);  
		if (bin < 0) bin = 0;
		if (bin >= nbins()) bin = nbins()-1;
		bins[bin]++;
		binMax = Math.max(binMax, bins[bin]);
		binsTot++;
	    }	
	}	    

	// query
	private int nbins() {
	    if (layout == null) return 10;
	    return Math.max(1, layout.nbins);
	}    

	// print
	public int print(Graphics g, PageFormat pf, 
	int pageIndex) {
	    if (pageIndex > 0) return Printable.NO_SUCH_PAGE;
	    Graphics2D g2d = (Graphics2D) g;
	    g2d.translate(pf.getImageableX(), pf.getImageableY());
	    g2d.scale(0.75,0.75); // Scale to 75% to fit on page for printout
	    print(g2d);           // Print scaled histogram instead of original
	    return Printable.PAGE_EXISTS;
	}
}

