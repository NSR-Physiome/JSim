/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// horizontal/vertical labels

package JSim.gui;

import java.io.File;
import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

import JSim.util.*;

public class GNestedLabels extends JPanel {
	private StringList labels;
	private boolean isVert;
	
	// constructor
	public GNestedLabels(boolean isVert) {
	    super();
	    this.isVert = isVert;
	    labels = new StringList();
	    setBackground(Color.white);
	}

	// set labels
	public void setLabels(StringList labels) {
	    this.labels = labels;
	    repaint();
	}
	public void setLabels(String[] labels) {
	    setLabels(new StringList(labels));
	}
	
	// paint
	public void paintComponent(Graphics g) {
	    if (isVert)
	        paintVert((Graphics2D) g); 
	    else 
	    	paintHoriz((Graphics2D) g);
	}
	
	// paint horiz orient
	private void paintHoriz(Graphics2D g) {
	    Dimension dim = getSize();
	    FontMetrics fm = g.getFontMetrics();
 	    int w = dim.width;
	    int h = dim.height;
//	    g.clearRect(0, 0, w, h); causes black during printing
	    g.setColor(Color.white);
	    g.fillRect(0, 0, w, h);
	    g.setColor(Color.black);
	    int htext = fm.getAscent();
	    int n = labels.size();
	    int y = h/2 + htext/2;
	    for (int i=0; i<n; i++) {
	    	String label = labels.get(i);
		if (label == null) continue;
		int lw = fm.stringWidth(label);
		int x = i * w / n + w/n/2 - lw/2;
		g.drawString(label, x, y);
	    }	     
	}
	
	// paint vert orient
	private void paintVert(Graphics2D g) {
	    Dimension dim = getSize();
	    FontMetrics fm = g.getFontMetrics();
 	    int w = dim.width;
	    int h = dim.height;
//	    g.clearRect(0, 0, w, h); causes black during printing
	    g.setColor(Color.white);
	    g.fillRect(0, 0, w, h);
	    g.setColor(Color.black);
	    int htext = fm.getAscent();
	    int n = labels.size();
	    int x = w/2;
	    for (int i=0; i<n; i++) {
	    	String label = labels.str(i);
		if (label == null) continue;
		int lw = fm.stringWidth(label);
		int y = h - (i * h / n + h/n/2);
		g.translate(x, y);
		g.rotate(-Math.PI/2);
		g.drawString(label, -lw/2, htext/2);
		g.rotate(Math.PI/2);
		g.translate(-x, -y);
	    }	     	      
	}

	// get preferred size
	public Dimension getPreferredSize() {
	    int htext = getFontMetrics(getFont()).getAscent();
	    if (isVert) 
	    	return new Dimension(htext*4/3, 100);
  	    else
	        return new Dimension(100, htext*4/3);
	}
		
	// test harness
	public static void main(String[] args) throws Exception {
	    int i = 0;
	    boolean isVert = Util.toBoolean(args[i++]);
	    StringList labels = new StringList();
	    while (i<args.length) labels.add(args[i++]);
	    
	    JFrame frame = new JFrame("GNestedLabels");
	    GNestedLabels gn = new GNestedLabels(isVert);
	    frame.getRootPane().setContentPane(gn);
	    Dimension dim = isVert ? 
	        new Dimension(50, 300) : new Dimension(300,50);
	    gn.setPreferredSize(dim);
	    frame.pack();
	    frame.setVisible(true);
	    gn.setLabels(labels);
	}
}

	    
	    
