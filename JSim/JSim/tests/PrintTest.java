/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// print test harness

package JSim.tests;

import java.io.File;
import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

import JSim.util.*;
import JSim.gui.*;
import JSim.gui.graph.*;
import JSim.gui.gsgraph.*;

public class PrintTest implements Printable, MouseListener {
	JComponent content;

	// constructor
	public PrintTest() throws Exception {
	    JFrame frame = new JFrame("GNestedLabels");
	    content = makeContent4();
	    content.addMouseListener(this);
	    frame.setContentPane(content);
	    frame.setSize(300, 300);
	    frame.setVisible(true);
	}
	
	// make content: panel + label
	private JComponent makeContent1() throws Exception {
	    JPanel panel = new JPanel(new GridLayout(1,1));
	    panel.setBackground(Color.white);
	    panel.add(new JLabel("ABCDEFG"));
	    return panel;
	}

	// make content: label only
	private JComponent makeContent2() throws Exception {
	    return new JLabel("ABCDEFG");
	}

	// make content: label + GNestedLabels
	private JComponent makeContent3() throws Exception {
	    JPanel panel = new JPanel(new GridLayout(2,1));
	    panel.setBackground(Color.white);
	    panel.add(new JLabel("ABCDEFG"));
	    GNestedLabels gl = new GNestedLabels(false);
	    gl.setLabels(new String[] { "1", "2", "3", "4" });
	    panel.add(gl);
	    return panel;
	}

	// make content: label + graph
	private JComponent makeContent4() throws Exception {
	    JPanel panel = new JPanel(new GridLayout(2,1));
	    panel.setBackground(Color.white);
	    panel.add(new JLabel("ABCDEFG"));
	    panel.add(makeGraph());
	    return panel;
	}

	// make graph
	private JComponent makeGraph() throws Exception {
	    GSGraph g = new GSGraph(null);
	    GraphLayout l = new GraphLayout();
	    l.xaxis = new GraphLayout.Axis();
	    l.xaxis.min = 0;
	    l.xaxis.max = 6;
	    l.yaxis = new GraphLayout.Axis();
	    l.yaxis.min = -1;
	    l.yaxis.max = 1;
	    GraphData d = new GraphData();
	    d.label = "sin(x)";
	    int N = 100;
	    d.x = new double[N];
	    d.y = new double[N];
	    for (int i=0; i<N; i++) {
	    	d.x[i] = 6.0*i/N;
		d.y[i] = Math.sin(d.x[i]);
	    }
	    d.color = Color.black;
	    d.line = GraphData.LINE_NONE;
	    d.shape = GraphData.SHAPE_TRIANGLE;
	    d.size = 5;
	    l.data = new GraphData[] { d };
	    g.setGraphLayout(l, null);
	    return g.jcomp();
	}

	// printable implementation
	public int print(Graphics g, PageFormat pf, int inx) 
	throws PrinterException {
	    if (inx>0) return(Printable.NO_SUCH_PAGE);
	    Graphics2D g2 = (Graphics2D) g;
	    g2.translate(pf.getImageableX(), pf.getImageableY());
	    RepaintManager.currentManager(content).setDoubleBufferingEnabled(false);
	    content.print(g2);
	    RepaintManager.currentManager(content).setDoubleBufferingEnabled(true);
	    return(Printable.PAGE_EXISTS);
	}

	// printing selected
	private void doPrint() {
	    System.err.print("printing...\n");
	    PrinterJob pjob = PrinterJob.getPrinterJob();
	    pjob.setJobName("PrintTest");
	    pjob.setPrintable(this);
	    if (! pjob.printDialog()) { 
		System.err.println("Printing canceled");
		return;
	    }
	    try {
		pjob.print();
	    } catch (PrinterException e) {
		System.err.println("" + e);
	    }
	}	
	
	// Mouse listener
	public void mouseClicked(MouseEvent e) { 
	    doPrint();
	}
  	public void mouseEntered(MouseEvent e) { }
  	public void mouseExited(MouseEvent e) { }
  	public void mousePressed(MouseEvent e) { }
 	public void mouseReleased(MouseEvent e) { }
	
	// mainline
	public static void main(String[] args) throws Exception {
	   new PrintTest();
	}
}
