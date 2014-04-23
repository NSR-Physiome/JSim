/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// real-time graph update debug harness

package JSim.tests; import JSim.gui.demograph.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.print.*;
import javax.swing.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class TestRT extends JFrame {

	// static 
	public static final int XMAX = 10;
	public static final int GRIDMAX = 100;
	public static final int CLUMP = 4;

	// mainline
	public static void main(String[] args)
	throws Exception {
	    // parse args
	    boolean testGraph = false;
	    if (args.length > 0 && args[0].equals("-testGraph"))
		testGraph = true;

	    // launch app
	    GraphRender render;
	    render = new DemoGraph(null);
	    TestRT frame = new TestRT(render.jcomp());
	    GraphLayout layout = initLayout();
	    render.setGraphLayout(layout, null);

	    // update graph data
	    GraphData.Subset subset = new GraphData.Subset();
	    GraphData.Subset[] subsets = 
		new GraphData.Subset[] { subset, subset };
	    for (int i=0; i<GRIDMAX/CLUMP; i++) {
		try { Thread.sleep(500); }
		    catch (Exception e) { }
		System.err.println("update " + i);
		layout.title = "should not see this " + i;
		for (int j=0; j<CLUMP; j++) {
		    int k = CLUMP*i + j;
		    layout.data[0].y[k] = Math.sin(x(k));
		    layout.data[1].y[k] = Math.cos(x(k));
		    subset.lox = k-j;
		    subset.hix = k+1;
		}
	        render.setGraphLayout(layout, subsets);
	    }

	}

	// constructor
	public TestRT(JComponent graph) throws Exception {
	    super("TestRT");

	    // add stuff
	    JRootPane root = getRootPane();
	    graph.setPreferredSize(new Dimension(900,400));
	    root.getContentPane().add(graph);
	    	
	    // show it and wait
	    pack();
	    setVisible(true);
	}

	public static double x(int i) {
	    return i*XMAX*1.0/GRIDMAX;
	}

	public static GraphLayout initLayout() {
	    GraphLayout l = new GraphLayout();
	    l.title = "run-time test";
	    l.style = GraphLayout.STYLE_2DLINE;
	    l.fontSize = 12;
	    l.xaxis = new  GraphLayout.Axis();
	    l.xaxis.min = 0;
	    l.xaxis.max = XMAX;
	    l.yaxis = new  GraphLayout.Axis();
	    l.yaxis.min = -1.5;
	    l.yaxis.max = 1.5;
	    l.titleX = 0.5;
	    l.titleY = 0.1;
	    l.showLegend = true;
	    l.legendX = 0.75;
	    l.legendY = 0.1;
	    GraphData dsin = new GraphData();
	    dsin.label = "sin(x)";
	    dsin.x = new double[GRIDMAX];
	    dsin.y = new double[GRIDMAX];
	    dsin.color = Color.red;
	    dsin.shape = GraphData.SHAPE_SQUARE;
	    dsin.size = 6;
	    dsin.line = GraphData.LINE_NONE;
	    GraphData dcos = new GraphData();
	    dcos.label = "cos(x)";
	    dcos.x = new double[GRIDMAX];
	    dcos.y = new double[GRIDMAX];
	    dcos.color = Color.blue;
	    dcos.shape = GraphData.SHAPE_SQUARE;
	    dcos.size = 6;
	    dcos.line = GraphData.LINE_SOLID;
	    for (int i=0; i<GRIDMAX; i++) {
		dsin.x[i] = dcos.x[i] = x(i);
		dsin.y[i] = dcos.y[i] = Double.NaN;
	    }
	    l.data = new GraphData[] { dsin, dcos };
	    return l;
	}

}

