/*NSRCOPYRIGHT
	Copyright (C) 1999-2014 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Plot graphical output

package JSim.gui;

import java.io.File;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import javax.swing.plaf.metal.*;
import javax.swing.plaf.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.*;
import java.util.ArrayList;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.graph.*;
import JSim.gui.gsgraph.*;

public class GPlotGraph extends GNode 
implements Printable, ChangeListener, ComponentListener {

	// state
	private PlotPage page;
	private JPanel panel;  // content panel
	private JLayeredPane layers;  // plot grid + X/Y rules
	private JPanel pgrid;  // plot grid
	private JSlider xRuleSlider, yRuleSlider; // rule sliders
	private JSSliderUI xRuleUI, yRuleUI; // slider UIs
	private JPanel xRule, yRule; // rules
	private GPlot.List gplots;
	private int nrow, ncol;
	private boolean showXRule, showYRule;

	// constant
	private final static int SLIDERMAX = 1000;
	private final Integer LOWER = new Integer(1);
	private final Integer UPPER = new Integer(2);

	// constructor
	public GPlotGraph(GNode p, PlotPage pa) {
	    super(p, null);
	    page = pa;
	    panel = new JPanel(new BorderLayout());
	    layers = new JLayeredPane();
	    pgrid = new JPanel();
	    layers.add(pgrid, LOWER);
	    xRuleSlider = new JSlider(JSlider.HORIZONTAL, 0, SLIDERMAX, 0);
	    xRuleUI = new JSSliderUI();
	    xRuleSlider.addChangeListener(this);
	    yRuleSlider = new JSlider(JSlider.VERTICAL, 0, SLIDERMAX, 0);
	    yRuleUI = new JSSliderUI();
	    yRuleSlider.addChangeListener(this);
	    if (glook().isMetal()) { // ??? needs work, bombs if shown
	    	xRuleSlider.setUI(xRuleUI);
	    	yRuleSlider.setUI(yRuleUI);
	    }
	    xRule = new JPanel();
	    xRule.setBackground(Color.blue);
	    layers.add(xRule, UPPER);
	    yRule = new JPanel();
	    yRule.setBackground(Color.blue);
	    layers.add(yRule, UPPER);
	    configRules();
	    moveRules();
	    setJComp(panel);
	    panel.addComponentListener(this);
	    layers.addComponentListener(this);
	}

	// configure X/Y rule sliders
	private void configRules() {
	    panel.removeAll();
	    panel.add(layers, BorderLayout.CENTER);    
	    showXRule = page.showXRule.val();
	    showYRule = page.showYRule.val();
	    if (showXRule) {
		panel.add(xRuleSlider, BorderLayout.SOUTH);
		int v = (int) (page.xRule.val() * SLIDERMAX);
		xRuleSlider.setValue(v);
	    }
	    if (showYRule) {
		panel.add(yRuleSlider, BorderLayout.EAST);
		int v = (int) (page.yRule.val() * SLIDERMAX);
		yRuleSlider.setValue(v);
	    }
	    pgrid.setSize(layers.getWidth(), layers.getHeight());
	    xRule.setVisible(showXRule);
	    yRule.setVisible(showYRule);
	}

	// reposition rules
	private void moveRules() {

	    // When saving proj file or changing UI color pref, the slider loses JSSliderUI and
	    // defaults to MetalSliderUI, Need to check:
	    if( xRuleSlider.getUI().getClass() != xRuleUI.getClass())
	    	xRuleSlider.setUI(xRuleUI);    
	    if( yRuleSlider.getUI().getClass() != yRuleUI.getClass())  
	    	yRuleSlider.setUI(yRuleUI);
	    
	    Rectangle r = layers.getBounds();
	    if (showXRule) {
		int x = xRuleUI.xCenter();
		xRule.setBounds(x, 0, 1, r.height);
	    }
	    if (showYRule) {
		int y = yRuleUI.yCenter();
		yRule.setBounds(0, y, r.width, 1);
	    }
	}

	// fill in plot grid
	private void configPGrid() {
	    pgrid.removeAll();
	    nrow = page.nRow.val();
	    ncol = page.nCol.val();
	    pgrid.setLayout(new GridLayout(nrow, ncol));
	    int ct = nrow*ncol;
	    gplots = new GPlot.List(ct);
	    for (int i=0; i<ct; i++) {
		GPlot gplot = new GPlot(this, page.plot(i));
		gplots.add(gplot);
		pgrid.add(gplot.render().jcomp());
	    }
	    pgrid.setSize(layers.getWidth(), layers.getHeight());
	}

	// refresh data
	public void liveUpdate() {
	    refresh(true);
	}
	public void refresh() { refresh(false); }
	public void refresh(boolean live) {
	    if (live && !page.liveUpdate.val()) return;
	    needsRefresh = false;
	    if (nrow != page.nRow.val() ||
	    	ncol != page.nCol.val()) {
		configPGrid();
		live = false;
	    }
	    if (showXRule != page.showXRule.val() ||
		showYRule != page.showYRule.val()) {
		configRules();
		live = false;
	    }
	    moveRules();
	    int ct = nrow*ncol;
	    for (int i=0; i<ct; i++) {
		GPlot gplot = gplots.gplot(i);
		try {
		    if (live)
		    	gplot.refreshLive();
		    else
		    	gplot.refreshAll();
		} catch (Exception e) {
		    warning("GPlotGraph: " + e);
		    e.printStackTrace();
		}
	    }
	}

	// symbol size for plots / icons
	public int plotSize(int i) {
	    double f = 0.4;
	    switch(i) {
	    case 0:  f = 0.275; break;
	    case 1:  f = 0.4; break;
	    case 2:  f = 0.55; break;
	    case 3:  f = 0.7; break;
	    }
	    return (int) (glook().fontSize() * f);
	} 

	// GraphRender for icon creation
	public GraphRender iconRender() {
	    if (gplots == null || gplots.size()<1) 
		return new GSGraph(null);
	    return gplots.gplot(0).render();
	}

	// print page
	public void print(PrinterJob pjob)
	throws Xcept {
	    pjob.setPrintable(this);
	    try {
		pjob.print();
	    } catch (PrinterException e) {
		throw new Xcept(e.toString());
	    }
	}

	// export Encapsulated Post-Script graphics
	public void exportEPS(File f) throws Xcept {
	    if (f.getName().indexOf('.') < 0)
		f = new File(f.toString() + ".eps");
	    int ct = nrow*ncol;
	    for (int i=0; i<ct; i++) {
		GraphRender render = gplots.gplot(i).render();
	        try {
		    render.exportEPS(f);
		    f = UtilIO.bump(f);
		} catch (Exception e) {
		    throw new Xcept(e.toString());
		}
	    }
	}

	// ChangeListener
	public void stateChanged(ChangeEvent e) {
	    JSlider js = null;
	    RealControl rule = null;	
	    if (e.getSource() == xRuleSlider) {
		js = xRuleSlider;
		rule = page.xRule;
		if (! page.showXRule.val()) return;
	    } else if (e.getSource() == yRuleSlider) {
		js = yRuleSlider;
		rule = page.yRule;
		if (! page.showYRule.val()) return;
	    } else 
		return;
	    int val = js.getValue();
	    double nval = (val+0.0)/SLIDERMAX;
	    try {
	    	if (rule.val() == nval) return;
	    	rule.setVal(nval);
	    } catch (Xcept x) {
		System.err.println(x.cleanMessage());
	    }
	    moveRules();
	}
		

	// Component Listener
	public void componentHidden(ComponentEvent e) { }
	public void componentMoved(ComponentEvent e) { }
	public void componentResized(ComponentEvent e) { 
	    Component c = e.getComponent();
	    if (c == panel) {
	        layers.revalidate();
	        pgrid.setSize(layers.getWidth(), layers.getHeight());
	    } else if (c == layers) {
	    	moveRules();
	    }
	}
	public void componentShown(ComponentEvent e) { }

	// print composite
	public int print(Graphics g, PageFormat pf, int inx) 
	throws PrinterException {
	    if (inx>0) return(Printable.NO_SUCH_PAGE);
	    int ct = nrow*ncol;
	    for (int i=0; i<ct; i++) {
		GraphRender render = gplots.gplot(i).render();
		render.printComposite(g, pf, inx, i, nrow, ncol);
            }
	    return(Printable.PAGE_EXISTS);
	}

	// slider UI
	public static class JSSliderUI extends MetalSliderUI {
	    public JSSliderUI() { super(); }
	    public int xCenter() {
	    	if (thumbRect == null) return 50;
		return thumbRect.x + thumbRect.width/2;
	    }
	    public int yCenter() {
	        if (thumbRect == null) return 10;
		return thumbRect.y + thumbRect.height/2;
	    }
	}

	// gplot query
	public GPlot gplot(int i) { return gplots.gplot(i); }
	    
}

