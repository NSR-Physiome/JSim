/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Config parms for plot page

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.jcomp.*;
import JSim.gui.graph.*;

public class GPlotConf extends GNode implements JSMinMaxSlider.Listener {

	// state
	public GPlotPage gpage;
	public PlotPage page;
	private JPanel lines;
	private GridLayout gridLayout;
	private Container line1, line2, line3, line4, line5;
	public GStringControl g_phase, g_expr;
	public GMenuAux g_exprAux, g_phaseAux;
	public GControl g_show;
	public GMenuControl g_style, g_plot,
	    g_shape, g_size, g_line, g_thickness, g_axis, 
	    g_src, g_item, g_color, g_colormap, g_palette, 
	    g_live;
	public GControl g_log, g_auto, g_min, g_max, g_scale;
	public JLabel l_min, l_max, l_phase, 
	    l_src, l_set;
	private JSMinMaxSlider g_minmax;
	private GAction showAnimation;
	private JButton b_animation;
	private GPlotAnime g_anime;

	// constructor
	public GPlotConf(GPlotPage p, PlotPage pa) {
	    super(p, pa);
	    gpage = p;
	    page = pa;

	    // Line 1: plot select style, update +
	    line1 = lineMgr();
	    g_plot = new GMenuControl(this, page.confPlot) {
		public String[] makeLabels() {
		    int n = plotpage().nPlot();
		    String[] labels = new String[n];
		    for (int i=0; i<labels.length; i++) 
			labels[i] = "plot " + (i+1);
		    return labels;
		}
	    };
	    g_plot.addAuxNode(gpage);
	    line1.add(g_plot.jcomp());
	    g_style = new GMenuControl(this, null) {
		public Control cntl() { 
		    return plotpage().confPlot().style;
		}
	    }; 
	    g_style.addAuxNode(gpage);
	    line1.add(g_style.jcomp());	    	    	    
	    g_live = new GMenuControl(this, page.liveUpdate) {
		public String[] makeLabels() {
		    return new String[] {
			"update during run", "update after run" };
		}
	    };
	    line1.add(g_live.jcomp());	    	    	    

	    // Line 2: data select, item#, hide
	    line2 = lineMgr();
	    l_src = new JLabel("Data ");
	    line2.add(l_src);
	    g_src = new GMenuControl(this, null) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().dataSrc;
		}
	    };
	    g_src.addAuxNode(gpage);
	    g_src.setPreferredWidth(10);
	    line2.add(g_src.jcomp());
	    g_expr = new GStringControl(this, null, 10) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().expr;
		}
	    };
	    g_expr.setValidBlack(true);
	    g_expr.addAuxNode(gpage);
	    line2.add(g_expr.jcomp());
	    g_exprAux = new GMenuAux(this, g_expr);
	    line2.add(g_exprAux.jcomp());
	    l_set = new JLabel(" Curve ");
	    line2.add(l_set);
	    g_item = new GMenuControl(this, null) {
		public String[] makeLabels() {
		    int n = plotpage().confPlot().nItem();
		    if (! plotpage().confPlot().hasBlankItems()) n++;
		    String[] labels = new String[n];
		    for (int i=0; i<labels.length; i++) 
			labels[i] = "" + (i+1);
		    return labels;
		}
		public Control cntl() {
		    return plotpage().confPlot().confItem;
		}
	    };
	    g_item.addAuxNode(this);
	    line2.add(g_item.jcomp());	    
	    line2.add(new JLabel(" show"));
	    g_show = new GBooleanControl(this, null) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().show;
		}
	    };
	    g_show.addAuxNode(gpage);
	    line2.add(g_show.jcomp());

	    // Line 3: phase expr, render
	    line3 = lineMgr();
	    l_phase = new JLabel("X-Expr ");
	    line3.add(l_phase);
	    g_phase = new GStringControl(this, null, 10) {
		public Control cntl() { 
		    return plotpage().confPlot().confItem().xExpr;
		}
	    }; 
	    g_phase.addAuxNode(gpage);
	    g_phase.setValidBlack(true);
	    line3.add(g_phase.jcomp());	    
	    g_phaseAux = new GMenuAux(this, g_phase);
	    line3.add(g_phaseAux.jcomp());
//	    makeIcons();	

	    g_line = new GPlotIconControl(this, null,
	    GPlotIconControl.LINE) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().line;
		}
	    };
	    g_line.addAuxNode(gpage);
	    line3.add(g_line.jcomp());
	    g_thickness = new GPlotIconControl(this, null, 
	    GPlotIconControl.THICK) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().thickness;
		}
	    };
	    g_thickness.addAuxNode(gpage);
	    line3.add(g_thickness.jcomp());
	    g_color = new GPlotIconControl(this, null, 
	    GPlotIconControl.COLOR) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().color;
		}
	    };
	    g_color.addAuxNode(gpage);
	    line3.add(g_color.jcomp());
	    g_shape = new GPlotIconControl(this, null, 
	    GPlotIconControl.SHAPE) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().shape;
		}
	    };
	    g_shape.addAuxNode(gpage);
	    line3.add(g_shape.jcomp());
	    g_size = new GPlotIconControl(this, null,
	    GPlotIconControl.SIZE) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().size;
		}
	    };
	    g_size.addAuxNode(gpage);
	    line3.add(g_size.jcomp());
	    g_colormap = new GMenuControl(this, null) {
		public String[] makeLabels() {
		    return new String[] { "no colormap", "area fill map", "raster map" };
		}
		public Control cntl() {
		    return plotpage().confPlot().confItem().colorMap;
		}
	    };
	    g_colormap.addAuxNode(gpage);
	    line3.add(g_colormap.jcomp());


	    g_palette = new GMenuControl(this, null) {
		public Control cntl() {
		    return plotpage().confPlot().confItem().palette;
		}
	    };
	    g_palette.addAuxNode(gpage);
	    line3.add(g_palette.jcomp());

	    // Line 4: axis select, log, autoscale, min/max
	    line4 = lineMgr();
	    g_axis = new GMenuControl(this, null) {
		public String[] makeLabels() {
		    if (plotpage().confPlot().style.val() == Plot.XY_PLOT) 
		    	return new String[] { "X", "Y" };
		    return new String[] { "X", "Y", "Z" };
		}
		public Control cntl() {
		    return plotpage().confPlot().confAxis;
		}
	    };
	    g_axis.addAuxNode(gpage);
	    line4.add(g_axis.jcomp());
	    line4.add(new JLabel(" Log"));
	    g_log = new GBooleanControl(this, null) {
		public Control cntl() { 
		    return plotpage().confPlot().confAxis().log;
		}
	    }; 
	    g_log.addAuxNode(gpage);
	    line4.add(g_log.jcomp());
	    line4.add(new JLabel(" Autoscale"));
	    g_auto = new GBooleanControl(this, null) {
		public Control cntl() { 
		    return plotpage().confPlot().confAxis().autoscale;
		}
	    };
	    g_auto.addAuxNode(gpage);
	    line4.add(g_auto.jcomp());	
	    l_min = new JLabel(" Min "); 
	    line4.add(l_min);
	    g_min = new GStringControl(this, null, 2) {
		public Control cntl() { 
		    return plotpage().confPlot().confAxis().min;
		}
	    };
	    g_min.addAuxNode(gpage);
	    line4.add(g_min.jcomp());
	    l_max = new JLabel(" Max ");	    
	    line4.add(l_max);
	    g_max = new GStringControl(this, null, 2) {
		public Control cntl() { 
		    return plotpage().confPlot().confAxis().max;
		}
	    };
	    g_max.addAuxNode(gpage);
	    line4.add(g_max.jcomp());	    

	    // Line 5: min/max slider & animation
	    line5 = lineMgr();
	    g_minmax = new JSMinMaxSlider();
	    g_minmax.setTailColor(glook().bright());
	    g_minmax.setContentColor(Color.white);
	    g_minmax.addMinMaxListener(this);
	    setHelp(g_minmax, "action$GPlotConf$axisBoundsSlider");
	    line5.add(g_minmax);
	    g_anime = new GPlotAnime(this);
	    showAnimation = new GAction(this, "Show Animation") {
		public void doit() {
		    g_anime.show();
		}
	    };
	    b_animation = showAnimation.button("Animate", false);
	    b_animation.setMargin(new Insets(0,0,0,0));
	    b_animation.setBackground(Color.white); 
	    line5.add(b_animation);

	    // all done
	    lines = new JPanel();
	    setJComp(lines);
	    reconfig();
	}

	// reconfig # lines if necessary
	public void reconfig() {
	    // test if really needed
	    int oldRows = (gridLayout == null) ? 
	    	0 : gridLayout.getRows();
	    boolean showSlider = page.showMinMaxSliders.val();
	    int newRows = showSlider ? 5 : 4;
	    if (oldRows == newRows) return;

	    // clear old lines
	    lines.removeAll();
	    lines.setLayout(new GridLayout(newRows, 1));
	    lines.add(line1);
	    lines.add(line2);
	    lines.add(line3);
	    lines.add(line4);
	    if (showSlider) lines.add(line5);
	}

	// refresh, # plot labels
	public void refresh() {
	    if (refreshing) return;

	    Plot plot = page.confPlot();
	    PlotAxis axis = plot.confAxis();
	    PlotItem item = plot.confItem();
	    boolean show = item.show.val();
	    boolean auto = plot.confAxis().autoscale.val(); 
	    boolean isXY = (plot.style.val() == Plot.XY_PLOT);

	    // 1st line
	    g_plot.resetLabels();
	    g_plot.jcomp().setVisible(page.nPlot()>1);
	    g_live.jcomp().setVisible(isXY);

	    // 2nd line
	    l_src.setEnabled(show);
	    g_src.jcomp().setEnabled(show);
	    g_src.resetLabels();
	    g_expr.jcomp().setEnabled(show);
	    g_item.resetLabels();

	    // 3rd line
	    l_phase.setEnabled(show);
	    l_phase.setVisible(isXY);
	    g_phase.jcomp().setEnabled(show);
	    g_phase.jcomp().setVisible(isXY);
	    g_phaseAux.jcomp().setVisible(isXY);
	    g_line.jcomp().setEnabled(show);
	    g_thickness.jcomp().setEnabled(show);
	    g_thickness.jcomp().setVisible(
		item.line.val() != GraphData.LINE_NONE);
	    boolean showColor = isXY || 	
		(item.line.val() != GraphData.LINE_NONE);
	    g_color.jcomp().setEnabled(show);
	    g_color.jcomp().setVisible(showColor);
	    g_shape.jcomp().setEnabled(show);
	    g_shape.jcomp().setVisible(isXY);
	    g_size.jcomp().setEnabled(show);
	    g_size.jcomp().setVisible(isXY && 
		item.shape.val() != GraphData.SHAPE_NONE);
	    g_colormap.jcomp().setVisible(!isXY);
	    boolean showPalette = !isXY && 
		(item.colorMap.val() != GraphData.COLORMAP_NONE);
	    g_palette.jcomp().setVisible(showPalette);

	    // 4th line
	    g_axis.resetLabels();
	    l_min.setEnabled(!auto);
	    g_min.jcomp().setEnabled(!auto);
	    l_max.setEnabled(!auto);
	    g_max.jcomp().setEnabled(!auto);

	    // 5th line
	    boolean showSlider = page.showMinMaxSliders.val();
	    refreshLive();
	    g_minmax.setMinMax(axis.min.val(), axis.max.val());
	    g_minmax.setVisible(showSlider);
	    g_minmax.setEnabled(!auto);
	    b_animation.setEnabled(!auto);

	    super.refresh();
	}

	// update g_minmax bounds
	public void refreshLive() {
	    GPlot gplot = gpage.graph().gplot(page.confPlot.val());
	    if (gplot == null) return;
	    int i = page.confPlot().confAxis.val();
	    double[] bounds = gplot.dataBounds(i);
	    if (bounds == null) 
		bounds = new double[] { Double.NaN, Double.NaN };
	    g_minmax.setBounds(bounds[0], bounds[1]);
	}	    
	     
	// line manager
	public Container lineMgr() {
	    return new Box(BoxLayout.X_AXIS);
//	    FlowLayout layout = new FlowLayout();
//	    layout.setHgap(0);
//	    layout.setVgap(0);
//	    return new JPanel(layout);
	}

	// min/max scroll events
	public void minmaxChanged(ComponentEvent e) {
	    PlotAxis axis = page.confPlot().confAxis();
	    try {
	    	axis.min.setVal(g_minmax.getMin());
	    	axis.max.setVal(g_minmax.getMax());
	    } catch (Xcept x) {
		gproject().message(x);
	    }
	    g_min.refresh();
	    g_max.refresh();
	    int i = page.confPlot.val();
	    GPlot gplot = gpage.graph().gplot(i);
	    if (gplot != null) 
	        gplot.refreshAxisBounds();
	}

	// animation update
	public void animationUpdate() {
	    g_min.refresh();
	    g_max.refresh();
	    PlotAxis axis = page.confPlot().confAxis();
	    g_minmax.setMinMax(axis.min.val(), axis.max.val());
	}

}
