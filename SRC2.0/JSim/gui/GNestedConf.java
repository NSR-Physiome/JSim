/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Config parms for nested plot

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.jcomp.*;
import JSim.gui.graph.*;

public class GNestedConf extends GNode {

	// state
	public GNested gnested;
	public PNested pnested;
	private JPanel lines;
	private GridLayout gridLayout;

	private Box styleLine;
	public GMenuControl g_style, g_scaling;
	public JLabel l_x2subAxes, l_y2subAxes;
	public GStringControl g_x2subAxes, g_y2subAxes;
	public JLabel l_filler;

	private Box itemLine;
	public JLabel l_src;
	public GMenuControl g_src;
	public GStringControl g_expr;
	public GMenuAux g_exprAux;
	public GMenuControl g_item;
	public GBooleanControl g_show;
	
	private Box renderLine;
//	public JLabel l_phase;
//	public GStringControl g_phase;  not currently implemented
//	public GMenuAux g_phaseAux;
	public GMenuControl g_shape, g_size, g_line, g_thickness, 
	    g_color, g_colormap, g_palette;
	private Icon[] shapeIcons, sizeIcons, lineIcons, thickIcons,
	    colorIcons;

	private ArrayList<GSlider> gsliders;
	private Hashtable<String, GAxis> gaxes;
	private ArrayList<JComponent> boxLines;
	    
	// constructor
	public GNestedConf(GNested g, PNested p) {
	    super(g, p);
	    gnested = g;
	    pnested = p;

	    // Line 1: style, scaling, X/Y nesting, #subaxes
	    styleLine =  new Box(BoxLayout.X_AXIS);
	    g_style = new GMenuControl(this, pnested.style); 
	    g_style.addAuxNode(gnested);
	    styleLine.add(g_style.jcomp());	    	    	    

	    g_scaling = new GMenuControl(this, pnested.scaling) {
		public String[] makeLabels() {
		    return new String[] {
		    	"global scaling",
			"page scaling",
			"local scaling" };
		}
	    };
	    g_scaling.addAuxNode(gnested);
	    styleLine.add(g_scaling.jcomp());	    	    	    

	    l_x2subAxes = new JLabel("#X subaxes");
	    styleLine.add(l_x2subAxes);
	    g_x2subAxes = new GStringControl(this, 
	    	pnested.globalAxis(PNestedAxis.X2).nsubAxes, 2);
	    g_x2subAxes.addAuxNode(gnested);
	    styleLine.add(g_x2subAxes.jcomp());

	    l_y2subAxes = new JLabel("#Y subaxes");
	    styleLine.add(l_y2subAxes);
	    g_y2subAxes = new GStringControl(this, 
	    	pnested.globalAxis(PNestedAxis.Y2).nsubAxes, 2);
	    g_y2subAxes.addAuxNode(gnested);
	    styleLine.add(g_y2subAxes.jcomp());

	    l_filler = new JLabel(".                             .");
	    styleLine.add(l_filler);

	    // Line 2: data select, item#, hide
	    itemLine = new Box(BoxLayout.X_AXIS);

	    l_src = new JLabel("Data ");
	    itemLine.add(l_src);
	    g_src = new GMenuControl(this, null) {
		public Control cntl() {
		    return confItem().dataSrc;
		}
	    };
	    g_src.addAuxNode(gnested);
	    g_src.setPreferredWidth(10);
	    itemLine.add(g_src.jcomp());

	    g_expr = new GStringControl(this, null, 10) {
		public Control cntl() {
		    return confItem().expr;
		}
	    };
	    g_expr.setValidBlack(true);
	    g_expr.addAuxNode(gnested);
	    itemLine.add(g_expr.jcomp());
	    g_exprAux = new GMenuAux(this, g_expr);
	    itemLine.add(g_exprAux.jcomp());

	    itemLine.add(new JLabel(" Curve "));
	    g_item = new GMenuControl(this, null) {
		public String[] makeLabels() {
		    int n = pnested.nItems.val();
		    if (! pnested.hasBlankItems()) n++;
		    String[] labels = new String[n];
		    for (int i=0; i<labels.length; i++) 
			labels[i] = "" + (i+1);
		    return labels;
		}
		public Control cntl() {
		    return pnested.confItem;
		}
	    };
	    g_item.addAuxNode(this);
	    itemLine.add(g_item.jcomp());	    

	    itemLine.add(new JLabel(" show"));
	    g_show = new GBooleanControl(this, null) {
		public Control cntl() {
		    return confItem().show;
		}
	    };
	    g_show.addAuxNode(gnested);
	    itemLine.add(g_show.jcomp());

	    // Line 3: phase expr, render options
	    renderLine = new Box(BoxLayout.X_AXIS);
	    
	    // phase not currently implemented
/*	    l_phase = new JLabel("X-Expr ");
	    renderLine.add(l_phase);
	    g_phase = new GStringControl(this, null, 10) {
		public Control cntl() { 
		    return confItem().xExpr;
		}
	    }; 
	    g_phase.addAuxNode(gnested);
	    g_phase.setValidBlack(true);
	    renderLine.add(g_phase.jcomp());	    
	    g_phaseAux = new GMenuAux(this, g_phase);
	    renderLine.add(g_phaseAux.jcomp());
*/
	    // makeIcons();	

	    g_line = new GPlotIconControl(this, null, 
	    GPlotIconControl.LINE) {
		public Control cntl() {
		    return confItem().line;
		}
	    };
	    g_line.addAuxNode(gnested);
	    renderLine.add(g_line.jcomp());

	    g_thickness = new GPlotIconControl(this, null, 
	    GPlotIconControl.THICK) {
		public Control cntl() {
		    return confItem().thickness;
		}
	    };
	    g_thickness.addAuxNode(gnested);
	    renderLine.add(g_thickness.jcomp());

	    g_color = new GPlotIconControl(this, null, 
	    GPlotIconControl.COLOR) {
		public Control cntl() {
		    return confItem().color;
		}
	    };
	    g_color.addAuxNode(gnested);
	    renderLine.add(g_color.jcomp());

	    g_shape = new GPlotIconControl(this, null, 
	    GPlotIconControl.SHAPE) {
		public Control cntl() {
		    return confItem().shape;
		}
	    };
	    g_shape.addAuxNode(gnested);
	    renderLine.add(g_shape.jcomp());

	    g_size = new GPlotIconControl(this, null, 
	    GPlotIconControl.SIZE) {
		public Control cntl() {
		    return confItem().size;
		}
	    };
	    g_size.addAuxNode(gnested);
	    renderLine.add(g_size.jcomp());

	    g_colormap = new GMenuControl(this, null) {
		public String[] makeLabels() {
		    return new String[] { "no colormap", "area fill map", "raster map" };
		}
		public Control cntl() {
		    return confItem().colorMap;
		}
	    };
	    g_colormap.addAuxNode(gnested);
	    renderLine.add(g_colormap.jcomp());

	    g_palette = new GMenuControl(this, null) {
		public Control cntl() {
		    return confItem().palette;
		}
	    };
	    g_palette.addAuxNode(gnested);
	    renderLine.add(g_palette.jcomp());

	    gsliders = new ArrayList<GSlider>();
	    gaxes = new Hashtable<String, GAxis>();
	    for (int i=0; i<PNestedAxis.ALLIDS.length; i++) {
	    	String id = PNestedAxis.ALLIDS[i];
		PNestedAxis.Global paxis = pnested.globalAxis(id);
		gaxes.put(id, new GAxis(this, paxis));
	    }

	    // all done
	    lines = new JPanel();
	    setJComp(lines);
	    boxLines = new ArrayList<JComponent>();
	    reconfig();
	}

	// reconfig # lines if necessary
	public void reconfig() {
	    ArrayList<JComponent> newLines = new ArrayList<JComponent>();
	    newLines.add(styleLine);
	    newLines.add(itemLine);
	    newLines.add(renderLine);
	    int ct = 3;
	    for (int i=0; i<PNestedAxis.ALLIDS.length; i++) {
		String id = PNestedAxis.ALLIDS[i];
	    	GAxis gaxis = gaxes.get(id);
		if (! gaxis.isActive()) continue;
		newLines.add(gaxis.line1);
		ct++;
	    }

	    // add sliders?
	    PNestedData data = gnested.getData();
	    if (data != null) {
	    	ArrayList<PNestedData.Slider> psliders = data.getSliders();
		for (int i=0; i<psliders.size(); i++) {
		    PNestedData.Slider pslider = psliders.get(i);
		    if (i>=gsliders.size()) 
		    	gsliders.add(new GSlider(this));
		    GSlider gslider = gsliders.get(i);
		    gslider.load(pslider);
		    newLines.add(gslider.jcomp());
		    ct++;
		}
	    }

	    // if changed, update
	    if (boxLines.equals(newLines)) return;
	    lines.removeAll();
	    for (int i=0; i<ct; i++) 
	    	lines.add(newLines.get(i));
	    lines.setLayout(new GridLayout(ct, 1));
	    boxLines = newLines;
	}

	// refresh, # nested labels
	public void refresh() {
	    if (refreshing) return;

	    PNestedItem item = confItem();
	    boolean showItem = item.show.val();
	    boolean isXY = (pnested.style.val() == PNested.XY_PLOT);

	    // style line
	    l_x2subAxes.setVisible(pnested.xnesting.val());
	    g_x2subAxes.jcomp().setVisible(pnested.xnesting.val());
	    l_y2subAxes.setVisible(pnested.ynesting.val());
	    g_y2subAxes.jcomp().setVisible(pnested.ynesting.val());

	    // item line
	    l_src.setEnabled(showItem);
	    g_src.jcomp().setEnabled(showItem);
	    g_src.resetLabels();
	    g_expr.jcomp().setEnabled(showItem);
	    g_item.resetLabels();

	    // render line

	    // phase not currently implemented
//	    l_phase.setEnabled(showItem);
//	    l_phase.setVisible(isXY);
//	    g_phase.jcomp().setEnabled(showItem);
//	    g_phase.jcomp().setVisible(isXY);
//	    g_phaseAux.jcomp().setVisible(isXY);

	    g_line.jcomp().setEnabled(showItem);
	    g_thickness.jcomp().setEnabled(showItem);
	    g_thickness.jcomp().setVisible(
		item.line.val() != GraphData.LINE_NONE);
	    boolean showItemColor = isXY || 	
		(item.line.val() != GraphData.LINE_NONE);
	    g_color.jcomp().setEnabled(showItem);
	    g_color.jcomp().setVisible(showItemColor);
	    g_shape.jcomp().setEnabled(showItem);
	    g_shape.jcomp().setVisible(isXY);
	    g_size.jcomp().setEnabled(showItem);
	    g_size.jcomp().setVisible(isXY &&
	        (item.shape.val() != GraphData.SHAPE_NONE));
	    g_colormap.jcomp().setVisible(!isXY);
	    boolean showPalette = !isXY && 
		(item.colorMap.val() != GraphData.COLORMAP_NONE);
	    g_palette.jcomp().setVisible(showPalette);

	    super.refresh();
	}

	// simple query
	public PNestedItem confItem() { return pnested.confItem(); }

	// one Slider
	public class GSlider extends GNode 
	implements ChangeListener, KeyListener, FocusListener {
	    public Box box;
	    public JLabel l_name;
	    public JTextField jtext;
	    public JSlider jslider;
	    public PNestedData.Slider pslider;
	    private boolean textChanged;	    

	    // constructor
	    public GSlider(GNestedConf g) {
	    	super(g, null);
		box = new Box(BoxLayout.X_AXIS);
		l_name = new JLabel("name");
		jtext = new JTextField(10);
		jtext.addKeyListener(this);
		jtext.addFocusListener(this);
		jslider = new JSlider(JSlider.HORIZONTAL);
		jslider.setMinimum(0);
		jslider.setPaintTicks(true);
		jslider.setMinorTickSpacing(1);
		jslider.setSnapToTicks(true);
		jslider.addChangeListener(this);
		box.add(l_name);
		box.add(jtext);
		box.add(jslider);
		setJComp(box);
	    }
	    
	    // load info
	    public void load(PNestedData.Slider s) {
		pslider = s;
	    	l_name.setText(pslider.getName());
		double value = pslider.getValue();
		jtext.setText(Util.pretty(value));
		double[] grid = pslider.getGrid();
		jslider.setMaximum(grid.length-1);
		double gmin = grid[0];
		double gmax = grid[grid.length-1];
		double gf = (value-gmin)/(gmax-gmin);
		int gv = (int) Math.round((grid.length-1)*gf);
		jslider.setValue(gv);
	    }

	    // state changed via slider
	    public void stateChanged(ChangeEvent event) {
	        int ginx = jslider.getValue();
		double[] grid = pslider.getGrid();
		if (grid == null) return;
		if (ginx < 0 || ginx >= grid.length) return;
	    	double v = grid[jslider.getValue()];
		if (v == pslider.getValue()) return;
		jtext.setText(Util.pretty(v));
		textChanged = false;
		try {
		    pslider.updateValue(v);
		    gnested.refreshData();
		} catch (Exception e) {
		    gnested.setStatus(e);
		}
	    }

	    // key callbacks
	    public void keyTyped(KeyEvent event) {
	    	if (event.getKeyChar() != '\n') {
		    textChanged = true;
		    return;
		}
		if (textChanged) updateFromText();
	    }
	    public void keyPressed(KeyEvent e) { }
	    public void keyReleased(KeyEvent e) { }

	    // focus callbacks
            public void focusGained(FocusEvent e) { 
	    	textChanged = false;
	    }
            public void focusLost(FocusEvent e) {
            	if (textChanged) updateFromText();
            }
	    
	    // update from text value
	    private void updateFromText() {	    
		double v = Util.toDouble(jtext.getText());
		if (Double.isNaN(v)) return;
		try {
		    pslider.updateValue(v);
		    gnested.refreshData();
		} catch (Exception e) {
		    gnested.setStatus(e);
		}
	    }	    

	    // unused listener callbacks
	}

	// one Axis controls
	public class GAxis extends GNode {
	    public Box line1;
	    public JLabel l_id;
	    public GMenuControl g_domain, g_sampling;
	    public JLabel l_log, l_autoscale, l_min, l_max;
	    public GBooleanControl g_log, g_autoscale;
	    public GStringControl g_min, g_max, g_sampleList;
	
	    // constructor
	    public GAxis(GNestedConf g, PNestedAxis.Global p) {
	    	super(g, p);
		line1 = new Box(BoxLayout.X_AXIS);

		l_id = new JLabel(idLabel() + ": ");
		line1.add(l_id);

		g_domain = new GMenuControl(this, null) {
		    public Control cntl() { 
		    	return localAxis().domain;
		    }
		};
		g_domain.addAuxNode(gnested);
		line1.add(g_domain.jcomp());

		g_sampling = new GMenuControl(this, null) {
		    public Control cntl() { 
		    	return localAxis().sampleMethod;
		    }
		    public String[] makeLabels() {
			return new String[] { "linear samples",
			    "log samples", "list samples" };
		    }
		};
		g_sampling.addAuxNode(gnested);
		line1.add(g_sampling.jcomp());
		
		g_sampleList = new GStringControl(this, null, 2) {
		    public Control cntl() { 
		    	return localAxis().sampleList;
		    }
		}; 
		g_sampleList.addAuxNode(gnested);		
		line1.add(g_sampleList.jcomp());

	    	l_log = new JLabel(" Log");    
	    	line1.add(l_log);
	    	g_log = new GBooleanControl(this, null) {
		    public Control cntl() { 
		        return globalAxis().log;
		    }
	    	}; 
	    	g_log.addAuxNode(gnested);
		line1.add(g_log.jcomp());
		
	    	l_autoscale = new JLabel(" Autoscale");    
	    	line1.add(l_autoscale);
	    	g_autoscale = new GBooleanControl(this, null) {
		    public Control cntl() { 
		        return autoscaleAxis().autoscale;
		    }
	    	}; 
	    	g_autoscale.addAuxNode(gnested);
		line1.add(g_autoscale.jcomp());
 
	    	l_min = new JLabel(" Min "); 
	    	line1.add(l_min);
	    	g_min = new GStringControl(this, null, 2) {
		    public Control cntl() { 
		    	return autoscaleAxis().dataMin;
		    }
	    	};
	    	g_min.addAuxNode(gnested);
	    	line1.add(g_min.jcomp());

	    	l_max = new JLabel(" Max "); 
	    	line1.add(l_max);
	    	g_max = new GStringControl(this, null, 2) {
		    public Control cntl() { 
		    	return autoscaleAxis().dataMax;
		    }
	    	};
	    	g_max.addAuxNode(gnested);
	    	line1.add(g_max.jcomp());
	    }

	    // refresh
	    public void refresh() {
		if (! isActive()) return;
	    	PNestedAxis.Global gaxis = globalAxis();
		PNestedAxis.Local laxis = localAxis();
		boolean showDomain = gaxis.isIndependent();
		boolean showSampling = gaxis.isSparse();
		boolean showSampleList = showSampling && 
		    (laxis.sampleMethod.val() == laxis.LIST);
		boolean showAutoscale = gaxis.isInner() || 
		    !showSampleList;
		boolean enableMinMax = showAutoscale 
		    && !autoscaleAxis().autoscale.val();
		boolean showLog = gaxis.isInner();

		g_domain.jcomp().setVisible(showDomain);
		if (showDomain) g_domain.resetLabels();
		g_sampling.jcomp().setVisible(showSampling);
		g_sampleList.jcomp().setVisible(showSampleList);
		l_log.setVisible(showLog);
		g_log.jcomp().setVisible(showLog);
		l_autoscale.setVisible(showAutoscale);
		g_autoscale.jcomp().setVisible(showAutoscale);
		l_min.setVisible(showAutoscale);
		g_min.jcomp().setVisible(showAutoscale);
		g_min.jcomp().setEnabled(enableMinMax);
		l_max.setVisible(showAutoscale);
		g_max.jcomp().setVisible(showAutoscale);
		g_max.jcomp().setEnabled(enableMinMax);
		
		super.refresh();
	    }

	    // query
	    public String id() { return globalAxis().id(); }
	    public String idLabel() { 
	    	String l = id().toUpperCase();
		if (! globalAxis().isAttr()) l = l + " Axis";
		return l;
	    }
	    public PNestedAxis.Global globalAxis() { return (PNestedAxis.Global) pnamed(); }
	    public PNestedAxis.Local localAxis() { return confItem().localAxis(id()); }
	    public PNestedAxis autoscaleAxis() {
	    	return globalAxis().isSparse() ? localAxis() : globalAxis();
	    }
	    public boolean isActive() { return globalAxis().isActive(); }
	}
}
