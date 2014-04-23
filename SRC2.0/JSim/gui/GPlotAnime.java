/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Plot page animation control

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;

public class GPlotAnime extends GNode implements ActionListener {

	// perm state
	private GPlotPage gpage; // dialog for this gpage
	private PlotPage page; // dialog for this page
	private int state; // see constants below
	private Timer timer; // timer event generator

	// states
	public static final int IDLE = 0;
	public static final int RUNNING = 1;
	public static final int PAUSED = 2;

	// single animation state
	private GPlot gplot; // plot to update
	private Plot plot; // plot to update
	private PlotAxis paxis; // axis to animate
	private double datamin; // starting data min
	private double datamax; // starting data max
	private double axisrange; // size of axis
	private double fpos; // current anime position (0-1)
	private double fdelta; // 1-step delta for fpos

	// sub-components
	private JLabel l_duration, l_rate, l_dir, l_loop;
	private GControl g_duration, g_rate, g_dir, g_loop;
	private JPanel panel;
	private JButton b_start, b_stop;
	private Window window;
	private GAction a_start, a_stop;

	// constructor
	public GPlotAnime(GPlotConf g) {
	    super(g, null);
	    gpage = g.gpage;
	    page = gpage.page();
	    state = IDLE;

	    // content panel
	    panel = new JPanel(new GridLayout(6, 2));
	    Border border = new LineBorder(Color.black, 2);
	    panel.setBorder(border);
	    ghelp().registerKeystrokes(panel);

	    // title line
	    JLabel l = new JLabel("Animation");
	    l.setFont(glook().bigFont());
	    l.setHorizontalAlignment(JLabel.CENTER);
	    panel.add(l);
	    l = new JLabel("Control");
	    l.setFont(glook().bigFont());
	    l.setHorizontalAlignment(JLabel.CENTER);
	    panel.add(l);

	    // duration line
	    l_duration = new JLabel("Duration (sec)");
	    panel.add(l_duration);
	    g_duration = new GStringControl(this, 
		page.animeDuration, 6);
	    panel.add(g_duration.jcomp());

	    // rate line
	    l_rate = new JLabel("Frames/sec");
	    panel.add(l_rate);
	    g_rate = new GStringControl(this, 
		page.animeRate, 4);
	    panel.add(g_rate.jcomp());

	    // direction line
	    l_dir = new JLabel("Direction");
	    panel.add(l_dir);
	    g_dir = new GMenuControl(this, page.animeFwd) {
		public String[] makeLabels() {
		    return new String[] { "forward", "backward" };
		}
	    }; 
	    panel.add(g_dir.jcomp());

	    // loop line
	    l_loop = (new JLabel("Loop"));
	    panel.add(l_loop);
	    g_loop = new GBooleanControl(this, page.animeLoop);
	    ((JCheckBox) g_loop.jcomp()).setHorizontalAlignment(
		SwingConstants.CENTER);
	    panel.add(g_loop.jcomp());

	    // start button
	    a_start = new GAction(this, "Start") {
		public void doit() throws Xcept {
		    switch (state) {
		    case IDLE: start(); break;
		    case RUNNING: pause(); break;
		    case PAUSED: resume(); break;
		    }
		}
	    };
	    b_start = a_start.button(null, false);
	    b_start.setBackground(Color.white);
	    panel.add(b_start);

	    // stop button
	    a_stop = new GAction(this, "Stop") {
		public void doit() throws Xcept {
		    if (state == IDLE) 
			hide();
		    else
			stop();
		}
	    };
	    b_stop = a_stop.button(null, false);
	    b_stop.setBackground(Color.white);
	    panel.add(b_stop);

	    // window
	    Window pwin =
		SwingUtilities.getWindowAncestor(gproject().jcomp());
	    window = new Window(pwin);
	    window.add(panel);
	}

	// refresh
	public void refresh() {
	    super.refresh();

	    // labels & controls
	    boolean e = (state == IDLE);
	    l_duration.setEnabled(e);
	    g_duration.jcomp().setEnabled(e);
	    l_rate.setEnabled(e);
	    g_rate.jcomp().setEnabled(e);

	    // start button
	    b_start.setEnabled(true);
	    switch (state) {
	    case IDLE: b_start.setText("Start"); break;
	    case RUNNING: b_start.setText("Pause"); break;
	    case PAUSED: b_start.setText("Resume"); break;
	    }

	    // stop button
	    b_stop.setEnabled(true);
	    b_stop.setText((state == IDLE) ?
		"Cancel" : "Stop");
	}	    

	// hide popups if window hidden
	public void hidePopups() {
	    hide();
	    super.hidePopups();
	}

	// show window
	public void show() {
	    window.setSize(panel.getPreferredSize());
	    window.setLocationRelativeTo(
		gproject().lefttabs().jcomp());
	    window.setVisible(true);
	}

	// start animation
	private void start() throws Xcept {

	    // fraction position
	    plot = page.confPlot();
	    if (page.animeRate.val() <= 0) throw new Xcept(page,
		"Animation frame rate must be positive.");
	    if (page.animeDuration.val() <= 0) throw new Xcept(page,
		"Animation duration must be positive.");
	    fdelta = 1 / (page.animeDuration.val() * page.animeRate.val());
	    fpos = page.animeFwd.val() ? 0 : 1;

	    // axismin/max
	    paxis = plot.confAxis();
	    if (paxis.autoscale.val()) throw new Xcept(page, 
		"Animation requires autoscale off");
	    axisrange = paxis.max.val() - paxis.min.val();
	    if (! (axisrange>0)) throw new Xcept(page,
		"Invalid axis min/max for animation.");

	    // data bounds
	    gplot = gpage.graph().gplot(page.confPlot.val());
	    if (gplot == null) throw new Xcept(page, 
		"No gplot to animate.");
	    int ax = page.confPlot().confAxis.val();
	    double[] bounds = gplot.dataBounds(ax);
	    if (bounds == null || Double.isNaN(bounds[0])
	    || Double.isNaN(bounds[0])) throw new Xcept(page, 
		"No data available in plot.");
	    datamin = bounds[0];
	    datamax = bounds[1];
	    if (datamax-datamin <= axisrange) throw new Xcept(page,
		"Invalid axis min/max for animation.");

	    // update dialog & state
	    state = RUNNING;
	    refresh();

	    // launch timer
	    int delay = (int) (1000 / page.animeRate.val());
	    if (timer == null) 
		timer = new Timer(delay, this);
	    else 
		timer.setDelay(delay);
	    timer.start();

	    // 1st event immediate
	    updateGraph();  
	}


	// stop animation
	private void stop() {
	    state = IDLE;
	    refresh();
	    timer.stop();
	}

	// pause animation
	private void pause() {
	    state = PAUSED;
	    refresh();
	    timer.stop();
	}

	// resume animation
	private void resume() {
	    state = RUNNING;
	    refresh();
	    timer.start();
	}

	// hide window
	private void hide() {
	    if (state != IDLE) stop();
	    window.setVisible(false);
	}

	// respond to timer event
	public void actionPerformed(ActionEvent e) {

	    // move fpos
	    boolean end = false;
	    if (page.animeFwd.val()) {
		fpos += fdelta;
		if (fpos > 1) {
		    fpos = 1;
		    end = true;
		}
	    } else {
		fpos -= fdelta;
		if (fpos < 0) {
		    fpos = 0;
		    end = true;
		}
	    }

	    // update graph
	    updateGraph();

	    // done? loop
	    if (!end) return;
	    if (page.animeLoop.val()) 
		fpos = 1-fpos; // 0->1, 1->0
	    else
		stop();
	}

	// update graph
	private void updateGraph() {
	    double min = datamin + fpos*(datamax-datamin-axisrange);
	    double max = min + axisrange;
	    try {
	    	paxis.min.setVal(min);
	    	paxis.max.setVal(max);
	    } catch (Xcept e) {
		System.err.println("animate: " + e);
	    }
	    gpage.conf.animationUpdate();
	    gplot.refreshAxisBounds();
	}

}

	
