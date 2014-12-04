/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// min,max scroller for plot window scrollbars

package JSim.gui.jcomp;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import JSim.util.*;

public class JSMinMaxSlider extends JComponent
implements MouseListener, MouseMotionListener {

	// listeners
	private ArrayList<Listener> listeners;

	// properties
	private double lowBound, highBound;
	private double min, max;
	private Color contentColor, tailColor;

	// text formatting
	private static double LOG10 = Math.log(10); 
	private static int MINPREC = 5; // minimum precision
	private static int MAXPREC = 15; // maximum precision
	private PrettyFormat pretty;

	// last paint fields for event processing
	private int xltrack; // left end of track
	private int xltail; // left end of left tail
	private int xlcontent; // left end of content
	private int xrcontent; // right end of content
	private int xrtail; // right end of right tail
	private int xrtrack; // right end of track

	// drag state (set on mouse press)
	private Point drag0;
	private double dragLow, dragHigh;
	private double dragMin, dragMax;
	private static final int NONE = 0;
	private static final int MIN = 1;
	private static final int CONTENT = 2;
	private static final int MAX = 3;
	private int dragRegion;	

	// constructor
	public JSMinMaxSlider() {
	    super();
	    lowBound = highBound = min = max = Double.NaN;
	    listeners = new ArrayList<Listener>();
	    addMouseListener(this);
	    addMouseMotionListener(this);
	    pretty = new PrettyFormat(4);
	}

	// set bounds
	public void setBounds(double lo, double hi) {
	    if (lo == lowBound && hi==highBound) return;
	    lowBound = lo;
	    highBound = hi;
	    updatePretty();
	    repaint();
	}

	// set min/max
	public void setMinMax(double mn, double mx) {
	    if (mn==min && mx==max) return;
	    min = mn;
	    max = mx;
	    updatePretty();
	    repaint();
	}	

	// update pretty formatter precision
	private void updatePretty() {
	    double lo = lowBound();
	    double hi = highBound();
	    double delta = Math.abs(hi-lo);
	    double mag = Math.max(Math.abs(lo), Math.abs(hi));
	    double precf = 3 + Math.ceil(Math.log(mag/delta)/LOG10);
	    int prec = Double.isNaN(precf) ? MINPREC : ((int) precf);
	    if (prec<MINPREC) prec = MINPREC;
	    if (prec>MAXPREC) prec = MAXPREC; // bullet-proof
	    if (prec != pretty.precision())
		pretty = new PrettyFormat(prec);
	}

	// set colors
	public void setTailColor(Color c) { 
	    tailColor = c;
	    repaint();
	}
	public void setContentColor(Color c) { 
	    contentColor = c;
	    repaint();
	}

	// get properties
	public double getMin() { return min; }
	public double getMax() { return max; }
	public double getLowBound() { return lowBound; }
	public double getHighBound() { return highBound; }
	public Color getTailColor() { return tailColor; }
	public Color getContentColor() { return contentColor; }

	// add listener
	public void addMinMaxListener(Listener l) {
	    listeners.add(l);
	}

	// paint the widget
	public void paint(Graphics g) {

	    // bounds adjusted by insets
	    Rectangle b = getBounds();
	    Insets insets = getInsets();
	    b.x += insets.left;
	    b.y += insets.bottom;
	    b.width -= insets.left + insets.right;
	    b.height -= insets.top + insets.bottom;
	    if (b.width > 10) { // margin adjust
		b.x += 2; 
	    	b.width -= 4;
	    }

	    // clear, set foreground
	    g.setColor(getBackground());
	    g.fillRect(b.x, b.y, b.width, b.height);
	    Color fg = isActive() ?
		getForeground() : Color.gray;
	    g.setColor(fg);

	    // font properties
	    Font font = getFont();
	    if (font == null) {
		font = new Font("Monospaced", Font.BOLD, 8);
		setFont(font);
	    }
	    FontMetrics fontM = getFontMetrics(font);
	    int fontH = fontM.getMaxAscent();

	    // Y positions + xtail
	    int y1 = b.y + (int) (0.1*b.height);  // bottom of scroller
	    int y2 = b.y + (int) (0.4*b.height); // lower track
	    int y3 = b.y + (int) (0.6*b.height); // upper track
	    int y4 = b.y + (int) (0.9*b.height); // top of scroller

	    // bounds text and track positions
	    String ltext = pretty.format(lowBound());
	    String rtext = pretty.format(highBound());
	    int ltextW = fontM.stringWidth(ltext); 
	    int rtextW = fontM.stringWidth(rtext);
	    int xltext = b.x;
	    int xrtext = b.x + b.width - rtextW;
	    int xtail = (y4-y1)/2; // width of tails	    
	    xltrack = xltext + ltextW + 2*xtail;
	    xrtrack = xrtext - 2*xtail;
	    
	    // draw text only track length >= b.width/2
	    boolean drawText = (xrtrack-xltrack) >= b.width/2;
	    if (drawText) {
	    	int ytext = b.y + (b.height+fontH)/2; 
	    	g.setFont(font);
	    	g.drawString(ltext, xltext, ytext);
	    	g.drawString(rtext, xrtext, ytext);
	    } else {
	    	xltrack = b.x + xtail;   
	    	xrtrack = b.x + b.width - xtail; 
	    }

	    // invalid min/max -> draw empty track & exit
	    if (!(min<max)) {  // NaN correct
		g.drawRect(xltrack, y2, xrtrack-xltrack, y3-y2);
		return;
	    }

	    // content fractions
	    double hi_lo = highBound()-lowBound();
	    double fmin = (min-lowBound())/hi_lo;
	    if (fmin < 0) fmin = 0;
	    if (fmin > 1) fmin = 1;
	    double fmax = (max-lowBound())/hi_lo;
	    if (fmax < 0) fmin = 0;
	    if (fmax> 1) fmin = 1;
	    if (fmin > fmax) {
		double tmp = fmax;
		fmax = fmin;
		fmin = tmp;
	    }

	    // min/max determined X positions
	    xlcontent = xltrack + (int)(fmin*(xrtrack-xltrack)); // content left
	    xrcontent = xltrack + (int)(fmax*(xrtrack-xltrack)); // content right
	    xltail = xlcontent - xtail; // left tail
	    xrtail = xrcontent + xtail; // right tail

	    // draw content
	    if (isActive() && contentColor != null) {
		g.setColor(contentColor);
		g.fillRect(xlcontent, y1, xrcontent-xlcontent, y4-y1-1);
		g.setColor(fg);
	    }
	    g.drawRect(xlcontent, y1, xrcontent-xlcontent, y4-y1-1);

	    // draw track
	    if (xltail>xltrack) {
	    	g.drawLine(xltrack, y2, xltail, y2);
	    	g.drawLine(xltrack, y3, xltail, y3);
		g.drawLine(xltrack, y2, xltrack, y3);
	    }
	    if (xrtrack>xrtail) {
	    	g.drawLine(xrtail, y2, xrtrack, y2);
	    	g.drawLine(xrtail, y3, xrtrack, y3);
		g.drawLine(xrtrack, y2, xrtrack, y3);
	    }

	    // define tail polygons
	    int ymid = (y1+y4)/2;
	    Polygon pleft = new Polygon(
		new int[] { xltail-1, xlcontent-1, xlcontent-1 },
		new int[] { ymid, y1, y4 },
		3);
	    Polygon pright = new Polygon(
		new int[] { xrtail+1, xrcontent+1, xrcontent+1 },
		new int[] { ymid, y1, y4 },
		3);

	    // fill tails
	    if (isActive()) {
		if (tailColor != null)
		    g.setColor(tailColor); 
	    	g.fillPolygon(pleft);
	    	g.fillPolygon(pright);
	    	g.setColor(fg);
	    }

	    // outline tails
	    g.drawPolygon(pleft);
	    g.drawPolygon(pright);
	}

	// is widget active
	private boolean isActive() {
	    return isEnabled()
	    && (min < max) // NaN correct
	    && (!Double.isNaN(lowBound))
	    && (!Double.isNaN(highBound));
	}

	// working bounds,  assumes min/max are valid
	private double lowBound() {
	    if (lowBound < min) return lowBound; // NaN correct
	    return min;
	}
	private double highBound() {
	    if (highBound > max) return highBound; // NaN correct
	    return max;
	}

	// mouse event processing
	public void mouseEntered(MouseEvent e) { }
        public void mouseExited(MouseEvent e) {
	    stopDrag();
	}
        public void mouseClicked(MouseEvent e) {
	    if (isActive() && e.getButton() == 1) 
	    	doClick(e.getPoint(), e.getClickCount());
	}
        public void mousePressed(MouseEvent e) { 
	    if (isActive() && e.getButton() == 1) 
	    	startDrag(e.getPoint());
	}
        public void mouseReleased(MouseEvent e) { 
	    stopDrag();
	}
	public void mouseDragged(MouseEvent e) { 
	    if (isActive()) 
	    	doDrag(e.getPoint());
	}
	public void mouseMoved(MouseEvent e) { }

	// do click
	private void doClick(Point p, int clicks) {

	    // dbl click recalcs min/max
	    if (clicks != 2) return;
	    double nmin, nmax;
	    double range = max-min;
	    switch (region(p)) {
	    case MIN: // shift window left
		nmin = min - range;
		if (nmin < lowBound()) nmin = lowBound();
		nmax = max - (min-nmin);
		break;
	    case MAX: // shift window right
		nmax = max + range;
		if (nmax > highBound()) nmax = highBound();
		nmin = min + (nmax-max);
		break;
	    case CONTENT: // full scale
		if (Double.isNaN(lowBound) 
		|| Double.isNaN(highBound)) return;
		nmin = lowBound;
		nmax = highBound;
		break;
	    default:
		return;
	    }

	    // update
	    if (min == nmin && max == nmax) 
		return; // avoid extraneous paints, events
	    min = nmin;
	    max = nmax;
	    repaint();
	    sendEvent();
	}

	// start drag
	private void startDrag(Point p) {
	    dragRegion = region(p);
	    if (dragRegion == NONE) return;

	    // correct bounds excess
	    if (lowBound > min || highBound < max) {
		if (min < lowBound || min >= highBound) 
		    min = lowBound;
		if (max > highBound || max <= lowBound) 
		    max = highBound;
		repaint();
		sendEvent();
		return;
	    }

	    // store drag needed fields
	    drag0 = p;
	    dragLow = lowBound();
	    dragHigh = highBound();
	    dragMin = min;
	    dragMax = max;
	}

	// do a drag
	private void doDrag(Point p) {
	    if (drag0 == null) return;

	    // figure delta
	    double delta = (p.x-drag0.x)*(highBound()-lowBound())
		/(xrtrack-xltrack);

	    // update min and/or max
	    double nmin = min;
	    double nmax = max;
	    switch (dragRegion) {
	    case MIN:
		nmin = dragMin+delta;
		if (nmin < dragLow) nmin = dragLow;
		if (nmin >= max) return;
		break;
	    case MAX:
		nmax = dragMax+delta;
		if (nmax > dragHigh) nmax = dragHigh;
		if (nmax <= min) return;
		break;
	    case CONTENT:
		nmin = dragMin+delta;
		nmax = dragMax+delta;
		if (nmin < dragLow) {
		    nmin = dragLow;
		    nmax = dragLow + dragMax - dragMin;
		}
		if (nmax > dragHigh) {
		    nmax = dragHigh;
		    nmin = dragHigh - (dragMax - dragMin);
		}
		break;
	    }
	    if (nmin == min && nmax == max) return;

	    // update
	    min = nmin;
	    max = nmax;
	    repaint();
	    sendEvent();
	}

	// stop drag
	private void stopDrag() {
	    if (drag0 == null) return;
	    drag0 = null;
	}
	 
	// region for point
	private int region(Point p) {
	    if (p.x > xltail && p.x < xlcontent) 
		return MIN;
	    else if (p.x > xlcontent && p.x < xrcontent) 
		return CONTENT;
	    else if (p.x > xrcontent && p.x < xrtail) 
		return MAX;
	    return NONE;
	}

	// send event to each listener
	private void sendEvent() {
	    for (int i=0; i<listeners.size(); i++) {
		Listener l = (Listener) listeners.get(i);
		l.minmaxChanged(new ComponentEvent(this, 0));
	    }
	}

	// JSMinMaxSlider.Listener
	public static interface Listener {
	    void minmaxChanged(ComponentEvent e);
	}

	//// test routines

	// test mainline
	public static void main(String[] args) throws Exception {

	    // command line
	    if (args.length != 5) throw new Exception(
		"usage: lo min max hi enabled");
	    double lo = Util.toDouble(args[0]);
	    double min = Util.toDouble(args[1]);
	    double max = Util.toDouble(args[2]);
	    double hi = Util.toDouble(args[3]);
	    boolean enabled;
	    if (args[4].equals("t"))
		enabled = true;
	    else if (args[4].equals("f"))
		enabled = false;
	    else
		throw new Exception("enabled must be t or f");

	    // scroller
	    JSMinMaxSlider gmm = new JSMinMaxSlider();
	    gmm.setBounds(lo, hi);
	    gmm.setMinMax(min, max);
	    gmm.setEnabled(enabled);
	    gmm.setPreferredSize(new Dimension(400,24));
	    gmm.setFont(new Font("Monospaced", Font.BOLD, 12));
	    gmm.setForeground(Color.black);
	    gmm.setBackground(new Color(160,210,250));
	    gmm.setTailColor(new Color(250,200,120));
	    gmm.setContentColor(Color.white);
	    mainListener l = new mainListener();
	    gmm.addMinMaxListener(l);

	    // containing panel & frame
	    JPanel panel = new JPanel(new GridLayout(1,1));
	    panel.add(gmm);
	    JFrame frame = new JFrame("JSMinMaxSlider");
	    frame.getRootPane().setContentPane(panel);
	    frame.pack();
	    frame.setVisible(true);
	}

	// test Listener
	public static class mainListener implements Listener {
	    public void minmaxChanged(ComponentEvent e) {
		JSMinMaxSlider g = 
		    (JSMinMaxSlider) e.getComponent();
		System.err.println("min=" + g.getMin() +
		    " max=" + g.getMax());
	    }
	}

}
