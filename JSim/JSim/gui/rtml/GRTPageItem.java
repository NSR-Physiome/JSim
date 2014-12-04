/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// item in GRTPage

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.expr.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.gui.*;

public abstract class GRTPageItem extends GRTNode{

	// state
	private double x, y, w, h;
	private double fontMult;
	private String fg;
	private String bg;
	private String text;
	private String helpText;
	private int xMax, yMax;
	private Expr visible;  // is item visible

	// constructor
	public GRTPageItem(GRTNode p, Element e) {
	    super(p);
	    double[] arr;
	    arr = parseN(e.getAttribute("pos"), 2);
	    x = arr[0];
	    y = arr[1];
	    arr = parseN(e.getAttribute("size"), 2);
	    w = arr[0];
	    h = arr[1];
	    arr = parseN(e.getAttribute("fontMult"), 1);
	    fontMult = arr[0];
	    fg = e.getAttribute("fg");
	    if (Util.isBlank(fg)) fg = null;
	    bg = e.getAttribute("bg");
	    if (Util.isBlank(bg)) bg = null;
	    text = e.getAttribute("text");
	    if (Util.isBlank(text)) text = null;
	    helpText = e.getAttribute("helpText");
	    if (Util.isBlank(helpText)) helpText = null;
	    visible = Expr.truex;
	    String visx = e.getAttribute("visible");
	    if (Util.isBlank(visx)) 
		visible = Expr.truex;
	    else try {
		PModelVars vars = gmodel().pmodel().vars();
		visible = vars.parseControlExpr(visx);
	        if (visible.dataType() != Expr.BOOLEAN)
		    visible = Expr.falsex;
	    } catch (Xcept x) {
		visible = Expr.falsex;
		System.err.println("RTML Xcept: " + 
		    x.cleanMessage());
	    }
	}

	// parse two numbers
	public double[] parseN(String s, int n) {
	    double[] arr = new double[n];
	    StringTokenizer stok = new StringTokenizer(s);
	    if (stok.countTokens() != n) {
	    	for (int i=0; i<n; i++) 
		    arr[i] = Double.NaN;
		return arr;
	    }
	    for (int i=0; i<n; i++) 
		arr[i] = Util.toDouble(stok.nextToken());
	    return arr;
	}	    


	// set size and position
	public void setBounds() {
	    if (! Double.isNaN(x)) 
	    	jcomp().setLocation(pix(x), pix(y));
	    if (userSized()) 
		jcomp().setSize(wpix(), hpix());
	    else
	    	jcomp().setSize(jcomp().getPreferredSize());

	    Rectangle r = jcomp().getBounds();
	    xMax = r.x + r.width;
	    yMax = r.y + r.height;
	}

	// set font size
	public void setFont(JComponent c) {
	    double mult = Double.isNaN(fontMult) ? 1 : fontMult;
	    c.setFont(glook().font(mult));
	}
	public void setFont() { setFont(jcomp()); }

	// pixel places
	public int pix(double x) {
	    return (int) (x * glook().fontSize());
	}

	// query
	public String text() { return text; }
	public String helpText() { return helpText; }
	public int xMax() { return xMax; }
	public int yMax() { return yMax; }
	public boolean visible() { 
	   try {
		return visible.constBoolVal();
	   } catch (Xcept e) {
		System.err.println("Visible calc xcept: " +
		    e.cleanMessage());
		return false;
	   }
	}
	public GRTPage page() {
	    return (GRTPage) ancestor(GRTPage.class);
	}
	public boolean userSized() { return !Double.isNaN(w); } 
	public int wpix() { return pix(w); }
	public int hpix() { return pix(h); }
}

