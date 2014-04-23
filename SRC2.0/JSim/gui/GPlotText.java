/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Plot numerical text viewer

package JSim.gui;

import java.io.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public class GPlotText extends GNode 
implements AdjustmentListener, MouseWheelListener {

	// state
	private JPanel jpanel;
	private JTextArea text;
	private JScrollBar vscroll, hscroll;
	private PlotPage page;
	private PrettyDataWriter pwrt;
	private int vertOfs, vertTot, vertExtent;
	private int horizOfs, horizTot, horizExtent;
	private Data.List currData;
	private int fontH;

	// constructor from PNotes
	public GPlotText(GNode p, PlotPage pa) {
	    super(p, pa);
	    page = pa;

	    // create widgets
	    jpanel = new JPanel(new BorderLayout()) { 
		public void doLayout() { 
		    super.doLayout();
		    refreshExtent(); 
		}
	    };
	    setJComp(jpanel);

	    // text
	    text = new JTextArea("text area");
	    text.setBackground(glook().dark());
	    text.setEditable(false);
	    setIndentedBorder(text);
	    jpanel.add(text, BorderLayout.CENTER);
	    jpanel.addMouseWheelListener(this);

	    // scrollbars
	    vscroll = new JScrollBar(JScrollBar.VERTICAL);
	    jpanel.add(vscroll, BorderLayout.EAST);
	    vscroll.addAdjustmentListener(this);
	    vscroll.addMouseWheelListener(this);
	    hscroll = new JScrollBar(JScrollBar.HORIZONTAL);
	    jpanel.add(hscroll, BorderLayout.SOUTH);
	    hscroll.addAdjustmentListener(this);

	    // data control
	    vertOfs = 0;
	    vertTot = 30;
	    vertExtent = 10;
	    horizOfs = 0;
	    horizTot = 100;
	    horizExtent = 10;
	}

	// refresh
	public void refresh() {
	    try {
	    	refreshData();
	    } catch (Xcept e) {
		msg(e);
	    }		
	    super.refresh();
	}

	// refresh data, vertTot
	private void refreshData() throws Xcept {
	    if (pwrt == null) refreshPretty();
	    currData = exportData();
	    pwrt.setBlocks(currData);
	    vertOfs = 0;
	    horizOfs = 0;
	    vertTot = Math.max(1, pwrt.nlines());
	    horizTot = Math.max(20, pwrt.width());
	    refreshScrolls();
	}

	// scrollbars moved
	public void adjustmentValueChanged(AdjustmentEvent e) {
	    if (e.getAdjustable() == vscroll)
	    	vertOfs = e.getValue();
	    else if (e.getAdjustable() == hscroll)
	    	horizOfs = e.getValue();
	    try {
	    	refreshScrolls();
	    } catch (Xcept x) {
		msg(x);
	    }
	}

	// mouse wheel
	public void mouseWheelMoved(MouseWheelEvent e) {
	    int delta = e.getClickCount() * 
	    	e.getScrollAmount() * e.getWheelRotation();
	    vertOfs += delta;
	    if (vertOfs < 0) vertOfs = 0;
	    try {
	    	refreshScrolls();
	    } catch (Xcept x) {
		msg(x);
	    }
	}

	// new layout determines vertExtent
	private void refreshExtent() {
	    Font font = glook().textFont();
	    int fontH = text.getFontMetrics(font).getHeight();
	    vertExtent = text.getHeight() / fontH; 
	    if (vertExtent < 1) vertExtent = 1;
	    int fontW = text.getFontMetrics(font).stringWidth("W");
	    horizExtent = text.getWidth() / fontW; 
	    if (horizExtent < 1) horizExtent = 1;
	    try {
	    	refreshScrolls();
	    } catch (Xcept x) {
		msg(x);
	    }
	}	    

	// set new Scroll Properties
	private void refreshScrolls() throws Xcept {

	    // vertial scrollbar
	    vscroll.setVisible(
		vertOfs > 0 || vertExtent < vertTot);
	    int vext = Math.min(vertExtent, vertTot);
	    if (vertOfs + vext > vertTot) 
		vertOfs = vertTot - vext;
	    vscroll.setValues(vertOfs, vext, 0, vertTot);

	    // horizontal scrollbar
	    hscroll.setVisible(
		horizOfs > 0 || horizExtent < horizTot);
	    int hext = Math.min(horizExtent, horizTot);
	    if (horizOfs + hext > horizTot) 
		horizOfs = horizTot - hext;
	    hscroll.setValues(horizOfs, hext, 0, horizTot);

	    // refresh text
	    refreshText();
	}

	// refresh text
	private void refreshText() throws Xcept {
	    if (pwrt == null) refreshPretty();
	    String s;
	    if (currData == null || currData.size() == 0) {
		s = "No data available...";
	    } else {
	    	StringWriter swrt = new StringWriter();
	    	int max = vertOfs+vertExtent;
	    	pwrt.writeData(swrt, vertOfs, max);
	    	s = swrt.toString();
		s = clipText(s);
	    }
	    text.setText(s);
	}

	// refresh pwrt
	private void refreshPretty() throws Xcept {
	    DataFormat fmt = gappl().dataFormats().format("pretty");
	    pwrt = (PrettyDataWriter) fmt.createWriter();
	    pwrt.setPrecision(Util.SINGLE_PRECISION);
	}

	// clip text
	private String clipText(String s) {
	    BufferedReader rdr = new BufferedReader(
		new StringReader(s));
	    StringBuffer buf = new StringBuffer(s.length());
	    while(true) {
		try {
		    String l = rdr.readLine();
		    if (l == null) break;
		    l = (l.length() > horizOfs) ?
			l.substring(horizOfs) : "";
		    buf.append(l);
		    buf.append('\n');
	    	} catch (IOException e) {
		    return s;
		}
	    }
	    return buf.toString();
	}

	// error message
	private void msg(Xcept e) {
	    gproject().message(e);
	}

	// query
	public String pageText() throws Xcept {
	    DataFormat fmt = gappl().dataFormats().format("pretty");
	    pwrt = (PrettyDataWriter) fmt.createWriter();
	    pwrt.setPrecision(Util.SINGLE_PRECISION);
	    StringWriter swrt = new StringWriter();
	    Data.List dlist = exportData();
	    if (dlist.size() == 0) throw new Xcept(page,
		"No data available to plot");
	    pwrt.writeData(swrt, dlist);
	    return swrt.toString();
	}
	public Data.List exportData() throws Xcept { 
	    Data.List dlist = new Data.List(4);
	    page.addData(dlist);
	    return dlist;
	}

}
