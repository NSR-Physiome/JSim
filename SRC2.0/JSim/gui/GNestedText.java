/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Nested plot numerical text viewer

package JSim.gui;

import java.io.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public class GNestedText extends GNode 
implements AdjustmentListener, MouseWheelListener {

	// state
	private JPanel jpanel;
	private JTextArea text;
	private JScrollBar vscroll, hscroll;
	private PrettyDataWriter pwrt;
	private int vertOfs, vertTot, vertExtent;
	private int horizOfs, horizTot, horizExtent;
	private Data.List currData;
	private int fontH;

	// constructor from PNotes
	public GNestedText(GNode g, PNested p) {
	    super(g, p);

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
	    currData = null;
	    currData = getDataList();
	    pwrt.setBlocks(currData);
	    vertOfs = 0;
	    horizOfs = 0;
	    vertTot = Math.max(1, pwrt.nlines());
	    horizTot = Math.max(20, pwrt.width());
	    refreshScrolls();
	}

	// get data list (complete or zoomed)
	protected Data.List getDataList() throws Xcept {
	    Data.List list = gnested().getDataList();
	    if (list.isEmpty()) return list;
	    if (gnested().isZoom()) {
	    	Data.List zlist = new Data.List();
		ArrayList<PNestedDataItem.Attr> attrs = gnested().getAttrList();
		int zoomX = gnested().getData().xinx(gnested().zoomInx());
		int zoomY = gnested().getData().yinx(gnested().zoomInx());
	        for (int i=0; i<list.size(); i++) {
		    PNestedDataItem.Attr attr = attrs.get(i);
		    if (attr.x2inx >= 0 && attr.x2inx != zoomX) continue;
		    if (attr.y2inx >= 0 && attr.y2inx != zoomY) continue;
		    zlist.add(list.get(i));
		}
		list = zlist;
	    }
	    // remove PlotData
	    return list;
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
		s = "No data available. " + gnested().statusMsg();
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
	    gproject().warning(e);
	    text.setText(e.cleanMessage());
	}

	// query
	public GNested gnested() { 
	    return (GNested) ancestor(GNested.class); 
	}
/*	public String pageText() throws Xcept {
	    DataFormat fmt = gappl().dataFormats().format("pretty");
	    pwrt = (PrettyDataWriter) fmt.createWriter();
	    pwrt.setPrecision(Util.SINGLE_PRECISION);
	    StringWriter swrt = new StringWriter();
//	    Data.List dlist = gnested().getDataList();
	    pwrt.writeData(swrt, currData);
	    return swrt.toString();
	}
*/
}
