/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataWriter for JSMLDataFormat

package JSim.data;

import JSim.util.*;

import java.io.*;
import java.net.URL;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

public class JSMLDataWriter extends XMLDataWriter {

	// format-specific options
	private boolean legacy; // write legacy version?
	private boolean single; // write using single-float cast
	private Data.List gridCache; // grid duplication cache

	// constructor
	public JSMLDataWriter(JSMLDataFormat f) { 
	    super(f);
	    legacy = false;
	}

	// available encodings
	public String[] encodings() {
	    return new String[] { "legacy", "ascii" };
	}

	// default encoding
	public String defaultEncoding() { return "ascii"; }

	// set JSML writer encoding
	public void setEncoding(String code) throws Xcept {
	    if (Util.isBlank(code) || code.equals("ascii"))
		legacy = false;
	    else if (code.equals("legacy"))
		legacy = true;
	    else throw new Xcept(this,
		"Unsupported encoding: \"" + code + "\"");
	}

	// customized export (write) support
	public void exportXML(Document doc, Element e, 
	Data.List dlist) throws Xcept {
	    e.setAttribute("version", Util.version());
	    e.setAttribute("precision", "" + precision());

	    // legacy support
	    if (legacy) {
		exportLegacy(doc, e, dlist);
		return;
	    }

	    // export each Data 
	    e.setAttribute("coding", "ascii");
	    single = precision() <= Util.SINGLE_PRECISION;
	    gridCache = new Data.List(8);
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		if (data instanceof GridData)
		    exportGrid(doc, e, (GridData) data);
		else if (data instanceof RealNData)
		    exportNData(doc, e, (RealNData) data);
		else throw new Xcept(this,
		    "Unsupported data class: " + data.getClass());
	    }
	}

	// export a grid
	private void exportGrid(Document doc, Element e, GridData grid)
	throws Xcept {
	    Element e1 = doc.createElement("grid");
	    e.appendChild(e1);

	    // replace irregular with regular grid,  if possible
	    if (grid instanceof IrregularGridData) {
		GridData ngrid = new RegularGridData(
		    grid.desc(), grid.unit(), grid.min(),
		    grid.max(), grid.ct());
		double err = (grid.max()-grid.min())/Math.pow(10, precision());
		if (grid.sameSamplesAs(ngrid, err)) {
		    ngrid.setName(grid.name());
		    grid = ngrid;
		}
	    }

	    // find same as in grid cache?
	    int gx = 0;
	    while (gx < gridCache.size()) {
		GridData cgrid = (GridData) gridCache.data(gx);
		if (grid.sameAs(cgrid))
		    break;
		gx++;
	    }
	    if (gx<gridCache.size()) {
		e1.setAttribute("tag", "grid_" + gx);
		return;
	    }

	    // add grid cache tag
	    exportAttr(e1, grid);
	    e1.setAttribute("tag", "grid_" + gridCache.size());
	    gridCache.add(grid);

	    // regular or irregular export
	    if (grid instanceof RegularGridData) {
		RegularGridData rgrid = (RegularGridData) grid;
		e1.setAttribute("min", pretty(rgrid.min()));
		e1.setAttribute("max", pretty(rgrid.max()));
		e1.setAttribute("ct", pretty(rgrid.ct()));
	    } else {
		e1.setAttribute("nsamples", "" + grid.nsamples());
		exportSamples(doc, e1, grid);
	    }
	}

	// export a RealNData
	private void exportNData(Document doc, Element e, RealNData ndata)
	throws Xcept {
	    Element e1 = doc.createElement("ndata");
	    e.appendChild(e1);
	    exportAttr(e1, ndata);
	    for (int i=0; i<ndata.ndim(); i++) 
		exportGrid(doc, e1, ndata.grid(i));
	    if (ndata.ndim() == 0) 
		e1.setAttribute("value", pretty(ndata.realVal(0)));
	    else 
	        exportSamples(doc, e1, ndata);
	}

	// export attributes
	private void exportAttr(Element e, Data data)
	throws Xcept {
	    // data attributes
	    if (data.name() != null) 
		e.setAttribute("name", data.name());
	    if (data.desc() != null) 
		e.setAttribute("desc", data.desc());
	    if (data.unit() != null) 
		e.setAttribute("unit", data.unit.pubName());
	}

	// export samples
	//    Util.pretty() not used for better performance
	private void exportSamples(Document doc, Element e, Data data)
	throws Xcept {
	    Element e1 = doc.createElement("samples");
	    StringBuffer buf = new StringBuffer();
	    for (int i=0; i<data.nsamples(); i++) {
		if (i%8 == 0 && i>0) buf.append("\n");
   		double d = data.realVal(i);
		String s;
		if (single) {
		    double absd = Math.abs(d);
		    if (absd < Float.MIN_VALUE || absd > Float.MAX_VALUE)
			s = " " + d;
		    else 
			s = " " + ((float) d);
		} else {
		    s = " " + d;
		}	
		buf.append(" " + s);
	    }
	    Text text = doc.createTextNode(buf.toString());
	    e1.appendChild(text);
	    e.appendChild(e1);
	}

	//// legacy export 

	// export data list
	public void exportLegacy(Document doc, Element e, 
	Data.List dlist) throws Xcept {
	    for (int i=0; i<dlist.size(); i++) {
		Data data = dlist.data(i);
		exportLegacy(doc, e, data);
	    }
	}

	// export one data
	private void exportLegacy(Document doc, Element e, Data data)
	throws Xcept { 
	    Element e1 = doc.createElement((data instanceof GridData) ?
		"grid_data" : "sampled_data");
	    if (data.name() != null) 
		e1.setAttribute("name", data.name());
	    if (data.desc() != null) 
		e1.setAttribute("desc", data.desc());
	    if (data.unit() != null) 
		e1.setAttribute("unit", data.unit.pubName());
	    e1.setAttribute("ndim", "" + data.ndim());
	    if (data instanceof RealNData)
		exportLegacyNData(doc, e1, (RealNData) data);
	    else if (data instanceof GridData) 
		exportLegacyGrid(doc, e1, (GridData) data);
	    else throw new Xcept(data,
		"JSMLDataWriter does not support request Data class");
	    e.appendChild(e1);
	}
	
	// export extra XML for GridData
	private void exportLegacyGrid(Document doc, Element e1, 
	GridData data) throws Xcept { 
	    for (int i=0; i<data.ct(); i++) {
		Element e2 = doc.createElement("datum");
		Text dtext = doc.createTextNode(
		    pretty(data.realVal(i)));
		e2.appendChild(dtext);
		e1.appendChild(e2);
	    }
	}

	// export extra XML for RealNData
	private void exportLegacyNData(Document doc, Element e1, 
	RealNData data) throws Xcept { 
	    for (int i=0; i<data.nsamples(); i++) {
		Element e2 = doc.createElement("datum");
		Text dtext = doc.createTextNode(dataStrLegacy(data, i));
		e2.appendChild(dtext);
		e1.appendChild(e2);
	    }
	}

	// RealNData data string
	private String dataStrLegacy(RealNData data, int inx) 
	throws Xcept {
	    if (data.ndim() == 0)
		return pretty(data.realVal(inx));
	    String s = "";
	    int[] gpos = data.gridPos(inx);
	    for (int i=0; i<data.ndim(); i++) {
		GridData grid = data.grid(i);
		s=s+pretty(grid.realVal(gpos[i]))+"  ";
	    }
	    s = s + pretty(data.realVal(inx));
	    return s;
	}
}
