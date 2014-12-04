/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Single dataset in project

package JSim.project;

import java.io.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import JSim.util.*;
import JSim.data.*;

public class PDataSet extends PNamed {

	// controls
	public TextControl notes;
	public StringControl origFile; // name of original imported file
	public ChoiceControl encoding; // encoding
	public ChoiceControl filterType; // what type of filter
	public IntControl  filterDim;
	public RealControl filterMin;
	public RealControl filterMax;
	public RealControl filterDelta;
	public BooleanControl log; // Y axis log scale display?

	// state
	private Data.NList dlist; // data list

	// constructor
	public PDataSet(Project p, String n) throws Xcept {
	    super(p, n);
	    addDescCntl();
	    notes = new TextControl(this, "notes");
	    origFile = new StringControl(this, "origFile");
	    encoding = new ChoiceControl(this, "encoding", 1,
		new String[] { "legacy", "ascii" });
	    filterType = new ChoiceControl(this, "filterType", 0,
		new String[] { "crop", "resample", "block", "normal" });
	    filterDim = new IntControl(this, "filterDim", 0);
	    filterMin = new RealControl(this, "filterMin", 0);
	    filterMax = new RealControl(this, "filterMax", 100);
	    filterDelta = new RealControl(this, "filterDelta", 0.1);
	    log = new BooleanControl(this, "log", false);
	    dlist = new Data.NList(4);
	}

	// JSReadable constructor,  dlist is readable contents of r
	public PDataSet(Project p, JSReadable readable, 
	Data.List dlist) 
	throws Xcept {
	    this(p, p.loadName(readable));
	    importData(dlist);
	    origFile.setVal(readable.prettyPath());
	}

	// create PDataSet from plot contents
	public PDataSet(Project p, PlotPage page)
	throws Xcept {
	    this(p, p.newChildName("PlotData", true));
	    Data.List ilist = new Data.List(8);
	    page.addData(ilist);
	    importData(ilist);
	}

	// import from arbitrary data list
	//     for use with PlotPage constructor or Batch
	//     don't do this to TAC data, or problems will occur 
	public void importData(Data.List list) throws Xcept {
	    for (int i=0; i<list.size(); i++) {
		Data data = list.data(i);
		String dname = data.name();
		if (Util.isBlank(dname)) dname = data.desc();
		if (Util.isBlank(dname)) dname = "data";
		dname = safeName(dname);
		boolean bump = false;
		while(dlist.data(dname) != null) { 
		    dname = Util.bump(dname);
		    bump = true;
		}
		if (bump) data = data.copy();
		data.setName(dname);
		dlist.add(data);
	    }
	}

	// rename a data item
	public void rename(Data data, String n) throws Xcept {
	    if (n.equals(data.name())) return;
	    if (! n.equals(safeName(n))) throw new Xcept(
		"Dataset curve name contains illegal characters");
	    if (dlist.data(n) != null) throw new Xcept(
		"Duplicate dataset curve name \"" + n + "\"");
	    dlist.rename(data.name(), n);
	    data.setName(n);
	}

	// remove a data item
	public void removeData(String name) {
	    dlist.remove(name);
	}
		
	// filter all/some data
	public void filterData() throws Xcept {
	    filterData(dataList());
	}
	public void filterData(Data.List clist) 
	throws Xcept {
	    DataFilter filter = new DataFilter(filterType.val());
	    filter.setDim(filterDim.val());
	    filter.setMin(filterMin.val());
	    filter.setMax(filterMax.val());
	    filter.setDelta(filterDelta.val());
	    for (int i=0; i<clist.size(); i++) {
		Data data = filter.filter(clist.data(i));
		dlist.replace(data);
	    }
	}

	// query
	public String diagInfo() { return "PDataSet " + name; }
	public int nData() { return dlist.size(); }
	public Data data(int i) { return dlist.data(i); }
	public Data data(String n) { return dlist.data(n); }
	public Data.List dataList() {
	    Data.List list = new Data.List(nData());
	    for (int i=0; i<nData(); i++) 
		list.add(data(i));
	    return list;	
	}
	public Data dataForDesc(String desc) throws Xcept { 
	    return dlist.dataForDesc(desc);
	}

	// write XML
	public String xmlLabel() { return "dataset"; }

	// export extra XML
	public void exportExtraXML(Element e) throws Xcept {
	    Document doc = e.getOwnerDocument();
	    DataFormat fmt = appl().dataFormats().format("JSML");
	    JSMLDataWriter wrt = 
		(JSMLDataWriter) fmt.createWriter();
	    wrt.setEncoding(encoding.stringVal());
	    wrt.exportXML(doc, e, dlist.list());
	}

	// import XML element: also import JSML data tags
	public void importXMLForce(Element e) {
	    super.importXMLForce(e);		
	    dlist = new Data.NList(8);
	    try {
	    	JSMLDataFormat fmt = (JSMLDataFormat) 
		    appl().dataFormats().format("JSML");
	    	JSMLDataReader rdr = new JSMLDataReader(fmt, null);
	    	importData(rdr.importXML(e));
	    } catch (Xcept x) {
		importXMLMessage(diagInfo() + ": " + x.cleanMessage());
	    }	
	}

	// import XML child: ignore JSMLDataFormat tags
	public PNamed importXMLChild(Element c) {
	    String n = c.getNodeName();
	    if (n.equals("sampled_data")) return null; // legacy
	    if (n.equals("grid_data")) return null; // legacy
	    if (n.equals("grid")) return null; // ascii 
	    if (n.equals("ndata")) return null; // ascii
	    return super.importXMLChild(c);
	}
	
}
