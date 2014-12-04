/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// reader for JSMLDataFormat

package JSim.data;

import java.io.*;
import java.util.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.w3c.dom.CharacterData;

import JSim.util.*;
import JSim.expr.*;

public class JSMLDataReader extends XMLDataReader {

	// read-time state
	private Data.List grids;
	private StringList gridTags;

	// constructor
	public JSMLDataReader(XMLDataFormat f, Reader r) 
	throws Xcept {
	    super(f, r);
	}

	// customizable import 
	public Data.List importXML(Element e) throws Xcept {

	    // which decode version
	    String encoding = e.getAttribute("coding");
	    if (Util.isBlank(encoding)) encoding = "legacy";
	    if (! encoding.equals("legacy")
	    && ! encoding.equals("ascii")) throw new Xcept(this,
		"Unsupported JSML encoding: \"" + encoding + "\"");
	    boolean isLegacy = encoding.equals("legacy");

	    // initialize load
	    Data.List dlist = new Data.List(4);
	    if (!isLegacy) {
		grids = new Data.List(4);
		gridTags = new StringList(4);
	    }

	    // load each node
	    NodeList nodes = e.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
	        if (! (nodes.item(i) instanceof Element)) continue;
		Element e1 = (Element) nodes.item(i);
		Data data = isLegacy ?
		    readDataLegacy(e1) : readData(e1);
		if (data != null) dlist.add(data);
	    }   
	    return dlist;
	}

	// read a data item from 
	private Data readData(Element e) throws Xcept {
	    String type = e.getNodeName();
	    if (type.equals("ndata"))
		return readNData(e);
	    if (type.equals("grid"))
		return readGrid(e);
	    return null;
	}

	// read a grid
	private GridData readGrid(Element e) throws Xcept {

	    // if duplicate tag,  return cached grid
	    String tag = e.getAttribute("tag");
	    if (! Util.isBlank(tag)) {
	    	int inx = gridTags.indexOf(tag);
	    	if (inx >= 0) return (GridData) grids.data(inx);
	    }

	    // common new grid properties
	    String name = e.getAttribute("name");
	    String desc = e.getAttribute("desc");
	    String unit = e.getAttribute("unit");
	    Unit u = Util.isBlank(unit) ? 
		null : new Unit(unit);

	    // irregular grid if nsamples, otherwise regular
	    GridData grid;
	    String nsamp = e.getAttribute("nsamples");
	    if (Util.isBlank(nsamp)) {
	    	double min = Util.toDouble(e.getAttribute("min"));
	    	double max = Util.toDouble(e.getAttribute("max"));
	    	int ct = (int) Util.toDouble(e.getAttribute("ct"));
	        grid = new RegularGridData(desc, u, min, max, ct);
	    } else {
	    	double[] samp = getSamples(e, Util.toInt(nsamp));
		grid = new IrregularGridData(desc, u, samp);
	    }

	    // common followup
	    if (! Util.isBlank(name)) grid.setName(name);
	    if (! Util.isBlank(tag)) {
		grids.add(grid);
		gridTags.add(tag);
	    }
	    return grid;
	}

	// read ndata
	private RealNData readNData(Element e) throws Xcept {
	    
	    // tabulate grids
	    Data.List gridList = new Data.List(4);
	    NodeList nlist = e.getChildNodes();
	    for (int i=0; i<nlist.getLength(); i++) {
		if (! (nlist.item(i) instanceof Element)) continue;
		Element de = (Element) nlist.item(i);
		if (! de.getNodeName().equals("grid")) continue;
		gridList.add(readGrid(de));
	    }

	    // create blank ndata
	    String name = e.getAttribute("name");
	    String desc = e.getAttribute("desc");
	    String unit = e.getAttribute("unit");
	    Unit u = Util.isBlank(unit) ? 
		null : new Unit(unit);
	    GridData[] gridArr = new GridData[gridList.size()];
	    for (int i=0; i<gridArr.length; i++) 
		gridArr[i] = (GridData) gridList.data(i);
	    if (gridArr.length == 0) gridArr = null;
	    RealNData ndata = new RealNData(desc, u, gridArr);
	    ndata.setName(name);

	    // fill in samples
	    if (gridArr == null) {
		double v = Util.toDouble(e.getAttribute("value"));
	        ndata.set(0, v);
	    } else {
	    	int nsamp = ndata.nsamples();
	    	double[] samp = getSamples(e, nsamp);
	    	ndata.set(samp);
	    }

	    // done
	    return ndata;
	}
	    
	// get samples array
	private double[] getSamples(Element e, int ct) throws Xcept {	    

	    // find samples element
	    Element esamp = null;
	    NodeList nlist = e.getChildNodes();
	    for (int i=0; i<nlist.getLength(); i++) {
		if (! (nlist.item(i) instanceof Element)) continue;
		Element de = (Element) nlist.item(i);
		if (de.getNodeName().equals("samples")) {
		    esamp = de;
		    break;
		}
	    }
	    if (esamp == null) throw new Xcept(this,
		"required \"samples\" tag not found");

	    // extract text from Character nodes
	    StringBuffer buf = new StringBuffer();
	    NodeList clist = esamp.getChildNodes();
	    for (int i=0; i<clist.getLength(); i++) {
		Node node = clist.item(i);
		if (! (node instanceof CharacterData))
		    continue;
		CharacterData cdata = (CharacterData) clist.item(i);
		buf.append(cdata.getData());
	    }

	    // decode text
	    double[] samp = new double[ct];
	    Hex.decode10(buf.toString(), samp);
	    return samp;
	}

	// read legacy encoding Data structure from DOM Element
	private Data readDataLegacy(Element e) throws Xcept {

	    // get top level attributes
	    if (! e.getNodeName().equals("sampled_data")) return null; 
	    String name = e.getAttribute("name");
	    String desc = e.getAttribute("desc");
	    String unitstr = e.getAttribute("unit");
	    int ndim = Util.toInt(e.getAttribute("ndim"));

	    // initialize data reading
	    StringList[] dstr = new StringList[ndim+1];
	    for (int i=0; i<=ndim; i++) 
		dstr[i] = new StringList(16);

	    // loop over datum elements within sampled_data element
	    NodeList nlist = e.getChildNodes();
	    for (int i=0; i<nlist.getLength(); i++) {
		if (! (nlist.item(i) instanceof Element)) continue;
		Element de = (Element) nlist.item(i);
		if (! de.getNodeName().equals("datum"))
		    throw new Xcept("<datum> expected");
		
		// build text from Character nodes
		String dtext = "";
		NodeList clist = de.getChildNodes();
		for (int j=0; j<clist.getLength(); j++) {
		    Node node = clist.item(j);
		    if (! (node instanceof CharacterData))
			continue;
		    CharacterData cdata = (CharacterData) clist.item(j);
		    dtext = dtext + cdata.getData();
		}

		// parse text for numeric
		StringTokenizer stok = new StringTokenizer(dtext);
		if (stok.countTokens() != ndim+1) throw new Xcept(
		    "datum<" + dtext + "> has incorrect # of tokens");
		for (int j=0; j<=ndim; j++)
		    dstr[j].add(stok.nextToken());
	    }

	    // create dtbl packed arrays
	    int ndatum = dstr[0].size();
	    double[][] dtbl = new double[ndim+1][ndatum];
	    for (int i=0; i<=ndim; i++) 
		for (int j=0; j<ndatum; j++) 
		    dtbl[i][j] = Util.toDouble(dstr[i].str(j));
	
	    // create grids
	    GridData[] grids = new GridData[ndim];
	    for (int i=0; i<ndim; i++) 
		grids[i] = new IrregularGridData("", Unit.scalar(),
		    dtbl[i]);
	    if (grids.length == 0) grids = null;

	    // create sampled data
	    Data ret = new RealNData(desc, Unit.scalar(), grids, dtbl[ndim]);
	    ret.setName(name);
	    return ret;
	}
}
