/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// reader for CSVDataFormat

package JSim.data;

import java.io.*;
import java.util.*;

import JSim.util.*;

public class CSVDataReader extends TextDataReader {

	// read state
	private int ny; // # y variables (#columns - 1)
	private String xname;	// names for indepenent variable
	private StringList ynames; // y variable names
	private ArrayList<double[]> lineData; // build area for line data

	// constructor
	public CSVDataReader(CSVDataFormat f, Reader r) 
	throws Xcept {
	    super(f, r);
	}

	// read data
	public Data.List readData() throws Xcept {
	    lineData = new ArrayList<double[]>(64);
			
	    // get tokens in 1st non-blank line 
	    String line = readLine();
	    if (line == null) 
		throw new Xcept(this, "File is empty");
 	    StringList toks = getTokens(line);
	    ny = toks.size() - 1;
	    if (ny < 1) throw new Xcept(this, 
		"First non-blank line must have 2 or more tokens");

	    // process first line tokens (header or data)
	    ynames = new StringList(ny);
	    if (isNumber(toks.str(0))) {
		xname = "x";
		for (int i=0; i<ny; i++) 
		    ynames.add("y" + (i+1));
		importData(toks);
	    } else {
		xname = peelQuote(toks.str(0));
		for (int i=0; i<ny; i++) {
		    String tok = toks.str(1+i);
		    ynames.add(peelQuote(tok));
		}
	    }

	    // process remaining lines (all data)
	    while ((line=readLine()) != null) {
		toks = getTokens(line);
		importData(toks);
	    }

	    // create Data objects
	    Data.List dlist = makeData();
	    lineData = null; // release data
	    return dlist;
	}

	// get next non-blank line
	private String readLine() throws Xcept {
	    while(true) {
		String line;
		try {
		    line = rdr().readLine();
	    	} catch (IOException e) {
		    throw new Xcept(this, 
			"IO error reading file");
	    	}
		if (line == null) return line; // EOF
		if (!Util.isBlank(line)) return line;
	    }
	}

	// count # tokens in line
	private StringList getTokens(String line) throws Xcept {
	    StringList list = new StringList(4);
	    int pos = 0;
	    while(true) {

		// find start of next token
		while (pos<line.length() && 
		    line.charAt(pos) == ',')
		    pos++;
		if (pos>=line.length()) return list;

		// start new token
		StringBuffer buf = new StringBuffer();
		char termChar = line.charAt(pos);
		if (isQuote(termChar)) {
		    buf.append(termChar);
		    pos++;
		} else {
		    termChar = ',';
		}
		    
		// fill new token buffer
		while(true) {
		    char c = (pos >= line.length()) ?
			termChar : line.charAt(pos++);
		    if (isQuote(termChar)) {
			if (c == termChar) {
			    buf.append(c);
			    break;
			}
		    } else {
			if (c == ',') break;
		    }
		    buf.append(c);
		}

		// add buffer to token list
		list.add(buf.toString());
	    }
	}
			
	// quote characters
	private boolean isQuote(char c) {
	    if (c == '"' || c == '\'')
		return true;
	    return false;
	}

	// peel quotes
	private String peelQuote(String s) {
	    if (s.length()>2 && isQuote(s.charAt(0)))
		return s.substring(1, s.length()-1);
	    return s;
	}

	// token is numeric?
	private boolean isNumber(String tok) {
	    if (Util.isBlank(tok)) return false;
	    if (tok.equalsIgnoreCase("nan")) return true;
	    char c = tok.charAt(0);
	    if (Character.isDigit(c) || 
		c == '.' ||
		c == '-')
		return true;
	    return false;
	}

	// import data from tokens
	private void importData(StringList toks) throws Xcept {
	    if (toks.size() != ny+1) throw new Xcept(this,
		"require " + (ny+1) + " tokens on line");
	    double[] line = new double[ny+1];
	    for (int i=0; i<=ny; i++) {
		String s = toks.str(i);
		if (!isNumber(s)) throw new Xcept(this,
		    "numeric token expected");
		line[i] = Util.toDouble(s);
	    }
	    lineData.add(line);
	}

	// make data
	private Data.List makeData() throws Xcept {

	    // create data grid from 1st column
	    int xct = lineData.size();
	    double[] xsamp = new double[xct];
	    for (int i=0; i<xct; i++) {
		double[] line = (double[]) lineData.get(i);
		xsamp[i] = line[0];
		if (i>0 && xsamp[i]<=xsamp[i-1]) throw new Xcept(this,
		    "Line " + (i+2) + ": 1st column of data is not strictly increasing.");
	    }
	    GridData xgrid = new IrregularGridData(xname, null, xsamp);

	    // convert to regular grid?
	    RegularGridData rgrid = new RegularGridData(xname, null,
		xgrid.min(), xgrid.max(), xgrid.ct());
	    double error = rgrid.delta() * 1e-7;
	    if (xgrid.sameSamplesAs(rgrid, error)) xgrid = rgrid;

	    // initialize data curves
	    Data.List dlist = new Data.List(ny);
	    for (int i=0; i<ny; i++) {
		Data ydata = new RealNData("", null, 
		    new GridData[] { xgrid });
		ydata.setName(ynames.str(i));
		dlist.add(ydata);
	    }

	    // load samples into data curves
	    for (int i=0; i<xct; i++) {
		double[] line = (double[]) lineData.get(i);
		for (int j=0; j<ny; j++) {
		    RealNData ydata = (RealNData) dlist.data(j);
		    ydata.set(i, line[1+j]);
		}
	    }
	    return dlist;
	}
}
