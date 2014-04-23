/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// reader for I4BullDataFormat

package JSim.data;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.expr.*;

public class I4BullDataReader extends TextDataReader {

	// current read state
	private boolean eof;
	private String buf;
	private int bufp;
	private int ns, nz, nt, np, zofs;
	private double[] tstart, tstop, angles;
	private String[] pname, punit;
	private double[][] bulls;

	// constructor
	public I4BullDataReader(I4BullDataFormat f, Reader r) 
	throws Xcept {
	    super(f, r);
	}

	// read data
	public Data.List readData() throws Xcept {
	    buf = null;

	    // read fixed header
	    readStr("I4bull1.0");
	    readStr("sztp");
	    ns = readInt();
	    nz = readInt();
	    nt = readInt();
	    np = readInt();
	    readStr("format");
	    readStr("ascii");

	    // allocate data space
	    angles = new double[nz];
	    for (int i=0; i<nz; i++) angles[i] = 90;

	    // optional reads
	    String tok = null;
	    while ((tok = readToken()) != null) {
		if (tok.equals("timebin")) readTime();
		else if (tok.equals("parm")) readParm();
		else if (tok.equals("zoffset")) readZofs();
		else if (tok.equals("angle")) readAngle();
		else if (tok.equals("angles")) readAngles();
		else if (tok.equals("aux")) readAux();
		else if (tok.equals("data")) readBull();
		else throw new Xcept(this,
		    "recognized keyword <" + tok + ">");
	    }

	    // check for completeness
	    if (tstart == null) throw new Xcept(this,
		"tstart keyword missing");
	    if (pname == null) throw new Xcept(this,
		"parm keyword missing");
	    if (bulls == null) throw new Xcept(this,
		"data keyword missing");

	    // create data grids
	    GridData s = new RegularGridData("s",
		Unit.scalar(), 1, ns, ns);
	    GridData z = new RegularGridData("z",
		Unit.scalar(), zofs+1, zofs+nz, nz);
	    GridData[] grids = null;
	    if (nt == 1) {
		grids = new GridData[] { s, z };
	    } else {
		GridData t = new IrregularGridData("t",
		    Unit.scalar(), tstart);
		grids = new GridData[] { s, z, t };
	    }

	    // bull data list	
	    Data.List list = new Data.List(4);
	    for (int i=0; i<np; i++) {
		Data d = new RealNData(pname[i], Unit.scalar(),
		    grids, bulls[i]);
		list.add(d);
	    }

	    // angles data
	    Data ad = new RealNData("angles", Unit.scalar(),
		new GridData[] { z }, angles);
	    list.add(ad);

	    // return result
	    return list;
	}

	// real timebin keyword
	private void readTime() throws Xcept {
	    tstart = new double[nt];
	    tstop = new double[nt];
	    for (int i=0; i<nt; i++) {
	    	tstart[i] = readDouble();
	        tstop[i] = readDouble();
	    }
	}

	// real angle keyword
	private void readZofs() throws Xcept {
	    zofs = readInt();
	}

	// real angle keyword
	private void readAngle() throws Xcept {
	    double a = readDouble();
	    for (int i=0; i<nz; i++) 
	    	angles[i] = a;
	}

	// real angles keyword
	private void readAngles() throws Xcept {
	    for (int i=0; i<nz; i++) 
	    	angles[i] = readDouble();
	}

	// real parm keyword
	private void readParm() throws Xcept {
	    pname = new String[np];
	    punit = new String[np];
	    for (int i=0; i<np; i++) {
	    	pname[i] = readStr();
	        punit[i] = readStr();
	    }
	}

	// real aux keyword
	private void readAux() throws Xcept {
	    int ct = readInt();
	    for (int i=0; i<ct; i++) {
	    	readStr(); // key
	        readStr(); // value
	    }
	}

	// real data keyword
	private void readBull() throws Xcept {
	    bulls = new double[np][ns*nz*nt];
	    for (int i=0; i<np; i++) 
		for (int j=0; j<bulls[i].length; j++) 
	    	    bulls[i][j] = readDouble();
	}

	// read integer
	private int readInt() throws Xcept {
	    String s = readStr();
	    return Util.toInt(s);
	}

	// read double
	private double readDouble() throws Xcept {
	    String s = readStr();
	    return Util.toDouble(s);
	}

	// read expected String
	private void readStr(String expected) throws Xcept {
	    String s = readStr();
	    if (! s.equals(expected)) throw new Xcept(this,
		"Expected token <" + expected + 
		">,  found token <" + s + ">");
	}

	// read String
	private String readStr() throws Xcept {
	    String s = readToken();
	    if (s == null) throw new Xcept(this,
		"Unexpected end of file");
	    return Util.stripQuotes(s);
	}

	// read next Token
	private String readToken() throws Xcept {
	    if (eof) return null;

	    // search for start of token
	    while (true) {
	    	while (buf == null || bufp >= buf.length()) {
		    try {
		    	buf = rdr().readLine();
		    } catch (IOException e) {
			throw Xcept.wrap(e);
		    }
		    if (buf == null) {
			eof = true;
			return null;
		    }
		    bufp = 0;
	    	}
		char c = buf.charAt(bufp);
		if (c == '#') {
		     buf = null;
		     continue;
		}
		if (! Character.isWhitespace(c)) 
		    break;
	     	bufp++;
	    }

	    // build token
	    StringBuffer s = new StringBuffer();
	    boolean quoted = false;
	    while (true) {
		boolean done = bufp >= buf.length();
		char c = done ? '\t' : buf.charAt(bufp++);
		if (c == '"') quoted = !quoted;
		done = done || Character.isWhitespace(c);
		if (c == ' ' && quoted) done = false;
		if (done) return s.toString();
		s.append(c);
	    }   	    
	}
}
