/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// write instance of a DataFormat

package JSim.data;

import JSim.util.*;
import java.io.*;

public abstract class DataWriter implements DiagInfo {

	// state
	protected DataFormat format;
	private String fileName; // or other destination, for diags
	private PrettyFormat pretty; // number formatter
	private double zeroThresh; // force to zero threshold
	protected String warning; // if any,  from last write

	// constructor
	public DataWriter(DataFormat f) {
	    format = f;
	    fileName = null;
	    setPrecision(Util.DOUBLE_PRECISION);
	    zeroThresh = 0;
	}

	// query
	public String diagInfo() {
	    return format.shortName() + " data writer";
	}
	public String fileName() { 
	    return (fileName==null) ? "unknown" : fileName; 
	}
	public int precision() { 
	    return pretty.precision(); 
	}
	public String pretty(double val) {
	    return pretty.format(val);
	}
	public String defaultEncoding() { return null; }
	public String[] encodings() { return null; }
	public String warning() { return warning; }

	// set properties
	public void setFileName(String s) { fileName = s; }
	public final void setPrecision(int i) {
	    pretty = new PrettyFormat(i);
	    pretty.setZeroThresh(zeroThresh);
	}
	public final void setZeroThresh(double z) {
	    zeroThresh = z;
	    pretty.setZeroThresh(zeroThresh);
	}
	public void setEncoding(String opt) throws Xcept {
	    if (! Util.isBlank(opt)) throw new Xcept(this,
		"Output encoding not supported: \"" + opt + "\"");
	}

	// customizable write support
	abstract public void writeData(Writer wrt, Data.List data) 
	throws Xcept;

	// write to File
	public final void writeFile(File f, Data.List data) throws Xcept {
	    try {
		FileWriter wrt = new FileWriter(f);
		writeData(wrt, data);
		wrt.close();
	    } catch (IOException e) {
		throw new Xcept(this, "IO Error writing file " + 
		    f.getAbsolutePath());
	    }
	}

	// write to text String
	public final String writeText(Data.List data) throws Xcept {
	    StringWriter wrt = new StringWriter();
	    writeData(wrt, data);
	    return wrt.toString();
	}

	// write to byte array
	public final byte[] writeBytes(Data.List data) throws Xcept {
	    ByteArrayOutputStream out = new ByteArrayOutputStream();
	    writeData(out, data);
	    return out.toByteArray();
	}

	// write to OutputStream
	public void writeData(OutputStream out, Data.List data) throws Xcept {
	    OutputStreamWriter wrt = new OutputStreamWriter(out);
	    writeData(wrt, data);
	    try {
		wrt.flush();
	    } catch (IOException e) {
		throw new Xcept(this, "IO error during write");
	    }
	} 

	// data writer than can write in blocks
	public abstract static class Blocked extends DataWriter {

	    // constructor
	    public Blocked(DataFormat f) { super(f); }

	    // set blocks
	    abstract public void setBlocks(Data.List dlist) 
	    throws Xcept;

	    // query
	    abstract int nlines(); // # lines
	    abstract int width();  // max width

	    // write data from lines [lmin,lmax)
	    abstract public void writeData(Writer wrt,
	    int lmin, int lmax) throws Xcept;

	    // write to OutputStream
	    public void writeData(OutputStream out, int lmin, int lmax) 
	    throws Xcept {
	    	OutputStreamWriter wrt = new OutputStreamWriter(out);
	    	writeData(wrt, lmin, lmax);
	    	try {
		    wrt.flush();
	    	} catch (IOException e) {
		    throw new Xcept(this, "IO error during write");
	    	}
	    } 
	}
}

