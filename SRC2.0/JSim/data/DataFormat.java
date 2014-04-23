/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// super-class for Data file reading and writing

package JSim.data;

import java.util.ArrayList;
import java.io.*;
import java.net.URL;

import JSim.util.*;

public abstract class DataFormat implements DiagInfo {

	// constructor
	public DataFormat() {
	    // nothing yet
	}

	// short name (1 word) for format
	abstract public String shortName();

	// long name (several words) for format
	abstract public String longName();

	// allowed file suffixes, 1st is default
	abstract public String[] suffixes();

	// URL for format specification (or null)
	abstract public URL url();

	// is content representable by String?
	abstract public boolean isText(); 

	// in readSupported(boolean fsAccess) methods :
	//   fsAccess indicates whether JRE has local file system access
	//   e.g. for reading imported libraries or creating temp file space
	//   fsAccess is usually false in remote applet version
	//   fsAccess is usually true in standalone application 
	public boolean readSupported(boolean fsAccess) { return false; }
	public DataReader createReader(Reader r) throws Xcept {
	    throw new Xcept(this, 
		"no read support for this format");
	}

	// customizable Writer output support
	public DataWriter createWriter() throws Xcept {
	   throw new Xcept(this, 
		"no write support for this format");
	}

	// add unique names to data (needs work ???)
	private Data.NList setUniqNames(Data.List list) throws Xcept {
	    Data.NList nlist = new Data.NList(list.size());
	    for (int i=0; i<list.size(); i++) {
		Data data = list.data(i);
		String n = data.name();
		if (Util.isBlank(n)) n = "y" + (i+1);
		String nbase = n;
		int sfx = 0;
		while (nlist.data(n) != null) 
		    n = nbase + "_" + sfx++;
		data.setName(n);
		nlist.add(data);
	    }
	    return nlist;		
	}

	//// uncustomizable methods

	// read from File
	public final DataReader createReader(File f) throws Xcept {
	    try {
		return createReader(new FileReader(f));
	    } catch (FileNotFoundException e) {
		throw new Xcept(this, "File not found:" 
		    + UtilIO.prettyPath(f));
	    }
	}

	// read content from String
	public final DataReader createReader(String s) throws Xcept {
	    return createReader(new StringReader(s));
	}

	// read content from byte array
	public final DataReader createReader(byte[] b) throws Xcept {
	    return createReader(new ByteArrayInputStream(b));
	}

	// read content from InputStream
	public final DataReader createReader(InputStream inp) throws Xcept {
	    return createReader(new InputStreamReader(inp));
	}

	// query
	public String diagInfo() {
	    return shortName() + " data format";
	}

	// DataFormat.List
	public static class List extends ArrayList<DataFormat> {
	    public List() { 
	    	super(); 
	    	add(new JSMLDataFormat());
	    	add(new TacDataFormat());
	    	add(new ColumnDataFormat());
	    	add(new PrettyDataFormat());
	    	add(new LineDataFormat());
	    	add(new I4BullDataFormat());
	    	add(new MatlabDataFormat());
	    	add(new CSVDataFormat());
	    }
	    public DataFormat format(int i) { 
		return (DataFormat) get(i); 
	    }
	    public DataFormat format(String n) throws Xcept {
		for (int i=0; i<size(); i++) 
		    if (format(i).shortName().equals(n))
		        return format(i);
		throw new Xcept("Unknown DataFormat \"" + 
		    n + "\"");
	    } 
	    public DataFormat forSuffix(String s) {
		for (int i=0; i<size(); i++) {
		    DataFormat fmt = format(i);
		    String[] sfxs = fmt.suffixes();
		    for (int j=0; j<sfxs.length; j++) 
			if (s.equals(sfxs[j]))
			    return fmt;
		}
		return null;
	    }
	    public Data.List readData(File file) throws Xcept {
	        String sfx = UtilIO.fileSuffix(file);
	    	DataFormat fmt = forSuffix(sfx);
		if (fmt == null) throw new Xcept(
		    "Data file suffix not recognized: " + file);
		DataReader rdr = fmt.createReader(file);
		return rdr.readData();
	    }
	}
	    
}
