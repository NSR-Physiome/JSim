/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Readable reference (File or URL)

package JSim.data;

import java.io.*;
import java.net.*;
import java.util.ArrayList;
import JSim.util.*;

public class JSReadable implements DiagInfo {

	// auto-recognizable file types
	public static final int UNKNOWN = 0;
	public static final int PROJECT = 1;
	public static final int MML = 2;
	public static final int ANTIMONY = 3;
	public static final int JAVA = 4;
	public static final int XML = 5;
	public static final int JSML = 6;
	public static final int COLUMNDATA = 7;
	public static final int I4BULL = 8;
	public static final int TAC = 9;	
	public static final int CSVDATA = 10;
	public static final int PARSET = 11;

	// JSim readable file suffix
	public static int fileType(String s) {
	    if (s.equals("proj")) return PROJECT;
	    if (s.equals("mod")) return MML;
	    if (s.equals("mml")) return MML;
	    if (s.equals("flat")) return MML;
	    if (s.equals("txt")) return ANTIMONY;
	    if (s.equals("java")) return JAVA;
	    if (s.equals("xml")) return XML;
	    if (s.equals("cellml")) return XML;
	    if (s.equals("sbml")) return XML;
	    if (s.equals("jsml")) return JSML;
	    if (s.equals("cdata")) return COLUMNDATA;
	    if (s.equals("csv")) return CSVDATA;
	    if (s.equals("bul")) return I4BULL;
	    if (s.equals("tac")) return TAC;
	    if (s.equals("par")) return PARSET;
	    return UNKNOWN;
	}

	// internal state
	private String origStr;
	private File file;
	private URL url;
	private int fileType;
	
	// constructors
	public JSReadable(URL u) { 
	    url = u; 
	    fileType = fileType(fileSuffix());
	}
	public JSReadable(File f) { 
	    file = f; 
	    fileType = fileType(fileSuffix());
	}
	public JSReadable(String s) throws Xcept {
	    origStr = s;
	    if (s.startsWith("http://")
	    ||  s.startsWith("ftp://")) {
	    	try {
	    	    url = new URL(s);
		} catch (MalformedURLException e) {
		    throw Xcept.wrap(e);
		}
	    } else {
	        file = new File(s);
	    }
	    fileType = fileType(fileSuffix());
 	}

	// set relative readable WRT another readable
	// return true if modified,  false otherwise
	public boolean setRelativeTo(JSReadable base) {
	    if (isURL()) return false;

	    // base is a file?
	    if (base.isFile()) {
	    	if (file.isAbsolute()) return false;
		file = new File(base.file(), file.getPath()); 
		return true;
	    }

	    // base is an URL
	    try {
		String path = (origStr != null) ? 
		    origStr : file.getPath();
	        url = new URL(base.url(), path);
	    	file = null;
	    	return true;
	    } catch (MalformedURLException e) {
	    	return false;
	    }
	}	

	// read content
	public byte[] readBytes() throws Xcept {
	    return (isFile()) ?
		UtilIO.readBytes(file) : UtilIO.readBytes(url);
	}
	public String readText() throws Xcept {
	    return (isFile()) ?
	    	UtilIO.readText(file) : UtilIO.readText(url);
	}
	public String readFirstLine() throws Xcept {
	    if (! isFile()) throw new Xcept(
	    	"File required: found " + this);
	    try {
	    	BufferedReader rdr = new BufferedReader(
	    	   new FileReader(file));
	    	String s = rdr.readLine();
	    	rdr.close();
	    	return s;
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
 	    }
	}

	// content warning
	public boolean isContentDubious() throws Xcept {
	    if (fileType != ANTIMONY) return false;
	    AntimonyCheck sb = new AntimonyCheck(readText());
	    return ! sb.isProbablyAntimony();
	} 
	
	// simple query
	public File file() { 
	    return (isFile()) ? file : new File(url.getPath());
	}
	public URL url() { return url; }
	public boolean isFile() { return file != null; }
	public boolean isURL() { return url != null; }

	// type query
	public int fileType() { return fileType; }
	public boolean isProject() { return fileType == PROJECT; }
	public boolean isModel() {
	    switch (fileType) {
	    case MML:
	    case JAVA:
	    case ANTIMONY:
	    case XML:
	    	return true;
	    }
	    return false;
	}
	public boolean isDataSet() {
	    switch (fileType) {
	    case JSML:
	    case COLUMNDATA:
	    case CSVDATA:
	    case TAC:
	    case I4BULL:
	    	return true;
	    }
	    return false;
	}
	public boolean isParSet() {
	    return fileType == PARSET;
	}
	    
	// String query
	public String fileSuffix() { 
	    return UtilIO.fileSuffix(file());
	}
	public String fileBaseName() {
	    return UtilIO.fileBaseName(file());
	}
	public String prettyPath() {
	    if (isURL()) return toString();
	    return UtilIO.prettyPath(file);
	}
	public String toString() {
	    if (isFile()) 
	    	return file.toString();
	    else
	    	return url.toString();
	}
	public String diagInfo() {
	    if (isFile())
	    	return "File " + file;
	    else
	    	return "URL " + url;
 	}
	public String truncString(int maxc) {
	    if (maxc < 4) maxc = 4;
	    String s = toString();
	    if (s.length() <= maxc) return s;
	    return "..." + s.substring(3);
	}
	
	// same File/URL
	public boolean equals(Object obj) {
	    if (! (obj instanceof JSReadable)) return false;
	    JSReadable r = (JSReadable) obj;
	    if (file != null) return file.equals(r.file);
	    if (url != null) return url.equals(r.url);
	    return false;
	}

	// JSReadable.List
	public static class List extends ArrayList<JSReadable> {
	    public List(int n) { super(n); }
	    public List() { super(); }
	    public JSReadable readable(int i) {
	    	return (JSReadable) get(i);
	    }
	    public int indexOf(JSReadable r) {
	    	for (int i=0; i<size(); i++) 
		    if (readable(i).equals(r)) 
		    	return i;
	 	return -1;
 	    }
	}
	
	// test harness
	public static void main(String[] args) throws Xcept {
	    JSReadable base = null;
	    for (int i=0; i<args.length; i++) {
	    	JSReadable r = new JSReadable(args[i]);
		System.err.println("Reading " + r.diagInfo());
		if (base != null) {
		    boolean b = r.setRelativeTo(base);
		    System.err.println("  changed=" + b + " " +
		    	r.diagInfo());
		}
		base = r;
	    }
	}
}
