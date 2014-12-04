/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Source code + include modules

// Needed for future:
//   CellML v1.1 imports, SBML imports, Antimony imports
//   Project file modules,  MML modularity
//
// Unresolved questions:
//   http: refs can't tell if valid, so defer to server?


package JSim.data;

import java.io.*;
import java.net.*;
import java.util.*;
import JSim.util.*;
import org.w3c.dom.*;

public class SourceBundler {
	private Path path;
	private URI mainURI;  // main program URI
	private Hashtable<URI, URIInfo> uriInfos;

	// source text type constants
	public static final int UNKNOWN = 0;
	public static final int MML = 1;
	public static final int ANTIMONY = 2;
	public static final int JAVA = 3; // JSim java run-time
	public static final int XML = 4; // either SBML or CellML
	public static final int SBML = 5;
	public static final int CELLML = 6;

	// constructor
	public SourceBundler(String pathString) 
	throws Xcept {
	    if (pathString == null) pathString = "";
	    try {
	    	path = new Path(pathString);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	    uriInfos = new Hashtable<URI, URIInfo>();
	    // others?
	}

	// load main code from file
	public void loadMain(URI uri) throws Xcept {
	    URIInfo info = makeInfo(uri);
	    process(info);
	}

	// load main code from in-memory String
	public void loadMain(URI uri, int textType, String text) 
	throws Xcept {
	    URIInfo info = new URIInfo(uri);
	    info.type = textType;
	    info.text = text;
	    process(info);
	}

	// process imports 
	protected void process(URIInfo info) throws Xcept {
	    if (mainURI == null) mainURI = info.uri;
	    uriInfos.put(info.uri, info);
	    StringList imps = getImports(info);
System.err.println("=== " + info + " imports=" + imps);
	    try {
	    	for (int i=0; i<imps.size(); i++) 
	    	    processImport(info, imps.str(i));
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// process one import
	protected void processImport(URIInfo pinfo, String impName)
	throws Exception {
	    URI uri = new URI(impName);

	    // MML imports default to .mod
	    if (pinfo.type == MML
	    && getSuffixType(uri) == UNKNOWN)
	    	uri = new URI(impName + ".mod");
		
	    // if absolute URI, check if exists
	    if (uri.isAbsolute()) {
	    	if (exists(uri))
		    addImport(pinfo, impName, uri);
		else throw new Xcept(
		   "Import " + uri + " in " + 
		   pinfo.uri + " not found.");
	    }

	    // check relative to parent
	    URI u = pinfo.uri.resolve(uri);
	    if (exists(u))
	    	addImport(pinfo, impName, u);
		
 	    // check path
	    for (int i=0; i<path.size(); i++) {
	    	URI puri = path.get(i);
		u = puri.resolve(uri);
		if (exists(u)) {
		    addImport(pinfo, impName, u);
		    return;
		}
	    }
	    throw new Xcept( "Import " + uri + " in " + 
		   pinfo.uri + " not found in path " + path);
	} 

	// add import
	private void addImport(URIInfo info, String importName,
	URI uri) throws Exception {
	    info.importURIs.put(importName, uri);
	    URIInfo iinfo = makeInfo(uri);
	    process(iinfo);
	}

	// does this URI exist?
	public boolean exists(URI uri) {
	    return true;
	}

	// make URIInfo
	protected URIInfo makeInfo(URI uri) throws Xcept {
	    URIInfo info = new URIInfo(uri);
	    info.type = getSuffixType(uri);
	    try {
	    	URL url = uri.toURL();
	    	info.text = UtilIO.readText(url);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	    if (info.type == XML)
	    	info.type = getXMLType(info.doc());
	    return info;
	}

	// suffix type from URI
	public static int getSuffixType(URI uri) throws Xcept {
	    String sfx = getSuffix(uri);
	    if (sfx.equals("mod")) return MML;
	    if (sfx.equals("mml")) return MML;
	    if (sfx.equals("xml")) return XML;
	    if (sfx.equals("cellml")) return CELLML;
	    if (sfx.equals("sbml")) return SBML;
	    if (sfx.equals("txt")) return ANTIMONY;
	    if (sfx.equals("java")) return JAVA;
	    return UNKNOWN;
	}

	// resolve XML type based on Document
	public static int getXMLType(Document doc) throws Xcept {
	    String rootName = doc.getDocumentElement().getNodeName();
	    if (rootName.equals("sbml")) return SBML;
	    if (rootName.equals("model")) return CELLML;
	    throw new Xcept(
	    	"Unknown XML format: rootName=" + rootName);
	}
	

	// get Suffix from URI path
	public static String getSuffix(URI uri) {
	    String s = uri.getPath();
	    if (s == null) return null;
	    int inx = s.lastIndexOf('.');
	    if (inx < 0) return null;
	    String sfx = s.substring(inx+1);
	    if (Util.isBlank(sfx)) return null;
	    return sfx;
	}

	// is XML type?
	public static boolean isXML(int type) {
	    switch(type) {
	    case XML:
	    case SBML:
	    case CELLML:
	    	return true;
	    default:
	    	return false;
	    }
	}
	
	// get imports from text
	public static StringList getImports(URIInfo info)
	throws Xcept {
	    switch (info.type) {
	    case MML: 
	    	return getMMLImports(info.text);
	    case ANTIMONY: 
	    	return getAntimonyImports(info.text);
	    case CELLML: 
	    	return getXMLImports(info.doc(), "import", "href");
	    case SBML:
	    	return getXMLImports(info.doc(), "externalModelDefinition", "source");
	    case JAVA: 
	    	return new StringList();
	    default: 
	    	throw new Xcept(
		    "SourceBundle.getImport: unsupported type=" +
		    info.type);
	    }
	}

	// get imports from MML
	public static StringList getMMLImports(String text) {
	    text = Util.stripComments(text, "{{", "}}");
	    return getAntimonyImports(text);
	}

	// get imports from Antimony
	public static StringList getAntimonyImports(String text) {
	    text = Util.stripComments(text, "/*", "*/");
	    text = Util.stripComments(text, "//", "\n");
	    StringTokenizer stok = new StringTokenizer(text, ";\n\r\t ");
	    StringList l = new StringList();
	    while (stok.hasMoreTokens()) {
		String tok = stok.nextToken();
		if (! tok.equals("import")) continue;
		if (! stok.hasMoreTokens()) break;
		tok = stok.nextToken();
		tok = Util.stripQuotes(tok);
		l.addUniq(tok);
	    }
	    return l;
	}
	
	// get XML import with known search parms
	private static StringList getXMLImports(Document doc,
	String tagName, String attrSfx) {
	    Element root = doc.getDocumentElement();
	    StringList l = new StringList();
	    NodeList es = root.getElementsByTagName(tagName);
	    for (int i=0; i<es.getLength(); i++) {
	    	Element e = (Element) es.item(i);
		NamedNodeMap map = e.getAttributes();
		for (int j=0; j<map.getLength(); j++) {
		    if (! (map.item(j) instanceof Attr)) continue;
		    Attr attr = (Attr) map.item(j);
		    String aname = attr.getName();
		    if (aname.equals(attrSfx) 
		    || aname.endsWith(":" + attrSfx))
		    	l.addUniq(attr.getValue());
		}
	    }
	    return l;
	}

	// Path to search
	public static class Path extends ArrayList<URI> {
	    public Path(String s) throws Exception {
	    	super();
		StringTokenizer stok = 
		    new StringTokenizer(s, File.pathSeparator);
		while (stok.hasMoreTokens()) {
		    String tok = stok.nextToken();
		    File f = new File(tok);
		    f = f.getCanonicalFile();
		    add(f.toURI());
		}
System.err.println("path=" + this);
	    }
	}
		    
	// URI Info class
	public static class URIInfo {
	    public URI uri; // info for this URI
	    public int type; // text type (UNKNOWN if not found)
	    public String text; // text
	    private Document doc; // if XML
	    public Hashtable<String, URI> importURIs;

	    public URIInfo(URI uri) { 
	    	this.uri = uri; 
		importURIs = new  Hashtable<String, URI>();
	    }

	    // String query
	    public String toString() { 
	    	return "" + uri + " type=" + type;
	    }
	    
	    // load Document, if appropriate
	    public Document doc() throws Xcept {
		if (doc != null) return doc;
	    	try {
		    doc = UtilXML.parse(text);
		} catch (Exception e) {
		    throw Xcept.wrap(e);
		}
		return doc;
	    }
	}

	// path test harness
	public static void main(String[] args) throws Exception {
	    SourceBundler b = new SourceBundler(args[0]);
	}    

	// URI resolve test harness
	public static void mainOLD2(String[] args) throws Exception {
	    URI b = new URI(args[0]);
	    URI u = new URI(args[1]);
	    URI bu = b.resolve(u);
	    System.out.println("bu=" + bu );
	}

	// Bundle test harness
	public static void mainOLD(String[] args) throws Exception {
	    for (int i=1; i<args.length; i++) {
	    	File f = new File(args[i]);
		URI uri = f.toURI();
		SourceBundler b = new SourceBundler(args[0]);
		b.loadMain(uri);
	    }
	}
}

