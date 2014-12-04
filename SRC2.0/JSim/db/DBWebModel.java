/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// load NSR WebModel database into MySQL

package JSim.db;

import JSim.util.*;
import java.io.*;
import java.util.*;
import org.w3c.dom.*;
import java.sql.*;

public class DBWebModel {
 	private File rootDir; // model root
	private File nsrDir;  // NSR webmodel root
	private String dbName; // typically "model"
	private String userName; // mysql model admin username
	private Document modelDoc, keywordDoc, locDoc, refDoc;
	private Hashtable<String, Element> modelHash, keywordHash, locHash,
	    refHash;
	private Statement stmt;

	// constructor
	public DBWebModel(File rootDir, File nsrDir, String dbName, String userName)
	throws Exception {
	    this.rootDir = rootDir;
	    this.nsrDir = nsrDir;
	    this.dbName = dbName;
	    this.userName = userName;

	    // build hash tables
	    modelDoc = UtilXML.parse(new File(nsrDir, "NSR_modelDB.xml"));
	    keywordDoc = UtilXML.parse(new File(nsrDir, "NSR_keywords.xml"));
	    locDoc = UtilXML.parse(new File(nsrDir, "NSR_location.xml"));
	    refDoc = UtilXML.parse(new File(nsrDir, "NSR_references.xml"));
	    modelHash = makeHash(modelDoc);
	    keywordHash = makeHash(keywordDoc);
	    locHash = makeHash(locDoc);
	    refHash = makeHash(refDoc);

	    // init DB
	    Class.forName("com.mysql.jdbc.Driver");
	    String uri =
	       "jdbc:mysql://localhost/" + 
	       dbName + "?" + "user=" + userName;
	    Connection conn = DriverManager.getConnection(uri);
	    stmt = conn.createStatement();
	
	    // remove existing NSR models
	    executeUpdate("DELETE FROM code WHERE model_id IN " +
	        "(SELECT model_id FROM model WHERE archive='NSR')");
	    executeUpdate("DELETE FROM model_author WHERE model_id IN " +
	        "(SELECT model_id FROM model WHERE archive='NSR')");
	    executeUpdate("DELETE FROM model_keyword WHERE model_id IN " +
	        "(SELECT model_id FROM model WHERE archive='NSR')");
	    executeUpdate("DELETE FROM model_reference WHERE model_id IN " +
	        "(SELECT model_id FROM model WHERE archive='NSR')");
	    executeUpdate("DELETE FROM model_ontology_tag WHERE model_id IN " +
	        "(SELECT model_id FROM model WHERE archive='NSR')");
	    executeUpdate("DELETE FROM model WHERE archive='NSR'");

	    // add all models
	    ArrayList<String> ids = new ArrayList<String>(modelHash.keySet());
	    Collections.sort(ids);
	    for (int i=0; i<ids.size(); i++) {
	        process(ids.get(i));
	    }

	    stmt.close();
	}
	
	// create hash lookup from Document
	private Hashtable<String, Element> makeHash(Document doc) {
	    Hashtable<String, Element> hash = new Hashtable<String, Element>();
	    addHash(hash, doc.getDocumentElement());
	    return hash;
	}
	
	// add model_number Element parents to hash
	private void addHash(Hashtable<String, Element> hash, Element e) {
	    NodeList nodes = e.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
	    	Node node = nodes.item(i);
		if (! (node instanceof Element)) continue;
		Element elem = (Element) node;
		if (elem.getNodeName().equals("model_number")) {
		    String id = UtilXML.getText(elem);
		    hash.put(id, e);
  	        } else { 
		   addHash(hash, elem);
		}
	    }
	}

	// process one model ID
	private void process(String id) throws Exception {
	    System.err.println("---- processing " + id);
	    Element emod = modelHash.get(id);
	    Element ekey = keywordHash.get(id);
	    Element eloc = locHash.get(id);
	    Element eref = refHash.get(id);
	    String name = getText(emod, "model_name");
	    String desc = getText(emod, "model_description");
	    String auth = getText(emod, "model_author");
	    if (auth.equals("None"))
	       auth = "";
	    String loc = getText(eloc, "location");
	    if (loc.startsWith("/jsim/models"))
	    	loc = loc.substring(12);
	    if (loc.startsWith("/"))
	    	loc = loc.substring(1);
	    StringList keys = getTexts(ekey, "key_word");
	    StringList refs = getTexts(eref, "reference");
	    String url = "http://physiome.org/jsim/models/" + loc;
	    String model_id = "NSR" + id;

	    // add model
	    executeUpdate(
	        "INSERT INTO model(model_id, archive, name, description, page_url, base_dir) "
		+ "VALUES (\"" + model_id + "\",\"NSR\",\"" + name + "\",\"" 
		+ desc + "\",\"" + url + "\",\"" + loc + "\")"); 

	    // add author
	    if (! Util.isBlank(auth)) executeUpdate(
	    	"INSERT INTO model_author(model_id, author_name) "
		+ "VALUES (\"" + model_id + "\",\"" + auth + "\")");
	    	
	    // add keywords
	    for (int i=0; i<keys.size(); i++) 
	    	executeUpdate(
	    	    "INSERT IGNORE INTO model_keyword(model_id, keyword) "
		    + "VALUES (\"" + model_id + "\",\"" + keys.str(i) + "\")");
	    	
	    // add references
	    for (int i=0; i<refs.size(); i++) executeUpdate(
	    	"INSERT INTO model_reference(model_id, reference_text) "
		+ "VALUES (\"" + model_id + "\",\"" + refs.str(i) + "\")");

	    // add code (.proj in directory)
	    int p = loc.startsWith("/") ? 1 : 0;
	    File mdir = new File(rootDir, loc.substring(p));
	    System.err.println("mdir=" + mdir + " isDir=" + mdir.isDirectory());
	    File[] files = mdir.listFiles();
	    if (files == null) files = new File[0];
	    for (int i=0; i<files.length; i++) {
		File f = files[i];
		String fname = f.getName();
		if (! fname.endsWith(".proj")) continue;
		String variant = fname.substring(0, fname.length()-5);
	    	System.err.println("  code=" + fname + " v=" + variant);
		executeUpdate(
		    "INSERT INTO code(model_id, variant, code_type, file_name) "
		    + "VALUES ('" + model_id + "','" + variant + "','Project', '"
		    + fname + "')");
	    }
	    
	}
	    
	// execute update using stmt
	private int executeUpdate(String sql) throws Exception {
	    System.err.println("  exec: " + sql);
	    int ct = stmt.executeUpdate(sql);
	    // System.err.println("    ct=" + ct);
	    return ct;
	}

	// get texts from matching elements
	private StringList getTexts(Element base, String tag) {
	    StringList texts = new StringList();
	    if (base == null) return texts;
	    NodeList elems = base.getElementsByTagName(tag);
	    for (int i=0; i<elems.getLength(); i++) {
	    	Element e = (Element) elems.item(i);
		String text = UtilXML.getText(e);
		text=text.replaceAll("\"", "'");
		text = text.trim();
		if (! Util.isBlank(text))
		    texts.add(text);
	    }
	    return texts;
	}
	
	// get text from matching elements
	private String getText(Element base, String tag) {
	    StringList texts = getTexts(base, tag);
	    return texts.isEmpty() ? "" : texts.get(0);
	}
	

	// test harness
	public static void main(String[] args) throws Exception {
	    if (args.length != 4) throw new Xcept(
	    	"Usage: DBWebModel Model-Root-dir NSR-dir dbName userName");
	    new DBWebModel(new File(args[0]), 
	    	new File(args[1]), args[2], args[3]);
	}
}

