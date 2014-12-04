/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Help data base

package JSim.gui;

import java.io.*;
import java.net.*;
import java.util.*;
import java.text.*;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.w3c.dom.CharacterData;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GHelpDB {
	private static String date; // change date for this program run
	private HItem.NList items; // items in DB

	// constructor 
	public GHelpDB(Element base) {
	    if (base == null) {
		items = new HItem.NList(64);
		return;
	    }
	    NodeList list = base.getChildNodes();
	    items = new HItem.NList(list.getLength());
	    for (int i=0; i<list.getLength(); i++) {
		if (! (list.item(i) instanceof Element))
		    continue;
		Element e = (Element) list.item(i);
		String ename = e.getNodeName();
		if (! ename.equals("item")) continue;
		String key = e.getAttribute("name");
		String date = e.getAttribute("date");
	    	StringBuffer buf = new StringBuffer();
	    	NodeList clist = e.getChildNodes();
	    	for (int j=0; j<clist.getLength(); j++) {
		    if (! (clist.item(j) instanceof CharacterData))
		    	continue;
		    CharacterData c = (CharacterData) clist.item(j);
		    buf.append(c.getData());
		}
		while (buf.length() > 0 
		&& buf.charAt(0) == '\n')
		    buf.deleteCharAt(0);
		while (buf.length() > 0 
		&& Character.isWhitespace(buf.charAt(buf.length()-1)))
		    buf.deleteCharAt(buf.length()-1);
	    	items.add(new HItem(key, date, buf.toString()));
	    }  
	}

	// read from an URL
	public static GHelpDB read(URL url) throws Xcept {
	    String txt = UtilIO.readText(url);
	    Document doc = UtilXML.parse(txt);
	    Element root = doc.getDocumentElement();
	    if (! root.getNodeName().equals("JSimHelp"))
		throw new Xcept("invalid root element");
	    return new GHelpDB(root);
	}

	// get editable text for key
	public String getEditableText(String key) {
	    HItem item = items.hitem(key);
	    return (item==null) ? null : item.text;
	}

	// get fixed text for key
	public String getFixedText(String key) {
	    return null;
	}

	// set text fo key
	public void setEditableText(String key, String text) {
	    if (date == null) date = 
		(new SimpleDateFormat("yyyy/MM/dd")).format(new Date());
	    HItem item = items.hitem(key);
	    if (item == null) {
		if (Util.isBlank(text)) return;
		items.add(new HItem(key, date, text));
	    } else {
		if (text.equals(item.text)) return;
		item.date = date;
		item.text = text;
	    }	    
	}

	// export XML
	public void exportXML(Document doc, Element base) 
	throws Xcept {
	    for (int i=0; i<items.size(); i++) {
	        HItem item = items.hitem(i);
		if (Util.isBlank(item.text)) continue;
		Element e = doc.createElement("item");
		e.setAttribute("name", item.key);
		if (! Util.isBlank(item.date))	
		    e.setAttribute("date", item.date);
		Text text = doc.createTextNode(item.text);
		e.appendChild(text);
		base.appendChild(e);
	    }
	}

	// save
	public void save(File file) throws Xcept {
	    try {
		Document doc = UtilXML.createDoc("JSimHelp");
		exportXML(doc, doc.getDocumentElement());
		FileWriter fwrt = new FileWriter(file);
		XMLWriter xwrt = new XMLWriter();
		xwrt.write(doc, fwrt);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// GHelpDB.HItem
	public static class HItem implements Named {
	    public String key;
	    public String date; // modification date
	    public String text;

	    // constructor
	    public HItem(String k, String d, String t) {
		key = k;
		date = d;
		text = t;
	    }

	    // query
	    public String name() { return key; }
	    public String diagInfo() { return "GHelpDB.HItem " + key; }

	    // NamedList
	    public static class NList extends NamedList {
		public NList(int n) { super(n); }
		public HItem hitem(int i) { return (HItem) get(i); }
		public HItem hitem(String n) { return (HItem) getByName(n); }
	    }
	}

	// mainline test program
	public static final void main(String[] args) throws Exception {
	    GHelpDB db = new GHelpDB(null);
	    URL url = db.getClass().getResource(args[0]);
	    db = read(url);
	    File f = new File(args[1]);
	    db.save(f);
	}
	    
}
