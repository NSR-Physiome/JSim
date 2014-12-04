/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XML utility routines

package JSim.util;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; // XML DOM document
import org.w3c.dom.Element; 
import org.w3c.dom.Comment; 
import org.w3c.dom.CharacterData; 
import org.w3c.dom.Node; 
import org.w3c.dom.NodeList; 
import org.apache.xerces.parsers.DOMParser;
import org.apache.xerces.dom.DOMImplementationImpl;
import org.w3c.dom.DOMImplementation;
import org.xml.sax.InputSource;

public class UtilXML {

	// parse XML file
	public static Document parse(File f) throws Xcept {
	    String s = UtilIO.readText(f);
	    return parse(s);
	}

	// parse XML content string
	public static Document parse(String s) throws Xcept {
	    try {
	    	DOMParser parser = new DOMParser();
		StringReader rdr = new StringReader(s);
		InputSource src = new InputSource(rdr);
	    	parser.parse(src);
	    	Document doc = parser.getDocument();
		return doc;

	    } catch (Exception e) {
		throw new Xcept("" + e);
	    }
	}

	// new XML Document with root name
	public static Document createDoc(String rootName) {
	    DOMImplementation domImpl = new DOMImplementationImpl();
	    Document doc = domImpl.createDocument(null, rootName, null);
	    return doc;
	}

	// remove all but single ID from element
	public static void removeElementsExceptID(Element e, String id)
	throws Xcept {
	    NodeList cns = e.getChildNodes();
	    for (int i=0; i<cns.getLength(); i++) {
	    	Node cn = cns.item(i);
		if (! (cn instanceof Element)) continue;
		String cnid = ((Element) cn).getAttribute("id");
		if (! cnid.equals(id))
		    e.removeChild(cn);
	    }
	}

	// get text value numeric value
	public static double getValueText(Element elem) throws Xcept {
	    String text = getText(elem);
	    double f = Util.toDouble(text);
	    if (Double.isNaN(f)) throw new Xcept(
	        "Illegal numeric constant <" + text + ">");
	    return f;
	}

	// get Text from XML Element
	public static String getText(Element elem) {
	    StringBuffer buf = new StringBuffer();
	    NodeList nodes = elem.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
		Node node = nodes.item(i);
		if (node instanceof Comment) continue;
		if (! (node instanceof CharacterData)) continue;
		String text = ((CharacterData) node).getData();
		buf.append(text);
	    }
	    return buf.toString().trim();
	}

	// get nexted Text from XML Element
	public static String getNestedText(Element elem) {
	    StringBuffer buf = new StringBuffer();
	    NodeList nodes = elem.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
		Node node = nodes.item(i);
		String text = "";
		if (node instanceof Comment) 
		    text = "";
		else if (node instanceof CharacterData) 
		    text = ((CharacterData) node).getData();
	 	else if (node instanceof Element) 
		    text = getNestedText((Element) node);
		buf.append(text);
	    }
	    return buf.toString().trim();
	}

	// get element list
	public static ArrayList<Element> getElements(Element e) {
	    ArrayList<Element> elems = new ArrayList<Element>();
	    NodeList nodes = e.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) 
	    	if (nodes.item(i) instanceof Element)
		    elems.add((Element) nodes.item(i));
	    return elems;
	}

	// get Node list
	public static ArrayList<Node> getNodes(Node node) {
	    ArrayList<Node> nlist = new ArrayList<Node>();
	    NodeList nodes = node.getChildNodes();
	    if (nodes == null) return nlist;
	    for (int i=0; i<nodes.getLength(); i++) 
		nlist.add(nodes.item(i));
	    return nlist;
	}
	
	// get single required element
	public static Element getUniqueElement(Element e, String name) 
	throws Xcept {
	    ArrayList<Element> elems = getElements(e);
	    if (elems.size() != 1) throw new Xcept(
	    	"Expected single <" + name + "> element within <" +
		    e.getNodeName() + ">");
  	    return elems.get(0);
	}

	// get nested Element based on series of tags
	public static Element getNestedElement(Element e, String[] tags) {
	    for (int i=0; i<tags.length; i++) {
	    	NodeList nodes = e.getElementsByTagName(tags[i]);
		if (nodes.getLength() == 0) return null;
		e = (Element) nodes.item(0);
	    }
	    return e;
	}

	// test harness for getText
	public static void main(String[] args) throws Exception {
	    File f = new File(args[0]);
	    Document doc = parse(f);
	    Element root = doc.getDocumentElement();
	    String[] tags = new String[args.length-1];
	    for (int i=1; i<args.length; i++) 
	    	tags[i-1] = args[i];
	    Element e = getNestedElement(root, tags);
	    String text = getText(e);
	    System.out.println("TEXT={" +  text + "}");
	}
}
