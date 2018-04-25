/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// named item in project

package JSim.project; import JSim.aserver.*;

import java.io.*;
import java.util.ArrayList;
import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.CharacterData;
import org.w3c.dom.NamedNodeMap;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.parsers.ParserConfigurationException;

public abstract class PNamed implements Named, NamedVal.Query {
	protected PNamed parent; // parent item
	protected String name;	// local name
	protected PNamed.NList children; // children
	private String desc; // free-form description
	public StringControl descCntl; // only if user-editable

	// constructor
	public PNamed(PNamed p, String n) throws Xcept {
	    parent = p;
	    name = n;
	    if (! name.equals(safeName(name))) throw new Xcept(
		"Illegal name \"" + name +
		"\": only letters, numbers, underscores " +
		" and periods are allowed.");
	    if (parent != null) 
		parent.addChild(this);
	}

	// query
	public String name() { return name; }
	public String toString() { return fullname(); }
	public PNamed parent() { return parent; }
	public String desc() { 
	    return (descCntl == null) ? 
		desc : descCntl.stringVal();
	}
	public String fullname() {
	    if (parent == null) return name;
	    return parent.fullname() + "." + name;
	}
	public String dotname() {
	    if (parent == null) return ".";
	    if (parent instanceof Project)
		return "." + name;
	    return parent.dotname() + "." + name;
	}
	public Project project() {
	    if (parent == null) 
		return (Project) this;
	    return parent.project();
	}
	public PModel pmodel() {
	    if (this instanceof PModel) 
		return (PModel) this;
	    return parent.pmodel();
	}
	public PApplication appl() { return project().appl(); }
	public ASServer server() { return appl().server(); }
	public boolean isLocked() { return false; }

	// dick with desc
	public void setDesc(String s) throws Xcept {
	    if (descCntl == null) 
		desc = s;
	    else
		descCntl.setVal(s);
	}   
	protected void addDescCntl() throws Xcept {
	    descCntl = new StringControl(this, "desc");
	}

	// keeping the kids in line
	public void addChild(PNamed c) throws Xcept {
	    if (children == null) 
		children = new PNamed.NList(8);
	    if (! children.add(c)) 
		throw new Xcept(this, c, "Duplicate name");
	}
	public int nChild() {
	    if (children == null) return 0;
	    return children.size();
	}	
	public PNamed child(int i) {
	    if (children == null) return null;
	    return children.pnamed(i);
	}
	public PNamed child(String n) {
	    if (children == null) return null;
	    return children.pnamed(n);
	}
	public PNamed.List children(Class clss) {
	    PNamed.List list = new PNamed.List(4);
	    for (int i=0; i<nChild(); i++) {
	        PNamed c = child(i);
		if (clss.isInstance(c))
		    list.add(c);
	    }
	    return list;
	}
	public PNamed ancestor(Class clss) {
	    PNamed p = this;
	    while (p != null) {
		if (clss.isInstance(p)) return p;
		p = p.parent();	
	    }
	    return p;
	}
	public PNamed.List descendants(Class clss) {
	    PNamed.List list = new PNamed.List(4);
	    for (int i=0; i<nChild(); i++) 
	        child(i).addDescendants(clss, list);
	    return list;
	}
	public void addDescendants(Class clss, PNamed.List list) {
	    if (clss.isInstance(this))
		list.add(this);
	    for (int i=0; i<nChild(); i++) 
	        child(i).addDescendants(clss, list);
	}	   
	public void addModelExprs(PModel model, StringList exprs) {
	    for (int i=0; i<nChild(); i++) 
	    	child(i).addModelExprs(model, exprs);
 	}

	public boolean remove(String n) throws Xcept {
	    PNamed c = child(n);
	    if (c == null) return false;
	    return remove(c);
	}
	public boolean remove(PNamed c) throws Xcept {
	    if (c != child(c.name())) return false;
	    children.remove(c.name());
	    if (c instanceof FuncGen && pmodel() != null)
	    	pmodel().vars().funcGenNamesChanged = true;
	    return true;
	}

	// recursive search for nested Child
	public PNamed nestedChild(String name) {

	    //initialize search
	    if (Util.isBlank(name)) return null;
	    PNamed pnamed = this;
	    String n = name;

	    // % -> top,  . -> parent
	    if (n.charAt(0) == '%') {
		pnamed = project();
		n = n.substring(1);
	    } else while (n.charAt(0) == '.') {
		pnamed = pnamed.parent();
		if (pnamed == null) return null;
		n = n.substring(1);
	    }

	    // loop forever
	    while (true) {
		PNamed p = pnamed.child(n);
		if (p != null) return p;
		int inx = n.indexOf(".");
		if (inx<0) return null;
		String n1 = n.substring(0,inx);
		pnamed = pnamed.child(n1);
		if (pnamed == null) return null;
		n = n.substring(inx+1);
	    }
	}
	    
	// NamedVal for a given name n
	public NamedVal namedVal(String n) throws Xcept {
	    PNamed child = nestedChild(n);
	    if (child instanceof Control) 
	    	return child.namedVal(n);
	    throw new Xcept(this, "No such child " + n);
	}
	    		
	// unique name
	public String newChildName(String base, boolean sfxreqd) {

	    // if no suffix,  and doesn't exist,  use base
	    if (!sfxreqd) 
	        if (children == null || children.getByName(base) == null)
		    return base;

	    // remove trailing _99, if any
	    int j = base.lastIndexOf('_');
	    if (j>0) {
		boolean cut = true;
		for (int k=j+1; k<base.length(); k++) 
		    if (! Character.isDigit(base.charAt(k)))
			cut = false;
	        if (cut) base = base.substring(0,j);
	    }

	    // look for unique name
	    for (int i=1; true; i++) {
		String n = base + "_" + i;
		if (children == null || children.getByName(n) == null)
		    return n;
	    }
	}

	// name with bad chars removed
	//    because of legacy proj files
	//    must allow periods and numeric starting chars
	public static String safeName(String s) {
	    return Util.safeName(s);
	}

	// revalidate (StringControls)
	public void revalidate() {
	    for (int i=0; i<nChild(); i++) 
		child(i).revalidate();
	}

	// set to defaults
	public void setDefaults() throws Xcept {
	    for (int i=0; i<nChild(); i++) 
		child(i).setDefaults();
	}

	// a child has been changed
	protected void childChanged(PNamed child) throws Xcept {
	    if (parent != null) 
	    	parent.childChanged(child); // this?
	}

	// rename
	public void rename(String n) throws Xcept {
	    if (this instanceof PModel ||
		this instanceof PDataSet)
		project().renameXref(this, n);
	    if (this instanceof FuncGen && pmodel() != null)
	    	pmodel().vars().funcGenNamesChanged = true;
	    if (parent != null) 
		parent.children.rename(name, n);
	    name = n;
	}

	// rename xrefs to another PNamed
	public void renameXref(PNamed p, String newname) 
	throws Xcept {
	    for (int i=0; i<nChild(); i++) 
		child(i).renameXref(p, newname);
	}

	// export XML
	public String xmlLabel() { return "PNamed"; }
	public Element exportXML(Document doc) throws Xcept {
	    Element e = doc.createElement(xmlLabel());
	    e.setAttribute("name", name());
	    exportExtraXML(e);
	    for (int i=0; i<nChild(); i++) {
		PNamed child = child(i);
		Element e1 = child.exportXML(doc);
		if (e1 != null)
		   e.appendChild(e1);
	    }
      	    return e;
	}
	public void exportExtraXML(Element e)
	throws Xcept { }

	// write to output
	public void writeXML(OutputStream out) throws Xcept {
	    Document doc = exportXMLDoc();
	    XMLWriter wrt = new XMLWriter();
	    wrt.write(doc, out);
	}
	public void writeXML(File f) throws Xcept {
	    Document doc = exportXMLDoc();
	    XMLWriter wrt = new XMLWriter();
	    wrt.write(doc, f);
	}

	// create JSim XML document
	public Document createXMLDoc() {
	    Document doc = UtilXML.createDoc("JSim");
	    Element root = doc.getDocumentElement();
	    root.setAttribute("version", Util.version());
	    return doc;
	}

	// create DOM Document containing this node
	public Document exportXMLDoc() throws Xcept {
	    Document doc = createXMLDoc();
	    Element root = doc.getDocumentElement();
	    Element e = exportXML(doc);
	    root.appendChild(e);
	    return doc;
	}

	// create DOM element for this node
	public Element exportXML() throws Xcept {
	    Document doc = exportXMLDoc();
	    Element root = doc.getDocumentElement();
	    NodeList nodelist = root.getChildNodes();
	    if (nodelist == null) return null;
	    for (int i=0; i<nodelist.getLength(); i++) {
		Node node = nodelist.item(i);
		if (node instanceof Element) 
		    return (Element) node;
	    }
	    return null;
	}

	// import XML element into this PNamed
	//    does not throw Xcept
	//    Xcepts s/b caught at low level to allow partial import
	public void importXML(Document doc) {
	    importXML(doc.getDocumentElement());
	}
	public void importXML(Element e) {
	    if (! importXMLLabelMatch(e)) return;
	    importXMLForce(e);
	}
	public void importXMLForce(Element e) {
	    NodeList nlist = e.getChildNodes();
	    for (int i=0; i<nlist.getLength(); i++) {
		if (! (nlist.item(i) instanceof Element)) continue;
		Element c = (Element) nlist.item(i);
		importXMLChild(c);
	    }
	}



	// check label matches
	public boolean importXMLLabelMatch(Element e) {
	    if (! (e.getNodeName().equals(xmlLabel()))) {
		importXMLMessage(diagInfo() + " XML label mismatch: " +
		    e.getNodeName() + " vs " + xmlLabel());
		return false;
	    }
	    return true;
	}

	// import XML element into matching PNamed child
	public PNamed importXMLChild(Element c) {
	    String cname = c.getAttribute("name");
	    PNamed pnamed = nestedChild(cname); // nested for PModelVars
	    if (pnamed == null) 
		importXMLMessage("XML Element " + c.getNodeName() + " " + 
		   cname + " ignored.  No matching child in " + 
		   xmlLabel() + " " + this + ".");
	    else pnamed.importXML(c);
	    
	    return pnamed;
	}
	
	// XML message
	public void importXMLMessage(String s) {
	    appl().importXMLMessage(project(), s);
	}

	// get XML element
	public Element getXML() {
	    return null;
	}

	// copy contents from another PNamed (via XML)
	public void copyFrom(PNamed pold) throws Xcept {
	    if (this.getClass() != pold.getClass()) 
		throw new Xcept(pold, this, 
		    "PNamed.import() class conflict");
	    Element e = pold.getXML();
	    importXML(e);
	}

	// get all text items in XML Element
	public static String allText(Element e) {
	    StringBuffer s = new StringBuffer();
	    NodeList nlist = e.getChildNodes();
	    for (int i=0; i<nlist.getLength(); i++) {
		if (! (nlist.item(i) instanceof CharacterData))
		    continue;
		CharacterData cdata = (CharacterData) nlist.item(i);
		String s1 = cdata.getData();
		int j=0;
		while (j<s1.length() && 
		    Character.isWhitespace(s1.charAt(j))) j++;
		if (j<s1.length()) 
		    s.append(s1.substring(j));
	    }
	    return s.toString();
	}


    // Flatten all XML child nodes into one string.
    public static String allTextAllChildNodes(Element e) {
		//	NodeList nlist = e.getChildNodes();
	try {
		String s;
		s = new String(prettyPrint(e.cloneNode(true))); 
		return s;
	}  catch  (ParserConfigurationException err) {
			err.printStackTrace();
		}
	   catch  (Exception err) {
			err.printStackTrace();
		}
	return 	null; // Nothing to print; 

    }
	
	// set several controls
	public void setControls(NamedVal.NList nvals) throws Xcept {
	    for (int i=0; i<nvals.size(); i++)
	    	setControl(nvals.nval(i));
	}
	public void setControls(NamedVal.List vals) throws Xcept {
	    for (int i=0; i<vals.size(); i++)
	    	setControl(vals.nval(i));
	}
	public void setControls(NamedVal[] nvals) throws Xcept {
	    for (int i=0; i<nvals.length; i++)
	    	setControl(nvals[i]);
	}

	// set a control based on a NamedVal
	public void setControl(NamedVal nval) throws Xcept {
	    String n = nval.name();
	    if (! (child(n) instanceof Control)) throw new Xcept(this,
	        "Control " + n + " not found");
	    Control c = (Control) child(n);
	    if (c instanceof RealControl) 
	    	((RealControl) c).setVal(nval.realVal());
	    else if (c instanceof IntControl)
	    	((IntControl) c).setVal(nval.intVal());
	    else if (c instanceof BooleanControl)
	    	((BooleanControl) c).setVal(nval.boolVal());
	    else if (c instanceof StringControl)
	    	((StringControl) c).setVal(nval.stringVal());
	    else throw new Xcept(c,
	    	"PNamed.setControl(NamedVal) doesn't support this control");
	}

	// just a list
	public static class List extends ArrayList<PNamed> {
	    public List(int n) { super(n); }
	    public PNamed pnamed(int i) { return (PNamed) get(i); }
	    public PModel pmodel (int i) { return (PModel) get(i); }
	    public PDataSet pdataset(int i) { return (PDataSet) get(i); }
	    public ParSet pparset(int i) { return (ParSet) get(i); }
	    public Plot pplot(int i) { return (Plot) get(i); }
	    public Control control(int i) { return (Control) get(i); }
	    public void addUniq(PNamed p) {
		if (! contains(p)) add(p);
	    }

	    // StringList representation for full names
	    public StringList stringList() throws Xcept {
		StringList slist = new StringList(size());
		for (int i=0; i<size(); i++) 
		    slist.add(control(i).fullname());
		return slist;
	    }

	    // StringList representation of names
	    public StringList nameList() throws Xcept {
		StringList slist = new StringList(size());
		for (int i=0; i<size(); i++) 
		    slist.add(pnamed(i).name());
		return slist;
	    }

	    // count class occurences in list
	    public int count(Class clss) {
	    	int ct = 0;
	    	for (int i=0; i<size(); i++)
		    if (clss.isInstance(get(i))) ct++;
	    	return ct;
	    }
	}

	// named list
	public static class NList extends NamedList {
	    public NList(int n) { super(n); }
	    public PNamed pnamed(int n) { return (PNamed) get(n); }
 	    public PNamed pnamed(String n) { return (PNamed) getByName(n); }
        }



    public static final String prettyPrint(Node node) throws Exception {
		try {
			String strOut;
	        Transformer tf = TransformerFactory.newInstance().newTransformer();
	        tf.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
	        tf.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
	        tf.setOutputProperty(OutputKeys.INDENT, "yes");
			Writer out = new StringWriter();
			tf.transform(new DOMSource(node), new StreamResult(out));
			strOut = new String(out.toString());
			// Treat string as XML node, so need to convert as necessary:
			strOut = strOut.replaceAll("&lt;","<");
			strOut = strOut.replaceAll("&gt;",">");
			return strOut;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null; // Nothing to print
	    }


}

