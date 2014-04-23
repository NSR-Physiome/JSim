/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Parameter set is preserved state of PModelVars

package JSim.project; import JSim.aserver.*;

import java.io.*;
import java.util.*;

import org.w3c.dom.*;

import JSim.util.*;
import JSim.data.*;

public class ParSet extends PNamed {

	// state
	public BooleanControl locked;
	public TextControl notes;
	private LinkedHashMap<String, Group> groups; // version 2 storage
	private boolean isLast; // model "last" store only vars.*

	public static final String INPUTS = "inputs";
	public static final String SOLVER = "solver";
	public static final String FUNCTION = "function";
	public static final String LOOPS = "loops";
	public static final String SENS = "sens";
	public static final String OPTIM = "optim";
	public static final String MEMORY = "memory";
	public static final char SEPARATOR = ':';

	public static final StringList SECTIONS = new StringList( 
	    new String[] { INPUTS, SOLVER, FUNCTION, LOOPS, SENS, OPTIM, MEMORY });

	// constructors
	public ParSet(PNamed p, String n) throws Xcept {
	    this(p, n, false);
	}
	public ParSet(PNamed p, String n, boolean isLast) throws Xcept
	{
	    super(p, n);
	    addDescCntl();
	    locked = new BooleanControl(this, "locked", false);
	    notes = new TextControl(this, "notes");
	    this.isLast = isLast;
	    clearGroups();
	}
	public ParSet(Project p, JSReadable readable) throws Xcept {
	    this(p, p.loadName(readable));
	    readFile(readable);
	}

	// clear groups in parset
	public void clearGroups() {
	    groups = new LinkedHashMap<String, Group>();
	}

	// store PModel values in groups
	protected void store(PModel pmodel) throws Xcept {
	    clearGroups();
	    storeGroup(INPUTS, pmodel.vars().assign());
	    storeGroup(SOLVER, pmodel.vars().solver());
	    storeGroup(MEMORY, pmodel.vars().memory());
	    for (int i=0; i<pmodel.vars().nChild(); i++) {
	    	PNamed p = pmodel.vars().child(i);
		if (p instanceof FuncGen)
		    storeGroup(FUNCTION + SEPARATOR + p.name(), p);
	    }
	    if (isLast) return;
	    if (! pmodel.loops().isEmpty())
	        storeGroup(LOOPS, pmodel.loops());
	    if (! pmodel.sens().isEmpty())
	        storeGroup(SENS, pmodel.sens());
	    if (! pmodel.optim().isEmpty())
	        storeGroup(OPTIM, pmodel.optim());
	}

	// create Group, store controls from PNamed
	private void storeGroup(String name, PNamed pnamed) throws Xcept {
	    Group g = new Group(name);
	    storeControls(g, "", pnamed);
	    if (g.size() > 0)
	    	groups.put(name, g);
	}

	// recursively store controls in Group
	private void storeControls(Group g, String pfx, PNamed pnamed) 
	throws Xcept {
	    for (int i=0; i<pnamed.nChild(); i++) {
	        PNamed c = pnamed.child(i);
		if (c instanceof Control) {
	    	    String key = pfx + c.name();
		    String value = ((Control) c).stringVal();
		    g.put(key, value);
		}
		String ppfx = pfx + c.name() + ".";
	    	if (ppfx.startsWith(".")) ppfx = ppfx.substring(1);
		storeControls(g, ppfx, c);
	    }
	}

	// load ParSet into model 
	protected void load(PModel pmodel) { 
	    ArrayList<Group> gs = groupsArray();
	    for (int i=0; i<gs.size(); i++) {
	    	Group g = gs.get(i);
		PNamed pgroup = g.pnamed(pmodel);
		if (pgroup == null) {
		    importXMLMessage("Parameter group not found: " +
		    	g.name);
		    continue;
		}
		Iterator<String> iter = g.keySet().iterator();
		while (iter.hasNext()) 
		    loadControl(iter.next(), g, pgroup);
	    } 
	}
	
	// load single par into control
	private void loadControl(String key, Group g, PNamed pgroup) {	
	    String value = g.get(key);
	    PNamed p = pgroup.nestedChild(key);
	    if (! (p instanceof Control)) {
		//importXMLMessage("No such parameter: " +
		//    g.name + "." + key);
		return;
	    }
	    Control c = (Control) p;
	    try {
	    	c.setVal(value);
	    } catch (Xcept e) {
	    	importXMLMessage(e.cleanMessage());
	    }    	
	}


	// import XML
	public void importXML(Element e) {
	    Element evars = null;
	    Element egroups = null;
	    NodeList nlist = e.getChildNodes();
	    for (int i=0; i<nlist.getLength(); i++) {
		if (! (nlist.item(i) instanceof Element)) continue;
		Element c = (Element) nlist.item(i);
		String n = c.getNodeName();
		if (n.equals("vars")) 
		    evars = c;
		else if (n.equals("pargroups"))
		    egroups = c;
		else
		    importXMLChild(c);
	    }
	    if (egroups != null)
	        importGroups(egroups);
	    else if (evars != null)
	        importGroups(evars);
	    else 
	        importXMLMessage("XML Parset missing both vars & pargroups");
	}

	// import groups from Element
	private void importGroups(Element e) {
	    clearGroups();
	    NodeList nodes = e.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) 
	        importGroup(nodes.item(i));
	}

	// add group from Element
	private void importGroup(Node node) {
	    if (! (node instanceof Element)) return;
	    Element e = (Element) node;
	    String gname = e.getNodeName();
	    if (gname.equals("pargroup"))
	    	gname = e.getAttribute("name");
	    if (gname.equals("assign"))
	    	gname = INPUTS;
	    if (gname.equals("function"))
	    	gname = FUNCTION + SEPARATOR + e.getAttribute("name");
	    Group g = new Group(gname);
	    groups.put(gname, g);
	    importGroupControls(g, e, "");
	}
	
	// import prefixed controls into Group
	private void importGroupControls(Group g, Element base, String prefix) {
	    NodeList nodes = base.getChildNodes(); 
	    for (int i=0; i<nodes.getLength(); i++) { 
	    	if (! (nodes.item(i) instanceof Element)) continue;
		Element e = (Element) nodes.item(i);
		String etype = e.getNodeName();
		String name = e.getAttribute("name");
		if (etype.equals("control")) {
		    String key = prefix + name;
		    String value = e.getAttribute("value");
		    g.put(key, value);
		} else {
		    importGroupControls(g, e, name + ".");
		}
	    }
	}

	// export doc (version 1) and groups (version 2)
	public void exportExtraXML(Element e) {
	    Document doc = e.getOwnerDocument();

	    // export <pargroups> from groups
	    Element egs = doc.createElement("pargroups");
	    e.appendChild(egs);
	    ArrayList<Group> gs = groupsArray();
	    for (int i=0; i<gs.size(); i++) 
	    	exportGroup(doc, egs, gs.get(i));

	    // export legacy <vars> from groups
	    exportLegacyXML(doc, e);	
	}  

	// export group XML
	private void exportGroup(Document d, Element egs, Group g) {
	    Element eg = d.createElement("pargroup");
	    eg.setAttribute("name", g.name);
	    egs.appendChild(eg);
	    Iterator<String> iter = g.keySet().iterator();
	    while (iter.hasNext()) {
	    	String key = iter.next();
		String value = g.get(key);
		if (value == null) continue;
		Element ep = d.createElement("control");
		ep.setAttribute("name", key);
		ep.setAttribute("value", value);
		eg.appendChild(ep);
	    }
	}

	// export legacy <vars> from groups
	// remove this once most JSim users have 2.05 or above
	private void exportLegacyXML(Document d, Element base) {
	    Element evars = d.createElement("vars");
	    evars.setAttribute("name", "vars");
	    base.appendChild(evars);
	    ArrayList<Group> gs = groupsArray();
	    for (int i=0; i<gs.size(); i++) {
	        Group g = gs.get(i);
	        String nodeName = null;
	        String pname = null;
	        if (g.name.equals(SOLVER)) {
	       	    nodeName = pname = SOLVER;
		} else if (g.name.equals(INPUTS)) {
		    nodeName = "assign";
		    pname = "vars";
		} else if (g.inSection(FUNCTION)) {
		    nodeName = FUNCTION;
		    pname = g.suffix();
		} else 
		    continue; // ignore other groups in legacy <vars>
		Element egrp = d.createElement(nodeName);
		egrp.setAttribute("name", pname);
		evars.appendChild(egrp);
		if (nodeName.equals(FUNCTION))
		    exportLegacyFunction(d, g, egrp);
		else 
		    exportLegacyGroup(d, g, egrp);
	     }
	 }
	 
	// export single group
	private void exportLegacyGroup(Document d, Group g, Element base) {	 
 	    Iterator<String> keys = g.keySet().iterator();
	    while (keys.hasNext()) {
	    	String key = keys.next();
		String value = g.get(key);
		Element c = d.createElement("control");
		c.setAttribute("name", key);
		c.setAttribute("value", value);
		base.appendChild(c);
	    }
	}
	    
	// export single group
	private void exportLegacyFunction(Document d, Group g, Element base) {	 
	    Hashtable<String, Element> subfuncs = new Hashtable<String, Element>();
 	    Iterator<String> keys = g.keySet().iterator();
	    while (keys.hasNext()) {
		String key = keys.next();
		String value = g.get(key);
		if (value == null) value = "";
		int inx = key.indexOf(".");
		Element efunc = base;
		if (inx > 0) {
		    String fname = key.substring(0, inx);
		    efunc = subfuncs.get(fname);
		    if (efunc == null) {
		        efunc = d.createElement(FUNCTION);
		        efunc.setAttribute("name", fname);
		        base.appendChild(efunc);
		        subfuncs.put(fname, efunc);
		    }
		    key = key.substring(inx+1);
		}
		Element c = d.createElement("control");
		c.setAttribute("name", key);
		c.setAttribute("value", value);
		efunc.appendChild(c);
	    }
	}
		    
	// read parameter text
	public void readFile(JSReadable r) throws Xcept {
	    clearGroups();
	    try {
	    	readFile1(r);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// read parameter text with exceptions
	private void readFile1(JSReadable r) throws Exception {
	    BufferedReader rdr = new BufferedReader(
	    	new StringReader(r.readText()));
	    Group group = readGroup("inputs"); // default group
	    while (true) {
	    	String line = rdr.readLine();
		if (line == null) break;
		int inx = line.indexOf("//");
		if (inx >= 0) 
		    line = line.substring(0, inx);
		if (Util.isBlank(line)) continue;
		line = line.trim();
		// System.err.println("read[" + line + "]");
	   	StringTokenizer stok = new StringTokenizer(line);
		String tok1 = stok.nextToken();
		if (tok1.equals("parameter")) {
		    String tok2 = stok.nextToken();
		    if (tok2 == null) tok2="";
		    if (tok2.equals("set"))
		        continue; // ignored for now
		    if (tok2.equals("group")) {
		        group = readGroup(stok.nextToken());
		        continue;
		    }
		} else {
		    readParValue(group, line);
		}
	    }
	    rdr.close();
	}

	// read group
	private Group readGroup(String name) throws Xcept {
	    Group g = groups.get(name);
	    if (g != null) return g;
	    g = new Group(name);
	    if (SECTIONS.contains(g.section())) {
		groups.put(name, g);
		return g;
	    } else {
	    	importXMLMessage("Unsupported parameter group: " + name);
	    	return null;
	    }
	}

	// read par value
	private void readParValue(Group g, String line) throws Xcept {
	    int inx = line.indexOf("=");
	    if (inx <= 0) {
	    	importXMLMessage("Ignored line1: " + line);
		return;
	    }
	    if (g == null) return;
	    String key = line.substring(0, inx);
	    String value = line.substring(inx+1, line.length());
	    g.put(key, value);
	}

	// remove all groups in section
	public void removeSection(String sname) {
	    ArrayList<Group> gs = groupsArray();
	    for (int i=0; i<gs.size(); i++) {
	    	Group g = gs.get(i);
		if (g.inSection(sname))
		    groups.remove(g.name);
	    }
	}

	// import sections from another ParSet
	public void importSections(ParSet pars, StringList
	snames) throws Xcept {
	    for (int i=0; i<snames.size(); i++)
	    	importSection(pars, snames.str(i));
	}
	
	// import selected group from another ParSet
	public void importSection(ParSet pars, String sname) 
	throws Xcept {
	    removeSection(sname);
	    ArrayList<Group> gs = pars.groupsArray();
	    for (int i=0; i<gs.size(); i++) {
	    	Group g = gs.get(i);
		if (g.inSection(sname))
		    groups.put(g.name, new Group(g));
	    }
	}

	// write par file text
	public void writeFile(PrintStream out) {
	    writeFile(new PrintWriter(out, true));
	}
	public void writeFile(PrintWriter out) {
	    out.println("parameter set " + name());
	    writeGroup(out, groups.get(INPUTS));
	    writeGroup(out, groups.get(SOLVER));
	    writeGroup(out, groups.get(MEMORY));
	    ArrayList<Group> gs = groupsArray();
	    for (int i=0; i<gs.size(); i++) {
	    	Group g = gs.get(i);
		if (g.name.equals(INPUTS)) continue;
		if (g.name.equals(SOLVER)) continue;
		if (g.name.equals(MEMORY)) continue;
		writeGroup(out, g);
	    }
	}
	
	// write 1 group to par file text
	private void writeGroup(PrintWriter out, Group g) {
	    if (g == null) return;
	    out.println("\nparameter group " + g.name);
	    Iterator<String> iter = g.keySet().iterator();
	    while (iter.hasNext()) {
	    	String key = iter.next();
		String value = g.get(key);
		out.println("  " + key + "=" + value);
	    }
	}	

	// query
	public String diagInfo() { return "ParSet " + name; }
	public String xmlLabel() { return "parset"; }
	public boolean isLocked() { return locked.val(); }
	public StringList getSectionNames() {
	    StringList slist = new StringList();
	    ArrayList<Group> gs = groupsArray();
	    for (int i=0; i<gs.size(); i++) {
		Group g = gs.get(i);
	    	slist.addUniq(g.section());
	    }
	    return slist;
	}
	public ArrayList<Group> groupsArray() {
	    return new ArrayList<Group>(groups.values());
	}

	// Parameter Group
	public static class Group extends LinkedHashMap<String,String> {
	    public String name;  // name of this group
	    private int sinx; // SEPARATOR index
	    
	    // constructor
	    public Group(String name) {
	    	super();
		this.name = name;
		sinx = name.indexOf(SEPARATOR);
	    }
	    public Group(Group g) {
	    	super(g);
		this.name = g.name;
		sinx = name.indexOf(SEPARATOR);
	    }

	    public boolean inSection(String s) {
	    	return name.startsWith(s);
	    }
	    public String section() {
		if (sinx < 0) return name;
		return name.substring(0, sinx);		
	    }
	    public String suffix() {
		return (sinx < 0) ? "" : name.substring(sinx+1);		
	    }
	    public PNamed pnamed(PModel pmodel)  {
	    	if (name.equals(INPUTS)) 
		    return pmodel.vars().assign();
	    	if (name.equals(SOLVER)) 
		    return pmodel.vars().solver();
	    	if (name.equals(MEMORY)) 
		    return pmodel.vars().memory();
		if (inSection(FUNCTION)) 
		    return pmodel.vars().child(suffix());
		return pmodel.nestedChild(name);
	    }
	}
	    
}

