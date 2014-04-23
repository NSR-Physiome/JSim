/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model browser controls

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.aserver.*;
import JSim.gui.*;
import JSim.gui.browser.*;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class GModelBrowser extends GNode {
	private PModelBrowser pbrowser;
	private JMenuBar mbar;
	private JLabel l_hdr;
	private GBGraph ggraph; // browser drawing component
	private JMenu m_which, m_collapse, m_fill, m_border, m_shape;
	public  GAction a_update, a_revert, a_print, a_export;
	private Hashtable<String,JRadioButtonMenuItem> saveRadios;

	// info updated each time model compiled
	private String graphMLText; // GraphML text from model
	private Document graphMLDoc;  // parsed GraphML from model
	private StringList whichOptions;
	private ArrayList<String> props;
	private ArrayList<String> domains;

	public static String[] collapseOptions = new String[] {
	    "true", "false"
	};
	public static String[] VAR_COLORS = new String[] {
	    "unitType", "dataType", "toolType", "domains",
	    "isInput", "isPrivate", "hasTool", "hasEvent"
	};
	public static String[] VAR_SHAPES = new String[] {
	    "isInput", "isPrivate", "hasTool", "hasEvent"
	};
	public static String[] SEQ_COLORS = new String[] {
	    "itemType", "phase", "deT", "hasDeT"
	};
	public static String[] SEQ_SHAPES = new String[] {
	    "hasDeT"
	};


	// constructor
	public GModelBrowser(GNode p, PModelBrowser b) {
	    super(p, b);
	    pbrowser = b;
	    saveRadios = new Hashtable<String,JRadioButtonMenuItem>();

	    // create widgets
	    setJComp(new JRootPane());

	    // create actions
	    a_update = new GAction(this, "Refresh") {
	    	public void doit() throws Xcept {
		    updateGraph(true);
		}
		public boolean sbEnabled() { return true; }
	    };
	    a_update.setAccel('R');
	    a_revert = new GAction(this, "Revert to default layout") {
	    	public void doit() throws Xcept {
		    updateGraph(false);
		}
		public boolean sbEnabled() { return true; }
	    };
	    a_print = new GAction(this, "Print ...") {
	    	public void doit() throws Xcept {
		    printGraph();
		}
		public boolean sbEnabled() { return true; }
	    };
	    a_print.setAccel('P');
	    a_export = new GAction(this, "Export ...") {
	    	public void doit() throws Xcept {
		    exportGraph();
		}
		public boolean sbEnabled() { return true; }
	    };
	    a_export.setAccel('E');

	    domains = new ArrayList<String>();
	    props = new ArrayList<String>();
	}

	// refresh
	public void refresh() {
	    if (needsContent) makeContent();
	    try {
		updateGraphML();
	    } catch (Xcept e) {	
		e.printStackTrace();
	    }
	    reloadMenus();
	    super.refresh();
	}

	// make content
	public void makeContent() {
	    super.makeContent();

	    JPanel mpanel = new JPanel(new BorderLayout());
	    JLabel jtitle = new JLabel("Model Browser", JLabel.CENTER);
	    jtitle.setFont(glook().bigFont());
	    mpanel.add(jtitle, BorderLayout.NORTH);  
	    JPanel gpanel = new JPanel();
	    ggraph = new GBGraph(gpanel, pbrowser.layout());
	    mpanel.add(gpanel, BorderLayout.CENTER);
	    
	    // menubar
	    mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);

	    // file menu
	    JMenu menu = newMenu("Render");
	    mbar.add(menu);
	    m_which = newMenu("Data select");
	    menu.add(m_which);
	    m_collapse = newMenu("Collapse variables");
	    menu.add(m_collapse);
	    m_fill = newMenu("Node fill color");
	    menu.add(m_fill);
	    m_border = newMenu("Node border color");
	    menu.add(m_border);
	    m_shape = newMenu("Node shape");
	    menu.add(m_shape);
	    menu.addSeparator();
	    menu.add(a_revert);
	    
	    mbar.add(a_update.button());

	    helpLinks().addHelpMenu(this, mbar);

	    // root additions
	    JRootPane root = (JRootPane) jcomp();
	    root.setContentPane(mpanel);
	    root.setJMenuBar(mbar);
	}

	// update props, domains from graphML
	private void updateGraphML() throws Xcept {
	    ASModel rt = gmodel().pmodel().rt();
	    String text = rt.getText(ASModel.TEXT_GRAPHML, null);
	    if (graphMLText != null && graphMLText.equals(text))
		 return;  // no update needed
	    graphMLText = text;
	    graphMLDoc = UtilXML.parse(graphMLText);

	    // grab selectable info from graphMLDoc
	    whichOptions = new StringList();
	    domains = new ArrayList<String>();
	    props = new ArrayList<String>();
	    Element root = graphMLDoc.getDocumentElement();
	    NodeList glist = root.getElementsByTagName("graph");
	    Element vgraph = null;
	    for (int i=0; i<glist.getLength(); i++) {
	    	Element g = (Element) glist.item(i);
		String id = g.getAttribute("id");
		whichOptions.add(id);
		if (id.equals("variables")) vgraph = g;
	    }
	    if (vgraph == null) return;
	    NodeList klist = vgraph.getElementsByTagName("key");
	    for (int i=0; i<klist.getLength(); i++) {
	    	Element key = (Element) klist.item(i);
		String name = key.getAttribute("attr.name");
		if (name.startsWith("hasDomain."))
		    domains.add(name.substring(10));
		if (name.startsWith("property."))
		    props.add(name.substring(9));
	    }
	    Collections.sort(domains);
	    Collections.sort(props);
	}

	// reload menus
	private void reloadMenus() {
	    if (whichOptions != null)
	    	reloadMenu(m_which, pbrowser.which, whichOptions);
	    reloadMenu(m_collapse, pbrowser.varCollapse, 
	    	new StringList(collapseOptions));
	    m_collapse.setEnabled(isVar());

	    StringList colors = new StringList(
	    	isVar() ? VAR_COLORS : SEQ_COLORS);
	    StringList shapes = new StringList(
	    	isVar() ? VAR_SHAPES : SEQ_SHAPES);

	    if (isVar()) {
	    	for (int i=0; i<domains.size(); i++) {
		    String hasX = "hasDomain." + domains.get(i);
		    colors.add(hasX);
		    shapes.add(hasX);
		}
	    	for (int i=0; i<props.size(); i++) {
		    String p = props.get(i);
		    String prop = "property." + p;
		    String hasProp = "hasProperty." + p;
		    colors.add(isUniq(p) ? hasProp : prop);
		    shapes.add(hasProp);
		}
	    }    
	    colors.add("disabled");
	    shapes.add("disabled");
	    	
	    reloadMenu(m_fill, nodeFillColor(), colors);
	    reloadMenu(m_border, nodeBorderColor(), colors);
	    reloadMenu(m_shape, nodeShape(), shapes);

	    updateMenu(m_which, pbrowser.which);
	    updateMenu(m_collapse, pbrowser.varCollapse);
	    updateMenu(m_fill, nodeFillColor());
	    updateMenu(m_border, nodeBorderColor());
	    updateMenu(m_shape, nodeShape());
	}
	
	// reload one menu
	private void reloadMenu(JMenu menu, Control cntl, StringList opts) {
	    menu.removeAll();
	    ButtonGroup group = new ButtonGroup();
	    for (int i=0; i<opts.size(); i++) {
	    	JRadioButtonMenuItem b = getRadio(cntl, opts.str(i));
	    	menu.add(b);
	    	group.add(b);
	    }
	}

	// get radio button for action
	private JRadioButtonMenuItem getRadio(Control cntl, String val) {
	    String s = cntl.name() + "#" + val;
	    JRadioButtonMenuItem r = saveRadios.get(s);
	    if (r == null) {
	    	GAction a = new GControlAction(this, cntl, val);
	    	r = a.radio();
		saveRadios.put(s, r);
	    }
	    return r;
	}

	// is property expected to be uniq?
	private boolean isUniq(String p) {
	    if (p.equals("desc")) return true;
	    if (p.equals("help")) return true;
	    return false;
	}

	// update menu     
	private void updateMenu(JMenu menu, Control cntl) {
	    String val = cntl.stringVal();
	    int ct = menu.getItemCount();
	    for (int i=0; i<ct; i++) {
	    	Component c = menu.getMenuComponent(i);
		if (! (c instanceof JRadioButtonMenuItem)) continue;
		JRadioButtonMenuItem r = (JRadioButtonMenuItem) c;
		r.setSelected(r.getText().equals(val));
	    }
	}

	// update graph
	public void updateGraph(boolean preserveLayout) throws Xcept {
	    if (graphMLDoc == null) throw new Xcept(
	    	"Model must be compiled before browser is operational");
	    GBParms parms = makeGBParms();
	    ggraph.update(graphMLDoc, parms, preserveLayout);
	}

	// get single-graph text NEW
	private String getGraphMLText() throws Xcept {
	    if (graphMLDoc == null) throw new Xcept(
	    	"Model must be compiled before browser is operational");
	    String graphID = graphID();
	    GraphMLWriter wrt = new GraphMLWriter();
	    try {
	    	return wrt.writeString(graphMLDoc, graphID);
  	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// create GBParms from project
	private GBParms makeGBParms() throws Xcept {
	    GBParms p = new GBParms();
	    p.graphID = graphID();
	    p.graphStyle = graphStyle();
	    p.collapseVars = pbrowser.varCollapse.val();
	    p.nodeTextColor = nodeTextColor().val();
	    p.nodeFillColor = nodeFillColor().val();
	    p.nodeBorderColor = nodeBorderColor().val();
	    p.nodeShape = nodeShape().val();
	    return p;
	}

	// graph style based on which
	private String graphID() { return pbrowser.which.val(); }
	private int graphStyle() {
	    String w = graphID();
	    return w.equals("variables") ? 
	    	GBParms.VARIABLES : GBParms.SEQUENCE;
	}
	private boolean isVar() { 
	    return graphStyle() == GBParms.VARIABLES; 
	}
	private StringControl nodeTextColor() {
	    return isVar() ? pbrowser.varNodeTextColor : pbrowser.seqNodeTextColor;
	}
	private StringControl nodeFillColor() {
	    return isVar() ? pbrowser.varNodeFillColor : pbrowser.seqNodeFillColor;
	}
	private StringControl nodeBorderColor() {
	    return isVar() ? pbrowser.varNodeBorderColor : pbrowser.seqNodeBorderColor;
	}
	private StringControl nodeShape() {
	    return isVar() ? pbrowser.varNodeShape : pbrowser.seqNodeShape;
	}

	// print graph
	public void printGraph() throws Xcept {
	    throw new Xcept("Model browser printing not yet supported");
	}

	// export graph
	public void exportGraph() throws Xcept {
	    throw new Xcept("Model browser export not yet supported");
	}

	// control set action
	public class GControlAction extends GAction {
	    Control control;
	    String value;
	    
	    public GControlAction(GNode n, Control c, String v) {
	        super(n, v);
		control = c;
		value = v;
	    }
	    
	    public void doit() throws Xcept {
	    	control.setVal(value);
		if (control == pbrowser.which)
		    reloadMenus();
	    }
	}	    

	// export XY graphic state before save
	public void exportGraphicState() throws Xcept {
	    if (ggraph != null)
	    	ggraph.updateLayout();
	}
}

