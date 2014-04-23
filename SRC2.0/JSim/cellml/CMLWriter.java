/*NSRCOPYRIGHT
  Copyright (C) 1999-2008 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// convert XMML to CellML

package JSim.cellml; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*; 

import java.io.*;
import java.util.*;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import org.w3c.dom.*;


public class CMLWriter {
    private Document xdoc; // XMML document
    private StringList warnings; 
    private Document cmldoc;  // CellML document being built
    private Element  cmlroot, cmlcomponent;
    private CMLUnitWriter unitWriter;
    private Element xroot, xtoolList, 
        xunitList, xvarList, xeventList, xrelationList;
    private Hashtable<String,Element> xvarIDs,
        xtoolIDs, xeventIDs, xrelationIDs;
    private StringList xdomains;

    // constructor
    public CMLWriter(Document doc, StringList warnings_in) 
        throws Exception {
        warnings = warnings_in;
        xdoc = doc;
        //doc.setAttribute("xmlns", "http://www.cellml.org/cellml/1.0#");
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        DOMImplementation domImpl = db.getDOMImplementation();
        cmldoc = domImpl.createDocument("http://www.cellml.org/cellml/1.0#", "model", null);
        cmlroot = cmldoc.getDocumentElement();
	    cmlroot.setAttribute("xmlns", "http://www.cellml.org/cellml/1.0#");
        cmlroot.setAttribute("xmlns:cellml", "http://www.cellml.org/cellml/1.0#");
        cmlcomponent = createElement("component");
        cmlcomponent.setAttribute("name", "JSim_model");
        xroot = xdoc.getDocumentElement();
        xtoolList = getFirstElement(xroot, "toolList");
        xunitList = getFirstElement(xroot, "unitList");
        xvarList = getFirstElement(xroot, "variableList");
        xeventList = getFirstElement(xroot, "eventList");
        xrelationList = getFirstElement(xroot, "relationList");
        unitWriter = new CMLUnitWriter(this, xunitList);
        xvarIDs = loadIDMap(xvarList);
        xtoolIDs = loadIDMap(xtoolList);
        xeventIDs = loadIDMap(xeventList);
        xrelationIDs = loadIDMap(xrelationList);
        xdomains = new StringList();
        findDomains();
        //mBadFunctions = new ArrayList<String>();
    }
    
    // create and return a CellML document. 
    public Document getCellML(String options) throws Xcept {
        //System.out.println("Units:");
        unitWriter.loadUnits();
        //System.out.println("Vars:");
        loadVars();
        //System.out.println("Tools:");
        loadTools();
        //System.out.println("Events:");
        loadEvents();
        //System.out.println("Constraints:");
        loadConstraints();
        return cmldoc;    
    }

    // Find the domain variable(s) in the model
    public void findDomains() {
        if (xvarList == null) return;
        NodeList nodes = xvarList.getChildNodes();
        for (int i=0; i<nodes.getLength(); i++) {
            if (! (nodes.item(i) instanceof Element)) continue;
            Element e = (Element) nodes.item(i);
            if (e.getAttribute("isDomain").equals("true")) {
                xdomains.add(e.getAttribute("id"));
            }
        }
    }

    // create and return a new Element as a direct child of the CellML root 'model' element.
    protected Element createElement(String name) {
        Element ret = cmldoc.createElement(name);
        cmlroot.appendChild(ret);
        return ret;
    }

    // create and return a new <variable> element, as a child of the single CellML 'component' element.
    protected Element createVariable(String name) {
        Element ret = cmldoc.createElement("variable");
        ret.setAttribute("name", name.replaceAll("\\.", "_"));
        cmlcomponent.appendChild(ret);
        return ret;
    }

    // create and return a new <ci> mathML element as part of the CellML document,
    //  but not as a child of any existing element.  Its text will be equal to 'name'
    private Element createCIElement(String name) {
        Element ret = cmldoc.createElement("ci");
        Text text = cmldoc.createTextNode(name.replaceAll("\\.", "_"));
        ret.appendChild(text);
        return ret;
    }

    // create an return a new mathML 'dx/dy' construct with 'var' and 'time', as
    //  part of the CellML document, but not as a child of any existing element.
    private Element createDiffElement(String var, String time) {
        Element ret = cmldoc.createElement("apply");
        Element diff = cmldoc.createElement("diff");
        ret.appendChild(diff);

        Element bvar = cmldoc.createElement("bvar");
        Element citime = cmldoc.createElement("ci");
        Text text = cmldoc.createTextNode(time);
        citime.appendChild(text);
        bvar.appendChild(citime);
        ret.appendChild(bvar);

        Element civar = createCIElement(var);
        ret.appendChild(civar);

        return ret;
    }

    // create and return a new <cn> mathML element as part of the CellML document,
    //  but not as a child of any existing element.  Its text will be equal to
    //  'value'.
    private Element createCNElement(Double value) {
        Element ret = cmldoc.createElement("cn");
        Text text = cmldoc.createTextNode(value.toString());
        ret.appendChild(text);
        return ret;
    }

    // If 'parent' contains at least one child node of type Node.TEXT_NODE, find
    //  and return that node.  If not, return null.
    private Text getTextChild(Element parent) {
        if (parent==null) return null;
        NodeList nodes = parent.getChildNodes();
        for (int n=0; n<nodes.getLength(); n++) {
            if (nodes.item(n).getNodeType() == Node.TEXT_NODE) return (Text) nodes.item(n);
        }
        return null;
    }

    // create CellML variables
    private void loadVars() throws Xcept {
        ArrayList<Element> xvars = new ArrayList<Element>(xvarIDs.values());
        for (int i=0; i<xvars.size(); i++) {
            Element xvar = xvars.get(i);
            String name = xvar.getAttribute("id");
            if (! isVarUsable(name)) continue;
            name.replaceAll("\\.", "_");

            Element variable = createVariable(name);
            //Theoretically, you could check 'isExtern' and set the public interface accordingly.  But, enh.
            // Let's do that when we have actual modularity in JSim, instead of faking
            // it with the current scheme.
            //System.out.println("Setting units for " + name);
            String unit = xvar.getAttribute("unitID");
            if (unit != null && !unit.equals("")) {
                variable.setAttribute("units", unitWriter.toCellMLFromJSim(unit));
            }
            //System.out.println("Set units.");
        }
        //System.out.println("done with vars, too.");
    }

    // create CellML equations from JSim 'tools'.
    private void loadTools() throws Xcept {
        ArrayList<Element> xtools = 
            new ArrayList<Element>(xtoolIDs.values());
        for (int i=0; i<xtools.size(); i++) {
            Element xtool = xtools.get(i);
            String type = xtool.getNodeName();
            //System.out.println("type: " + type);
            if (type.equals("exprTool")) {
                makeAssignmentRule(xtool);
            }
            else if (type.equals("implicitTool")) {
                makeAlgebraicRules(xtool);
            }
            else if (type.equals("ODETool")) {
                makeRateRule(xtool);
            }
            else if (type.equals("domainTool"))
                continue;
            else if (type.equals("externTool")) {
                //checkExtern(xtool);
                //We don't worry about this--at some point we may
                // want to set the variable's interface, but only
                // after we have real JSim modularity.
            }
            else {
                warnings.add("Warning: " + type + "s are not translatable to CellML: ignoring this part of the model.");
            }
        }
    }

    // create what in SBML is an 'assignment rule', and which in CellML is
    //  and equation with a single variable set equal to an arbitrary expression.
    private void makeAssignmentRule(Element xtool)
        throws Xcept {
        Element vsols = getFirstElement(xtool, "solvedVariableList");
        Element vsol = getFirstElement(vsols, "variableUsage");
        String vid = vsol.getAttribute("variableID");
        if (! isVarUsable(vid)) return;
        vid = vid.replaceAll("\\.", "_");
        Element expr = getFirstElement(xtool, "expression");
        Element origmath = getFirstElement(expr, "math");
        Element math = (Element) cmldoc.importNode(origmath, true);
        translateXmmlMath(math);
        //There theoretically is a way to code delay into a cellml model, but
        // it is 'not really used by any real models' (-D. Nickerson).  In CellML
        // 1.2 the plan is to make this easier, but that has not been released
        // as of Jan, 2012.  Revisit after CellML 1.2 is released, perhaps.
        //translateDelay(math);
        Double initVal = getValue(math);
        if (initVal != null) {
            Element p = getVariable(vid);
            p.setAttribute("initial_value", initVal.toString());
            return;
        }
        String status = vsol.getAttribute("status");
        //System.out.println("Status: " + status);
        if (status.equals("CURR")) {
            Element lhs = createCIElement(vid);
            addEquation(lhs, math);
        }
        else if (status.equals("MIN")) {
            //If the math is just a variable, we can use the initial_value
            String initVar = getVariable(math);
            Element p = getVariable(vid);
            if (initVar!= null) {
                p.setAttribute("initial_value", initVar);
                return;
            }
            else {
                //Otherwise we need to create a temp variable.
                initVar = vid + "_init";
                p.setAttribute("initial_value", initVar);
                Element initvar = createVariable(initVar);
                Element lhs = createCIElement(initVar);
                addEquation(lhs, math);
            }
        }
        else {
            throw new Xcept("Unknown status for variable '" + vid + "':  " + status);
        }
    }

    // create what in SBML is an 'algebraic rule', and which in CellML is
    //  and equation with an arbitrary expression set equal to 0.
    private void makeAlgebraicRules(Element xtool)
        throws Xcept {
        Element zeroexplist = getFirstElement(xtool, "zeroExpressionList");
        NodeList expressions = zeroexplist.getElementsByTagName("expression");
        Element solvedvarlist = getFirstElement(xtool, "solvedVariableList");
        NodeList vars = solvedvarlist.getElementsByTagName("variableUsage");
        LinkedHashMap<String, String> varToInits = new LinkedHashMap<String, String>();
        for (int vnum=0; vnum<vars.getLength(); vnum++) {
            Element var = (Element) vars.item(vnum);
            String id = var.getAttribute("variableID");
            Element param = getVariable(id);
            if (param == null) {
                warnings.add("Warning: unable to find the variable " + id + " which was to be solved by an implicit equation.");
                continue;
            }
            String status = var.getAttribute("status");
            if (status.equals("MIN")) {
                String initname = id + "_init";
                Element initparam = getVariable(initname);
                if (initparam == null) {
                    initparam = createVariable(initname);
                }
                varToInits.put(id, initname);
            }
            else if (!status.equals("CURR")) {
                warnings.add("Warning:  unknown implicit tool status " + status + " when determining how to solve variable " + id + " with an implicit equation.");
            }
        }
        for (int exp=0; exp<expressions.getLength(); exp++) {
            Element expression = (Element) expressions.item(exp);
            Element origmath = getFirstElement(expression, "math");
            Element math = (Element) cmldoc.importNode(origmath, true);
            translateXmmlMath(math);
            renameInitVars(math, varToInits);
            // Again, implement this function if CellML has reasonable support for delays.
            //translateDelay(math);
            Element zero = createCNElement(0.0);
            addEquation(zero, math);
        }
    }

    // create rate rule from ODETool
    private void makeRateRule(Element xtool)
        throws Xcept {
        String timeid = xtool.getAttribute("timeDomainID");
        Element vsols = getFirstElement(xtool, "solvedVariableList");
        Element vstate = null;
        Element vu = getFirstElement(vsols, "variableUsage");
        String vid = vu.getAttribute("variableID");
        if (! isVarUsable(vid)) return;
        Element state = getFirstElement(xtool, "stateEquation");
        String toolID = state.getAttribute("toolID");
        Element tool = xtoolIDs.get(toolID);
        String type = tool.getNodeName();
        if (! type.equals("exprTool")) {
            warnings.add("Warning:  ODETools which use " + type + " as the state equation are not supported.");
            return;
        }
        Element expr = getFirstElement(tool, "expression");
        Element origmath = getFirstElement(expr, "math");
        Element math = (Element) cmldoc.importNode(origmath, true);
        translateXmmlMath(math);
        //As above:  implement this function if CellML has reasonable support for delays
        //translateDelay(math);
        Element lhs = createDiffElement(vid, timeid);
        addEquation(lhs, math);
    }

    // No support yet for events, so just add warnings if any are found.
    private void loadEvents() {
        if (xeventList == null) return; //No events; no warnings.
        NodeList nodes = xeventList.getChildNodes();
        if (nodes.getLength() != 0) {
            warnings.add("Warning: Translation of JSim events to CellML is not yet supported.");
        }
        return;
    }

    // Load the constraints from the XMML model and translate them to CellML.
    private void loadConstraints() 
        throws Xcept {
        if (xrelationList == null) return; //No relations; no warnings.
        ArrayList<Element> xrelations = 
            new ArrayList<Element>(xrelationIDs.values());
        for (int i=0; i<xrelations.size(); i++) {
            Element xrelation = xrelations.get(i);
            makeConstraint(xrelation);
        }
        return;
    }

    // Create a 'constraint':  a variable set inequal to an arbitrary expression.
    private void makeConstraint(Element xrelation)
        throws Xcept {
        Element expr = getFirstElement(xrelation, "expression");
        Element origmath = getFirstElement(expr, "math");
        Element math = (Element) cmldoc.importNode(origmath, true);
        translateXmmlMath(math);
        // Again, imlement this if CellML gets reasonable handling of delays.
        //translateDelay(math);
        cmlcomponent.appendChild(math);
    }

    //Check to see if the passed-in element is a <cn> element and if so, return the
    // value of the double it represents.
    private Double getValue(Element math) {
        NodeList children = math.getElementsByTagName("*");
        if (children.getLength() != 1) return null;
        Element cn = (Element) children.item(0);
        if (cn.getTagName().equals("cn")) {
            Text value = getTextChild(cn);
            if (value==null) return null;
            return Double.parseDouble(value.getData());
        }
        return null;
    }

    //Check to see if the passed-in element is a <ci> element and if so, return the
    // string it represents.    
    private String getVariable(Element math) {
        NodeList children = math.getElementsByTagName("*");
        if (children.getLength() != 1) return null;
        Element ci = (Element) children.item(0);
        if (ci.getTagName().equals("ci")) {
            Text variable = getTextChild(ci);
            if (variable==null) return null;
            //System.out.println("math seems to only have a single child element: " + variable.getData());
            return variable.getData();
        }
        return null;
    }

    //Find the <variable> element with the id 'vid', and return it.  Return 'null' if
    // no such variable exists.
    private Element getVariable(String vid) {
        NodeList vars = cmlcomponent.getElementsByTagName("variable");
        for (int v=0; v<vars.getLength(); v++) {
            Element var = (Element) vars.item(v);
            if (vid.equals(var.getAttribute("name"))) return var;
        }
        return null;
    }

    //Add an equation to CellML component consisting of a single equation with
    // a left hand side (lhs) and a right hand side (rhs).
    private void addEquation(Element lhs, Element rhs) {
        //System.out.println("Adding an equation (theoretically)");
        Element math = cmldoc.createElement("math");
        math.setAttribute("xmlns", "http://www.w3.org/1998/MathML");
        cmlcomponent.appendChild(math);
        Element apply = cmldoc.createElement("apply");
        math.appendChild(apply);
        Element eq = cmldoc.createElement("eq");
        apply.appendChild(eq);
        apply.appendChild(lhs);
        if (rhs.getTagName().equals("math")) {
            rhs = getFirstElement(rhs);
        }
        apply.appendChild(rhs);
    }

    // 1st child element of given name
    private Element getFirstElement(Element base, String name) {
        NodeList nodes = base.getElementsByTagName(name);
        if (nodes.getLength()==0) return null;
        return (Element) nodes.item(0);
    }

    // 1st child element of any name
    private Element getFirstElement(Element base) {
        NodeList nodes = base.getElementsByTagName("*");
        if (nodes.getLength()==0) return null;
        return (Element) nodes.item(0);
    }

    // is this var name usable by CellML?
    private boolean isVarUsable(String name) throws Xcept {
        if (name.indexOf("__init")>=0) {
            warnings.add("Warning: XMML file created without 'makeDEICParms=false' defined, resulting in the creation of '__init' variables.");
        }
        if (name.indexOf("__")>=0) return false; // JSim created
        if (name.indexOf(":")>=0) return false; // derivative

        //Note:  for now, we don't export the t.min, t.max, t.ct, and t.delta variables.
        // If we ever want (say) 't.min' to be useable in a function in CellML, it'll
        // need to be added back in, or some other decision will have to be made.
        for (int d=0; d<xdomains.size(); d++) {
            if (name.startsWith(xdomains.get(d) + ".")) return false;
        }
        return true;
    }

    // create ID map
    private Hashtable<String,Element> loadIDMap(Element base) {
        Hashtable<String,Element> map = 
            new Hashtable<String,Element>();
        if (base == null) return map;
        NodeList nodes = base.getChildNodes();
        for (int i=0; i<nodes.getLength(); i++) {
            if (! (nodes.item(i) instanceof Element)) continue;
            Element e = (Element) nodes.item(i);
            String id = e.getAttribute("id");
            map.put(id, e);
        }
        return map;
    }

    //Convert the MathML from XMML for use in CellML.  This includes replacing
    // 'ci' elements with children to a function, replacing '.min' with '__min',
    // and converting xmml:units attributes to cellml:units attributes.
    private void translateXmmlMath(Element mathml) {
        if (mathml.getTagName().equals("math")) {
            NodeList children = mathml.getElementsByTagName("*");
            for (int c=0; c<children.getLength(); c++) {
                Element child = (Element) children.item(c);
                translateXmmlMath(child);
            }
        }
        else if (mathml.getTagName().equals("ci")) {
            String textValue = "";
            Node firstchild = mathml.getFirstChild();
            if (firstchild != null) {
                textValue = firstchild.getNodeValue();
            }
            if (mathml.hasAttribute("type")) {
                if (mathml.getAttribute("type").equals("function")) {
                    if (getVariable(textValue) != null) {
                        firstchild.setNodeValue(textValue + "__function");
                        mathml.removeAttribute("type");
                    }
                }
            }
            else {
                firstchild.setNodeValue(textValue.replaceAll("\\.", "_"));
            }
        }
        else if (mathml.getTagName().equals("cn")) {
            String xmmlunits = mathml.getAttribute("xmml:units");
            if (!xmmlunits.equals("")) {
                mathml.removeAttribute("xmml:units");
                mathml.setAttribute("cellml:units", xmmlunits);
            }
        }
    }

    //Find all the 'ci' elements in 'mathml', and replace all variable names
    // with the '__init' form of that variable name, for all variables
    // found in 'varToInits'.
    private void renameInitVars(Element mathml,  LinkedHashMap<String, String> varToInits) {
        if (mathml.getTagName().equals("math")) {
            NodeList children = mathml.getElementsByTagName("*");
            for (int c=0; c<children.getLength(); c++) {
                Element child = (Element) children.item(c);
                renameInitVars(child, varToInits);
            }
        }
        else if (mathml.getTagName().equals("ci")) {
            String textValue = "";
            Node firstchild = mathml.getFirstChild();
            if (firstchild != null) {
                textValue = firstchild.getNodeValue();
            }
            for (Map.Entry<String, String> entry: varToInits.entrySet()) {
                if (textValue.equals(entry.getKey())) {
                    firstchild.setNodeValue(entry.getValue());
                }
            }
        }
    }


    //Add a warning.
    protected void addWarning(String warning) {
        warnings.add(warning);
    }

    // simple query
    private StringList getWarnings() { return warnings; }

    //When a reasonable CellML delay construct exists, find 'translateDelay'
    // from sbml/SBWriter.java, copy it in here, and translate it to use
    // CellML concepts/constructs.

    //// TEST HARNESS
    public static final void main(String[] args)
        throws Exception {
        File f = new File(args[0]);
        String options = args[1];
        Document xdoc = UtilXML.parse(f);
        CMLWriter wrt = new CMLWriter(xdoc, new StringList());
        Document cellml = wrt.getCellML(options);
	XMLWriter xwrt = new XMLWriter();
	StringWriter swrt = new StringWriter();
        xwrt.write(cellml, swrt);
        System.out.println(swrt.toString());
        StringList warns = wrt.getWarnings();
        System.err.println(warns);
    } 
}

