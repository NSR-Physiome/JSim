/*NSRCOPYRIGHT
  Copyright (C) 1999-2008 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// convert XMML to SBML

package JSim.sbml; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*; 

import java.io.*;
import java.util.*;
import org.w3c.dom.*;

import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;

import org.sbml.libsbml.*;

public class SBWriter {
    private Document xdoc; // XMML document
    private long mLevel, mVersion;
    protected SBMLDocument sdoc; //SBML document
    protected Model model;
    protected StringList warnings;
    protected Element xroot, xmodel, xtoolList, 
        xunitList, xvarList, xeventList, xrelationList;
    private SBUnitWriter unitWriter;
    private ArrayList<String> mBadFunctions;
    static private boolean sbmlPresent;

    // maps
    private Hashtable<String,Element> xvarIDs,
        xtoolIDs, xeventIDs, xrelationIDs;
    private String xtime;

    // fundamental units translator
    private static final String[] xfunds = new String[] {
    "kg", "m", "sec", "amp", "degK", "mol", "candela"
    };
    private static final String[] sfunds = new String[] {
    "kilogram", "meter", "second", "ampere", "kelvin",
    "mole", "candela"
    };

    // initialize native libSBML
    static  {
        try
        {
            System.loadLibrary("sbmlj");
            /* Extra check to be sure we have access to libSBML: */
            Class.forName("org.sbml.libsbml.libsbml");
            sbmlPresent = true;
        }
        catch (Throwable e)
        {
            sbmlPresent = false;
            e.printStackTrace();
        }
    }

    // constructor
    public SBWriter(Document xdoc, StringList warnings) 
        throws Exception {
        if (!sbmlPresent) throw new Xcept("libSBML native library not found.");
        this.xdoc = xdoc;
        this.mLevel = 2;
        this.mVersion = 4;
        this.sdoc = new org.sbml.libsbml.SBMLDocument(mLevel,mVersion);
        this.model = this.sdoc.createModel();
        this.warnings = warnings;
        xroot = xdoc.getDocumentElement();
        xmodel = getFirstElement(xroot, "model");
        xtoolList = getFirstElement(xroot, "toolList");
        xunitList = getFirstElement(xmodel, "unitList");
        xvarList = getFirstElement(xmodel, "variableList");
        xeventList = getFirstElement(xmodel, "eventList");
        xrelationList = getFirstElement(xmodel, "relationList");
        xvarIDs = loadIDMap(xvarList);
        xtoolIDs = loadIDMap(xtoolList);
        xeventIDs = loadIDMap(xeventList);
        xrelationIDs = loadIDMap(xrelationList);
        unitWriter = new SBUnitWriter(this);
        mBadFunctions = new ArrayList<String>();
    }
    
    // make SBML 
    public String getSBML(String options) throws Exception {
        findTime();
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
        org.sbml.libsbml.SBMLWriter writer = new org.sbml.libsbml.SBMLWriter();
        long level = 2;
        long version = 4;
        boolean strict = false;
        long numerrors = 0;
        String validlevels = "Valid level/version combinations are level 1 version 2 (l1v2), level 2 versions 1-4 (l2v1, l2v2, l2v3, and l2v4), and level 3 version 1 (l3v1).";
        if (options != null) {
            String[] optionlist = options.split(",");
            for(String option : optionlist) {
                if(option.matches("[Ll]1v1")) {
                    throw new Xcept("Unable to export SBML Level 1 Version 1." + validlevels);
                }
                if(option.matches("[Ll]1[Vv]2")) {
                    level = 1;
                    version = 2;
                    //numerrors = sdoc.checkL1Compatibility();
                }
                else if(option.matches("[Ll]2[Vv]1")) {
                    level = 2;
                    version = 1;
                    // numerrors = sdoc.checkL2v1Compatibility();
                }
                else if(option.matches("[Ll]2[Vv]2")) {
                    level = 2;
                    version = 2;
                    //numerrors = sdoc.checkL2v2Compatibility();
                }
                else if(option.matches("[Ll]2[Vv]3")) {
                    level = 2;
                    version = 3;
                    //numerrors = sdoc.checkL2v3Compatibility();
                }
                else if(option.matches("[Ll]2[Vv]4")) {
                    level = 2;
                    version = 4;
                    //numerrors = sdoc.checkL2v4Compatibility();
                }
                else if(option.matches("[Ll]3[Vv]1")) {
                    level = 3;
                    version = 1;
                    //numerrors = sdoc.checkL3v1Compatibility();
                }
                else if (option.matches("[Ll]\\d[Vv]\\d")) {
                    throw new Xcept("Unable to export SBML Level " + option.substring(1,2) + " Version " + option.substring(3,4) + ".  " + validlevels);
                }
                else if (option.equals("strict")) {
                    strict = true;
                }
                else {
                    throw new Xcept("Unknown SBML export option '" + option + "'.  " + validlevels);
                }
            }
        }
        //Clear the error log and try to set the level and version.
        sdoc.getErrorLog().clearLog();
        if (!sdoc.setLevelAndVersion(level, version, strict)) {
            warnings.add("Unable to convert SBML model to level " + level + ", version " + version + ".  The default was used instead (l" + mLevel + "v" + mVersion + ").");
        }

        //If there are errors now, it's because of the attempted conversion.  Add those to the warnings list.
        for (long err=0; err<sdoc.getNumErrors(); err++) {
            SBMLError error = sdoc.getError(err);
            if (error.getSeverity() >= 2) {
                warnings.add("Translation error from libsbml: " + sdoc.getError(err).getMessage());
                //System.out.println("Translation error from libsbml: " + sdoc.getError(err).getMessage());
            }
        }
        sdoc.getErrorLog().clearLog();
        sdoc.checkInternalConsistency();
        // sdoc.checkConsistency(); //??? crashes libsbml ???
        for (long err=0; err<sdoc.getNumErrors(); err++) {
            SBMLError error = sdoc.getError(err);
            if (error.getSeverity() >= 2) {
                warnings.add("Validation error from libsbml: " + sdoc.getError(err).getMessage());
                //System.out.println("Validation error from libsbml: " + sdoc.getError(err).getMessage());
            }
        }
        //        return writer.writeToString(sdoc); // deprecated
        return writer.writeSBMLToString(sdoc);
    }

    public StringList getWarnings() {
        return warnings;
    }

    public void findTime() throws Xcept {
        ArrayList<String> domains = new ArrayList<String>();
        //Find the domains that the ODETools and the PDEtools
        ArrayList<Element> xtools = 
            new ArrayList<Element>(xtoolIDs.values());
        for (int i=0; i<xtools.size(); i++) {
            Element xtool = xtools.get(i);
            String type = xtool.getNodeName();
            if (type.equals("ODETool") || type.equals("PDETool")) {
                String timedomain = xtool.getAttribute("timeDomainID");
                domains.add(timedomain);
            }
        }

        //Also, if there are events, they are probably also in time:
        if (xeventList != null) {
            NodeList nodes = xeventList.getChildNodes();
            for (int i=0; i<nodes.getLength(); i++) {
                if (! (nodes.item(i) instanceof Element)) continue;
                Element event = (Element) nodes.item(i);
                String timedomain = event.getAttribute("timeDomainID");
                domains.add(timedomain);
            }
        }

        //Now find the most frequenly used symbol for an ODE, PDE, and event domain:
        if (domains.size()==0) {
            //Can't find any time domains this way; try by name.
            findTimeByName();
            return;
        }
        Collections.sort(domains);
        String candidate = domains.get(0);
        int freq = 0;
        int prevfreq = 0;
        for (int d=1; d<domains.size(); d++) {
            if (candidate.equals(domains.get(d))) {
                freq++;
            }
            else {
                if (candidate.equals(domains.get(d-1))) {
                    prevfreq = freq;
                    freq = 1;
                    candidate = domains.get(d);
                }
            }
            if (freq > prevfreq) {
                prevfreq = freq;
                candidate = domains.get(d);
            }
        }
        xtime = candidate;
    }
    public void findTimeByName() throws Xcept {
        if (xtime != null) return; //Why did we call this?
        ArrayList<Element> xvars = 
            new ArrayList<Element>(xvarIDs.values());
        ArrayList<String> domains;
        for (int i=0; i<xvars.size(); i++) {
            Element xvar = xvars.get(i);
            String name = xvar.getAttribute("id");
            if (! isVarUsable(name)) continue;
            String isDomain = xvar.getAttribute("isDomain");
            if (isDomain.equals("true")) {
                if (name.equals("t")) {
                    xtime = name;
                    return;
                }
                if (name.equals("time")) {
                    xtime = name;
                    return;
                }
            }
        }
    }
    // create SBML parameters (might later create SBML species)
    public void loadVars() throws Xcept {
        ArrayList<Element> xvars = new ArrayList<Element>(xvarIDs.values());
        for (int i=0; i<xvars.size(); i++) {
            Element xvar = xvars.get(i);
            String name = xvar.getAttribute("id");
            if (! isVarUsable(name)) continue;
            String isDomain = xvar.getAttribute("isDomain");
            if (isDomain.equals("true")) {
                if (xtime != null && !xtime.equals(name)) {
                    warnings.add("Warning: Multiple domains not supported.  Domain '" + name + "' ignored; domain '" + xtime + "' used instead for SBML implicit time.");
                }
                else {
                    xtime = name;
                    continue;
                }
            }
            Parameter param = model.getParameter(name);
            if (param == null) {
                param = model.createParameter();
                if (param.setId(name) == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE) {
                    warnings.add("Warning:  Unable to create the variable '" + name + "', as it is an illegal SBML name.");
                    model.removeParameter(model.getNumParameters()-1);
                    continue;
                }
            }
            NodeList domainlist = xvar.getElementsByTagName("domainList");
            if (domainlist.getLength() > 0) {
                param.setConstant(false);
            }
            else {
                param.setConstant(true);
            }
            //System.out.println("Setting units for " + name);
            String unit = xvar.getAttribute("unitID");
            if (unit != null && !unit.equals("")) {
                unit = unitWriter.toSBMLFromJSim(unit);
                param.setUnits(unit);
            }
            //System.out.println("Set units.");
        }
        //System.out.println("done with vars, too.");
    }

    // create SBML rules
    public void loadTools() throws Xcept {
        ArrayList<Element> xtools = 
            new ArrayList<Element>(xtoolIDs.values());
        for (int i=0; i<xtools.size(); i++) {
            Element xtool = xtools.get(i);
            String type = xtool.getNodeName();
            if (type.equals("exprTool"))
                makeAssignmentRule(xtool);
            else if (type.equals("implicitTool")) 
                makeAlgebraicRules(xtool);
            else if (type.equals("ODETool"))
                makeRateRule(xtool);
            else if (type.equals("domainTool"))
                continue;
            else if (type.equals("externTool")) {
                //checkExtern(xtool);
                //We don't worry about this--an externally defined variable will still be externally defined in the translated SBML.
            }
            else {
                warnings.add("Warning: " + type + "s are not translatable to SBML: ignoring this part of the model.");
            }
        }
    }
    
    // create assignment rule or initial value from exprTool
    public void makeAssignmentRule(Element xtool)
        throws Xcept {
        Element vsols = getFirstElement(xtool, "solvedVariableList");
        Element vsol = getFirstElement(vsols, "variableUsage");
        String vid = vsol.getAttribute("variableID");
        if (! isVarUsable(vid)) return;
        Element expr = getFirstElement(xtool, "expression");
        Element math = getFirstElement(expr, "math");
        fixTminAndDelays(math);
        String mathml = elementToString(math);
        ASTNode astn = org.sbml.libsbml.libsbml.readMathMLFromString(mathml);
        Element debug = getFirstElement(expr, "debug");
        String origtxt = "";
        if (debug != null) {
            String debstr = elementToString(debug);
            Node sub = debug.getFirstChild();
            origtxt = sub.getNodeValue();
            origtxt = " ('" + origtxt + "')";
        }
        if (astn==null) {
            warnings.add("Warning: Unable to translate definition of '" + vid + "'" + origtxt + " to SBML.");
            return;
        }
        fixTime(astn);
        translateDelay(astn);
        if (hasBadNodes(astn)) {
            warnings.add("Warning: Unable to translate definition of '" + vid + "'" + origtxt + " to SBML.");
            return;
        }
        //System.out.println(elementToString(math));
        String formula = libsbml.formulaToString(astn);
        //System.out.println("formula: \n" + formula);
        String init = libsbml.formulaToString(astn);
        Parameter p = model.getParameter(vid);
        if (p == null) {
            p = model.createParameter();
            if (p.setId(vid) == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE) {
                warnings.add("Warning:  Unable to create the variable '" + vid + "', as it is an illegal SBML name.");
                model.removeParameter(model.getNumParameters()-1);
                return;
            }
        }
        try {
            Double d = Double.parseDouble(init);
            //If that didn't throw, it's a real double.
            p.setValue(d);
        }
        catch (Exception e) {
            //Need a rule
            String status = vsol.getAttribute("status");
            if (status.equals("CURR")) {
                p.setConstant(false);
                AssignmentRule ar = model.createAssignmentRule();
                ar.setVariable(vid);
                ar.setMath(astn);
            }
            else if (status.equals("MIN")) {
                InitialAssignment ia = model.createInitialAssignment();
                ia.setSymbol(vid);
                ia.setMath(astn);
            }
            else {
                throw new Xcept("Unknown status for variable '" + vid + "':  " + status);
            }
        }
    
    }

    public String elementToString(Element elin)
        throws Xcept {
        StringWriter sw = new StringWriter();
        StreamResult result = new StreamResult(sw);

        DOMSource source = new DOMSource(elin);

        //Do the transformation and output
        //Set up the transformer to write the output string;
        try {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer transformer = tFactory.newTransformer();
            transformer.setOutputProperty("indent", "no");
            transformer.transform(source, result);
        }
        catch (Exception e) {
            throw new Xcept("Unable to create XML transformer for MML to SBML translator.");
        }
        String ret = sw.toString();
        //ret = ret.replaceFirst("<.*?>", "");
        return ret;
    }

    /* public static String nodeToString(Node node){
       DOMImplementation impl = node.getOwnerDocument().getImplementation();
       DOMImplementationLS factory = (DOMImplementationLS) impl.getFeature("LS", "3.0");
       LSSerializer serializer = factory.createLSSerializer();
       return serializer.writeToString(node);
       }
    */
    // create algebraic rule from implicitTool
    public void makeAlgebraicRules(Element xtool)
        throws Xcept {
        Element zeroexplist = getFirstElement(xtool, "zeroExpressionList");
        NodeList expressions = zeroexplist.getElementsByTagName("expression");
        Element solvedvarlist = getFirstElement(xtool, "solvedVariableList");
        NodeList vars = solvedvarlist.getElementsByTagName("variableUsage");
        LinkedHashMap<String, String> varToInits = new LinkedHashMap<String, String>();
        for (int vnum=0; vnum<vars.getLength(); vnum++) {
            Element var = (Element) vars.item(vnum);
            String id = var.getAttribute("variableID");
            Parameter param = model.getParameter(id);
            if (param == null) {
                warnings.add("Warning: unable to find the variable " + id + " which was to be solved by an implicit equation.");
                continue;
            }
            String status = var.getAttribute("status");
            param.setConstant(false);
            if (status.equals("MIN")) {
                String initname = id + "_init";
                Parameter initparam = model.getParameter(initname);
                if (initparam == null) {
                    initparam = model.createParameter();
                    if (initparam.setId(initname) == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE) {
                        warnings.add("Warning:  Unable to create the variable '" + initname + "', as it is an illegal SBML name.");
                        model.removeParameter(model.getNumParameters()-1);
                        continue;
                    }
                }
                initparam.setConstant(false);
                varToInits.put(id, initname);
            }
            else if (!status.equals("CURR")) {
                warnings.add("Warning:  unknown implicit tool status " + status + " when determining how to solve variable " + id + " with an implicit equation.");
            }
        }
        for (int exp=0; exp<expressions.getLength(); exp++) {
            Element expression = (Element) expressions.item(exp);
            Element math = getFirstElement(expression, "math");
            fixTminAndDelays(math);
            renameInitVars(math, varToInits);
            String mathml = elementToString(math);
            ASTNode astn = org.sbml.libsbml.libsbml.readMathMLFromString(mathml);
            String origtxt = "";
            Element debug = getFirstElement(expression, "debug");
            if (debug != null) {
                String debstr = elementToString(debug);
                Node sub = debug.getFirstChild();
                origtxt = sub.getNodeValue();
                origtxt = " ('" + origtxt + "')";
            }
            if (astn==null) {
                warnings.add("Warning: Unable to translate definition of implicit expression" + origtxt + " to SBML.");
                return;
            }
            fixTime(astn);
            translateDelay(astn);
            if (hasBadNodes(astn)) {
                warnings.add("Warning: Unable to translate definition of implicit expression" + origtxt + " to SBML.");
                return;
            }
            AlgebraicRule ar = model.createAlgebraicRule();
            ar.setMath(astn);
        }
    }

    // create rate rule from ODETool
    public void makeRateRule(Element xtool)
        throws Xcept {
        Element vsols = getFirstElement(xtool, "solvedVariableList");
        Element vstate = null;
        Element vu = getFirstElement(vsols, "variableUsage");
        String vid = vu.getAttribute("variableID");
        if (! isVarUsable(vid)) return;
        Parameter p = model.getParameter(vid);
        if (p == null) {
            p = model.createParameter();
            if (p.setId(vid) == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE) {
                warnings.add("Warning:  Unable to create the variable '" + vid + "', as it is an illegal SBML name.");
                model.removeParameter(model.getNumParameters()-1);
                return;
            }
        }
        p.setConstant(false);
        Element state = getFirstElement(xtool, "stateEquation");
        String toolID = state.getAttribute("toolID");
        Element tool = xtoolIDs.get(toolID);
        String type = tool.getNodeName();
        if (! type.equals("exprTool")) {
            warnings.add("Warning:  Rate rule type " + type + " not supported.");
            return;
        }
        Element expr = getFirstElement(tool, "expression");
        Element math = getFirstElement(expr, "math");
        fixTminAndDelays(math);
        String mathml = elementToString(math);
        ASTNode astn = org.sbml.libsbml.libsbml.readMathMLFromString(mathml);
        String origtxt = "";
        Element debug = getFirstElement(expr, "debug");
        if (debug != null) {
            String debstr = elementToString(debug);
            Node sub = debug.getFirstChild();
            origtxt = sub.getNodeValue();
            origtxt = " ('" + origtxt + "')";
        }
        if (astn==null) {
            warnings.add("Warning: Unable to translate definition of '" + vid + "'" + origtxt + " to SBML.");
            return;
        }
        fixTime(astn);
        translateDelay(astn);
        if (hasBadNodes(astn)) {
            warnings.add("Warning: Unable to translate definition of '" + vid + "'" + origtxt + " to SBML.");
            return;
        }
        RateRule rr = model.createRateRule();
        int success = rr.setVariable(vid);
        if (success != libsbml.LIBSBML_OPERATION_SUCCESS) {
            throw new Xcept("Unable to create rate rule for parameter '" + vid + "'.");
        }
        rr.setMath(astn);
    }

    public void checkExtern(Element xtool) throws Xcept {
        Element vsols = getFirstElement(xtool, "solvedVariableList");
        Element vu = getFirstElement(vsols, "variableUsage");
        String vid = vu.getAttribute("variableID");
        if (vid.indexOf("time") < 0) {
            warnings.add("Warning: incompletely defined variable '" + vid + "' due to extern tool being ignored.");
            return;
        }
    }

    public void fixTminAndDelays(Element mathml) {
        if (mathml.getTagName().equals("math")) {
            NodeList children = mathml.getElementsByTagName("*");
            for (int c=0; c<children.getLength(); c++) {
                Element child = (Element) children.item(c);
                fixTminAndDelays(child);
            }
        }
        else if (mathml.getTagName().equals("ci")) {
            String textValue = "";
            Node firstchild = mathml.getFirstChild();
            if (firstchild != null) {
                textValue = firstchild.getNodeValue();
            }
            if (textValue.indexOf(".") != -1) {
                //System.err.println("Found a '.' in mathml");
                firstchild.setNodeValue(textValue.replaceAll("\\.", "__"));
            }
            else if (mathml.hasAttribute("type")) {
                if (mathml.getAttribute("type").equals("function")) {
                    if (model.getParameter(textValue) != null) {
                        firstchild.setNodeValue(textValue + "__function");
                        mathml.removeAttribute("type");
                    }
                }
            }
        }
    }

    public void renameInitVars(Element mathml,  LinkedHashMap<String, String> varToInits) {
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

    public boolean hasBadNodes(ASTNode astn) {
        if (astn==null) return false;
        if (astn.isOperator() == false && astn.isNumber() == false) {
            String nodename = astn.getName();
            if (nodename.indexOf('.') != -1) {
                //LS DEBUG:  at some point, we may want to rename some of these guys instead of giving up.
                return true;
            }
            for (int b=0; b<mBadFunctions.size(); b++) {
                //System.err.println("Bad function found!");
                if (nodename.equals(mBadFunctions.get(b))) return true;
            }
        }
        for (long c = 0; c < astn.getNumChildren() ; c++) {
            if (hasBadNodes(astn.getChild(c))) return true;
        }
        return false;
    }

    public void fixTime(ASTNode astn) {
        if (astn==null) return;
        if (astn.isOperator() == false && astn.isNumber() == false) {
            if (astn.getName().equals("time")) {
                astn.setType(libsbml.AST_NAME_TIME);
            }
            if (astn.getName().equals(xtime)) {
                astn.setType(libsbml.AST_NAME_TIME);
            }
        }
        for (long c = 0; c < astn.getNumChildren() ; c++) {
            fixTime(astn.getChild(c));
        }
    }

    public void translateDelay(ASTNode astn) throws Xcept{
        if (astn==null) return;
        String name = astn.getName();
        //System.out.println("Node name: " + name);
        if (name != null && name.indexOf("__function") >= 0) {
            //System.out.println("found __function.");
            //Hopefully we can translate this to a delay.
            String vid = astn.getName();
            vid = vid.substring(0, name.indexOf("__function"));
            Parameter param = model.getParameter(vid);
            ASTNode paramnode = new ASTNode();
            paramnode.setName(vid);
            ASTNode time = new ASTNode();
            time.setName("time");
            time.setType(libsbml.AST_NAME_TIME);
            if (nodeIsOnlyTmin(astn)) {
                //Option 1:  param(time.min) -> delay(time)
                //System.out.println("Option 1.");
                //This is probably used in some context that could be shortened overall to a simple delay function, but it should still work this way.
                astn.setName("delay");
                astn.setType(libsbml.AST_FUNCTION_DELAY);
                astn.replaceChild(0, paramnode);
                astn.addChild(time);
            }
            else if (nodeIsFunctionWithoutTmin(astn)) {
                if (astn.getNumChildren() != 1) {
                    warnings.add("Warning: This function is untranslateable to SBML, as it involves multiple domains: '" + libsbml.formulaToString(astn) + "'.");
                    //System.err.println("Added bad function " + astn.getName());
                    mBadFunctions.add(astn.getName());
                    return;
                }
                ASTNode child = astn.getChild(0);
                ASTNode func = getFunctionOfTminus(child);
                if (func != null) {
                    //Option 2a:  param(time - function) -> delay(function)
                    //System.err.println("Option 2a.");
                    astn.setName("delay");
                    astn.setType(libsbml.AST_FUNCTION_DELAY);
                    astn.replaceChild(0, paramnode);
                    astn.addChild(func);
                }
                else {
                    //Option 2b:  param(function) -> delay(time - function)
                    //System.err.println("Option 2b.");
                    astn.setName("delay");
                    astn.setType(libsbml.AST_FUNCTION_DELAY);
                    astn.replaceChild(0, paramnode);
                    ASTNode minus = new ASTNode();
                    minus.setName("minus");
                    minus.setType(libsbml.AST_MINUS);
                    minus.addChild(time);
                    minus.addChild(child);
                    astn.addChild(minus);
                }
            }
            else {
                ASTNode func = getDelayFromCanonical(astn);
                if (func != null) {
                    //Option 3:  param(if(func) time.min otherwise time-X) -> delay(X)  [the default delay translation]
                    //System.err.println("Option 3: time - " + libsbml.formulaToString(func));
                    astn.setName("delay");
                    astn.setType(libsbml.AST_FUNCTION_DELAY);
                    astn.replaceChild(0, paramnode);
                    astn.addChild(func);
                }
                else {
                    //Option 4:  something with time.min that's not one of the above
                    throw new Xcept("Unable to parse '"  + libsbml.formulaToString(astn) + "' into SBML as a delay function--it used 'time.min' in an unexpected way.  For models intended to be translated to SBML, the format 'X(time-y)' is sufficient.");
                }
            }

            //System.out.println(vid);
        }
        else {
            for (long c = 0; c < astn.getNumChildren() ; c++) {
                translateDelay(astn.getChild(c));
            }
        }
    }

    public boolean nodeIsOnlyTmin(ASTNode astn) throws Xcept{
        if (astn == null) return false;
        if (astn.getNumChildren() != 1) {
            return false;
        }
        ASTNode child = astn.getChild(0);
        if (child.getNumChildren() != 0) return false;
        String name = child.getName();
        if (name == null) return false;
        return (name.equals(xtime+ "__min"));
    }

    public ASTNode getFunctionOfTminus(ASTNode astn) throws Xcept{
        //ASTNode child = astn.getChild(0);
        if (astn.getType() != libsbml.AST_MINUS) return null;
        if (astn.getNumChildren() != 2) return null;
        if (astn.getChild(0).getName() == null) return null;
        if (!astn.getChild(0).getName().equals(xtime)) return null;
        return astn.getChild(1);
    }

    public boolean nodeIsFunctionWithoutTmin(ASTNode astn) {
        return (!(libsbml.formulaToString(astn).indexOf(xtime + "__min") >=0) );
    }

    public ASTNode getDelayFromCanonical(ASTNode astn) throws Xcept {
        ASTNode child = astn.getChild(0);
        if (!child.isPiecewise()) return null;
        //System.out.println("is piecewise...");
        if (child.getNumChildren() != 3) return null;
        //System.out.println("Does have three children...");
        ASTNode tmin = child.getChild(0);
        ASTNode func = child.getChild(1);
        ASTNode otherwise = child.getChild(2);
        if (!tmin.getName().equals(xtime + "__min")) return null;
        //System.out.println("Second child is time__min...");
        //We don't care what the 'if' statement is--we assume it's correct, and involves the delay.
        ASTNode retfunc = getFunctionOfTminus(otherwise);
        if (retfunc == null) {
            retfunc = new ASTNode();
            retfunc.setName("minus");
            retfunc.setType(libsbml.AST_MINUS);
            ASTNode time = new ASTNode();
            time.setName("time");
            time.setType(libsbml.AST_NAME_TIME);
            retfunc.addChild(time);
            retfunc.addChild(otherwise.getChild(0));
        }
        //System.out.println("final canonical: " + libsbml.formulaToString(retfunc));
        return retfunc;
    }

    // is this var name usable by SBML?
    public boolean isVarUsable(String name) throws Xcept {
        if (name.indexOf("__init")>=0) {
            warnings.add("Warning: XMML file created without 'makeDEICParms=false' defined, resulting in the creation of '__init' variables.");
        }
        if (name.indexOf("__")>=0) return false; // JSim created
        if (name.indexOf(":")>=0) return false; // derivative

        // domain control hack should really test against xtime
        if (name.endsWith(".ct")) return false; 
        if (name.endsWith(".min")) return false; 
        if (name.endsWith(".max")) return false; 
        if (name.endsWith(".delta")) return false;
        return true;
    }

    public void loadEvents() {
        if (xeventList == null) return; //No events; no warnings.
        NodeList nodes = xeventList.getChildNodes();
        if (nodes.getLength() != 0) {
            /*
              xeventIDs = loadIDMap(xvarList);
              ArrayList<Element> xevents = 
              new ArrayList<Element>(xeventIDs.values());
              if (xevents.size() > 0) {
            */
            warnings.add("Warning: Translation of JSim events to SBML is not yet supported.");
        }

        return;
    }

    public void loadConstraints() 
        throws Xcept {
        if (xrelationList == null) return; //No relations; no warnings.
        ArrayList<Element> xrelations = 
            new ArrayList<Element>(xrelationIDs.values());
        for (int i=0; i<xrelations.size(); i++) {
            Element xrelation = xrelations.get(i);
            makeConstraint(xrelation);
        }
        //warnings.add("Warning: Translation of JSim relations to SBML is not yet supported.");
        return;
    }

    public void makeConstraint(Element xrelation)
        throws Xcept {
        Element expr = getFirstElement(xrelation, "expression");
        Element math = getFirstElement(expr, "math");
        fixTminAndDelays(math);
        String mathml = elementToString(math);
        ASTNode astn = org.sbml.libsbml.libsbml.readMathMLFromString(mathml);
        Element debug = getFirstElement(expr, "debug");
        String origtxt = "";
        if (debug != null) {
            String debstr = elementToString(debug);
            Node sub = debug.getFirstChild();
            origtxt = sub.getNodeValue();
            origtxt = " ('" + origtxt + "')";
        }
        if (astn==null) {
            warnings.add("Warning: Unable to translate the relation '" + origtxt + " to SBML.");
            return;
        }
        fixTime(astn);
        translateDelay(astn);
        if (hasBadNodes(astn)) {
            warnings.add("Warning: Unable to translate the relation '" + origtxt + " to SBML.");
            return;
        }
        Constraint cstr = model.createConstraint();
        cstr.setMath(astn);
    }
    //// UTILITIES
    
    // 1st child element of given name
    private Element getFirstElement(Element base, String name) {
        NodeList nodes = base.getElementsByTagName(name);
        return (Element) nodes.item(0);
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

    //// TEST HARNESS
    public static final void main(String[] args)
        throws Exception {
        File f = new File(args[0]);
        String options = args[1];
        Document xdoc = UtilXML.parse(f);
        SBWriter wrt = new SBWriter(xdoc, new StringList());
        String sbml = wrt.getSBML(options);
        System.out.println(sbml);
        StringList warns = wrt.getWarnings();
        System.err.println(warns);
    } 
}
