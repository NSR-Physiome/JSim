/*NSRCOPYRIGHT
  Copyright (C) 1999-2008 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML model document for import

package JSim.sbml;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.mml.*;
import JSim.mathml.*;

import java.io.*;
import java.util.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.NodeList; 

import org.sbml.libsbml.*;

public class SBModel implements MLNameSpace {    
    protected org.sbml.libsbml.SBMLDocument document;
    protected org.sbml.libsbml.Model model;
    protected UnitNList sysUnits; // JSim defined units, typically nsrunit
    protected SBUnitChecker unitChecker;
    protected SBCompartment.NList sbcomps;
    protected SBUnitSpace mSBUnitSpace;
    protected SBUnit.NList sbunits;
    protected SBEvent.NList sbevents;
    protected SBFunction.NList sbfuncs;
    protected SBParameter.NList sbparms;
    protected SBReaction.NList sbreacs;
    protected SBRule.NList sbrules;
    protected SBSpecies.NList sbspecs;
    protected SBVar.NList sbvars;
    protected SBConstraint.NList sbconstraints;
    protected MLCSymbol.NList csymbols;
    protected int nDelayExpr; // # delay scratch vars
    protected boolean abortEvents; // abort if events found
    protected NamedRealConst timeExpr;
    protected ConstExpr avogadroExpr;
    protected SBContext ctxt; // for MML Expr rendering
    protected ArrayList<String> changedUnits;
    protected ArrayList<String> mWarnings;
    protected ArrayList<UnitDefinition> mExtraUnitDefinitions;
    static private boolean sbmlPresent;

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

    // default metaunits
    private static final String muNames[] = new String[] {
    "substance", "volume", "area", "length", "time", "item"
    };
    private static final String muDefs[] = new String[] {
    "mole", "litre", "metre^2", "metre", "second", "dimensionless"
    };
    
    // time url
    public static final String TIME_URL = 
        "http://www.sbml.org/sbml/symbols/time";
    public static final String DELAY_URL = 
        "http://www.sbml.org/sbml/symbols/delay";
    public static final String AVOGADRO_URL = 
        "http://www.sbml.org/sbml/symbols/avogadro";

    // constructor
    public SBModel(String text, UnitNList sysUnits, 
                   String options) throws Xcept {
        if (!sbmlPresent) throw new Xcept("libSBML native library not found.");

        mWarnings = new ArrayList<String>();
        SBMLReader reader = new SBMLReader();
        document = reader.readSBMLFromString(text);
        model = document.getModel();
        this.sysUnits = sysUnits;
        unitChecker = new SBUnitChecker(sysUnits);
        fixNames();
        
        sbvars = new SBVar.NList();
        ctxt = new SBContext(this);

        // load options
        abortEvents = false;

        // load csymbols
        csymbols = new MLCSymbol.NList();
        MLCSymbol delay = new MLCSymbol("delay") {
                public Expr makeExpr(Expr.List args) throws Xcept {
                    if (args.size() != 2) throw new Xcept("csymbol delay requires 2 arguments");
                    String v = args.expr(0).toString(ctxt);
                    SBVar var = (SBVar) sbvars.getByName(v);
                    if (var != null && !var.hasKnownVariance()) {
                        //JSim would choke on a delay equation for a non-varying parameter
                        var.inDelay = true;
                    }
                    if (! Util.onlyLettersAndDigits(v)) {
                        SBVar d = makeSBVar("delayExpr" + nDelayExpr++, "delay");
                        d.setAssign(v);
                        d.setConst(false);
                        v = d.mmlName;
                    }
                    String delay = args.expr(1).toString(ctxt);
                    String s = "" + v + 
                        "(if (time<(time.min+" + delay + 
                        ")) time.min else time-(" + delay + "))";
                    return new NamedRealConst(s, Double.NaN);
                }
            };
        csymbols.add(delay);

        // time variables
        timeExpr = new NamedRealConst("time", Double.NaN);
        addTime("time");

        // avogadro variables
        avogadroExpr = Expr.cons(6.02214179e23);
        addAvogadro("avogadro");

        // load SBML components
        changedUnits = new ArrayList<String>();
        mSBUnitSpace = new SBUnitSpace(document, this);
        mExtraUnitDefinitions = new ArrayList<UnitDefinition>();
        sbunits = new SBUnit.NList(this);
        for (int i=0; i<muNames.length; i++) {
            if (sbunits.sbunit(muNames[i]) != null) continue;
            mSBUnitSpace.addSBMLJSimConversion(muNames[i], muDefs[i]);
        }
        for (int ud=0; ud<mExtraUnitDefinitions.size(); ud++) {
            sbunits.addFirst(this, mExtraUnitDefinitions.get(ud));
        }
        //System.out.println("Creating functions.");
        sbfuncs = new SBFunction.NList(this); // prevents nesting
        //System.out.println("Creating compartments.");
        sbcomps = new SBCompartment.NList(this);
        //System.out.println("Creating parameters.");
        sbparms = new SBParameter.NList(this);
        //System.out.println("Creating species.");
        sbspecs = new SBSpecies.NList(this);
        //System.out.println("Creating reactions.");
        sbreacs = new SBReaction.NList(this);
        //System.out.println("Creating events.");
        sbevents = new SBEvent.NList(this);
        //System.out.println("Creating rules.");
        sbrules = new SBRule.NList(this);
        //System.out.println("Creating constraints.");
        sbconstraints = new SBConstraint.NList(this);

        for (int i=0; i<sbvars.size(); i++) {
            sbvars.sbvar(i).checkCalc();
        }
        //System.out.println("Sorting algebraic rules.");
        SBAlgRuleSorter ars = new SBAlgRuleSorter(sbrules, sbvars);
        ars.classifyVars();
        String warnings = ars.getWarnings();
        if (warnings != null) {
            mWarnings.add(warnings);
        }
    }

    protected void addWarning(String warning) {
        mWarnings.add(warning);
    }

    private void fixNames() {
        //Replaces names that are invalid in mml with valid ones.
        // for(int var=0; var<document.GetModel().
    }

    public String getUniqueInitValue(String mmlName) {
        String name = mmlName + "_init";
        int suffix=1;
        while (sbvars.getByName(name) != null) {
            name = mmlName + "_init" + suffix;
        }
        return name;
    }

    public SBUnitSpace getSBUnitSpace() {
        return mSBUnitSpace;
    }

    // add time variable
    private void addTime(String name) {
        MLCSymbol tsym = new MLCSymbol(name) {
                public Expr makeExpr(Expr.List args) throws Xcept {
                    if (args.size() != 0) throw new Xcept
                        ("csymbol Time requires 0 arguments");
                    return timeExpr;
                }
            };
        csymbols.add(tsym);
    }

    private void addAvogadro(String name) {
        MLCSymbol asym = new MLCSymbol(name) {
                public Expr makeExpr(Expr.List args) throws Xcept {
                    if (args.size() != 0) throw new Xcept
                        ("csymbol avogadro requires 0 arguments");
                    return avogadroExpr;
                }
            };
        csymbols.add(asym);
    }

    // make sbvar hack for anonymous class use
    protected SBVar makeSBVar(String name, String role) throws Xcept {
        return new SBVar(this, name, role);
    }

    // write MML
    public void writeMML(Writer wrt) throws Xcept {
        PrintWriter out = new PrintWriter(wrt, true);
        out.println("// This model generated automatically from SBML");
        for (int i=0; i<mWarnings.size(); i++) 
            out.println("// " + mWarnings.get(i));
        for (int i=0; i<sbevents.size(); i++) 
            sbevents.sbevent(i).writeMML(out);
        for (int i=0; i<sbvars.size(); i++) 
            sbvars.sbvar(i).writeRenameWarning(out);
        out.println("");
                
        // units
        out.println("// unit definitions");
        if (!changedUnits.isEmpty()) {
            out.println("// Note:  the following unit(s) have been changed due to conflicts with built-in JSim unit definitions:");
            for (int i=0; i<changedUnits.size(); i++) {
                out.println("//    '" + changedUnits.get(i) + "' to '" + mmlUnitName(changedUnits.get(i)) + "'");
            }
        }
        out.println("import nsrunit;");
        out.println("unit conversion off;");
        for (int i=0; i<sbunits.size(); i++) 
            sbunits.sbunit(i).writeMML(out);
        out.println("");

        out.println("// SBML property definitions");
        out.println("property sbmlRole=string;");
        out.println("property sbmlName=string;");
        out.println("property sbmlCompartment=string;");
        out.println("");

        out.println("// SBML reactions");
        for (int i=0; i<sbreacs.size(); i++) 
            sbreacs.sbreac(i).writeMML(out);        
        out.println("");

        out.println("math main {");
        out.println("  realDomain time second;");
        out.println("  time.min=0;");
        out.println("  extern time.max;");
        out.println("  extern time.delta;");
        out.println("");

        out.println("  // variable definitions");
        for (int i=0; i<sbvars.size(); i++) 
            sbvars.sbvar(i).writeMMLDecl(out);
        out.println("");

        out.println("  // equations");
        for (int i=0; i<sbvars.size(); i++) 
            sbvars.sbvar(i).writeMMLEqn(out);
        for (int i=0; i<sbrules.size(); i++) 
            sbrules.sbrule(i).writeMML(out);
        for (int i=0; i<sbspecs.size(); i++) 
            sbspecs.sbspec(i).writeMML2(out);
        for (int i=0; i<sbparms.size(); i++) 
            sbparms.sbparm(i).writeMML(out);
        for (int i=0; i<sbcomps.size(); i++) 
            sbcomps.sbcomp(i).writeMML(out);
        for (int i=0; i<sbspecs.size(); i++) 
            sbspecs.sbspec(i).writeMML(out);
        out.println("");

        if (sbfuncs.size() > 0) {
            out.println("  // Used function definitions");
            for (int i=0; i<sbfuncs.size(); i++) 
                sbfuncs.sbfunc(i).writeMML(out);
            out.println("");
        }

        if (sbconstraints.size() > 0) {
            out.println("  // relations");
            for (int i=0; i<sbconstraints.size(); i++) 
                sbconstraints.sbconstraint(i).writeMML(out);
            out.println("");
        }

        out.println("  // variable properties");
        for (int i=0; i<sbvars.size(); i++) 
            sbvars.sbvar(i).writeMMLProp(out);

        // done 
        out.println("}");
    }

    // Expr from MathML text
    private Expr mathExpr(String text) throws Xcept {
        //LS NOTE:  I have no idea what this next line is doing, but I'm afraid
        // to remove it.  Removing a namespace?  For some reason?
        text = text.replaceAll("math:", "");
        MLMath math = new MLMath(text);
        try {
            Expr.List list = math.makeExprList(this, csymbols);
            if (list.size() == 0) return null;
            if (list.size() != 1) throw new Xcept("Expected single MathML expression, got:\n" + text);
            return list.expr(0);
        }
        catch(Xcept e) {
            ASTNode astn = libsbml.readMathMLFromString(text);
            String infix = libsbml.formulaToString(astn);
            String error = e.getMessage();
            error = error.replaceAll("\\n"," ");
            addWarning("MathML error:  " + error);
            addWarning("Untranslated math element:  " + infix);
        }
        return null;
    }

    // MML text from MathML text
    public String mathExprMML(String text) throws Xcept {
        Expr expr = mathExpr(text);
        if (expr == null) return null;
        return expr.toString(ctxt);
    }

    // Return the SBVar with the given name;
    public SBVar getSBVar(String name) {
        if (name==null) return null;
        return (SBVar) sbvars.getByName(name);
    }

    // NAMESPACE METHODS
    public Expr compByName(String name) throws Xcept {
        return new NamedRealConst(name, Double.NaN);
    }
    public String compNameByElement(Element elem) throws Xcept {
        String url = elem.getAttribute("definitionURL");
        if (url == null || url.length()==0) throw new Xcept("Unable to define csymbol " + UtilXML.getText(elem) + ": no definitionURL provided.");
        if (url.equalsIgnoreCase(TIME_URL)) return "time";
        if (url.equalsIgnoreCase(DELAY_URL)) return "delay";
        if (url.equalsIgnoreCase(AVOGADRO_URL)) return "avogadro";
        throw new Xcept("Unable to define csymbol " + UtilXML.getText(elem) + ": definitionURL " + url + " is unknown.");
    }
    public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
        throw new Xcept("makeDeriv() not implemented");
    }
    public JSim.expr.Unit unitByName(String name) throws Xcept {
        return JSim.expr.Unit.scalar();
    }
    public Expr funcCall(String name, Expr.List elist) throws
        Xcept {
        if (sbfuncs == null) throw new Xcept
            ("Nested function " + name + " not currently supported");
        SBFunction f = sbfuncs.sbfunc(name);
        if (f == null)
            throw new Xcept("SBML function " + name + " not defined");         
        return f.makeCall(elist);
    }

    public void  changeUnit(String unit) {
        changedUnits.add(unit);
    }

    public String mmlUnitName(String origunit) {
	if (origunit.length() > 0 && ! 
	    Character.isLetter(origunit.charAt(0)))
	    origunit = "xxx" + origunit;
        String jsim = mSBUnitSpace.toJSimFromSBML(origunit);
        if (jsim != null) return jsim;
        for (int i=0; i<changedUnits.size(); i++) {
            if (origunit.equals(changedUnits.get(i))) {
                return origunit + "_";
            }
        }
        return origunit;
    }


    // mainline test
    public static final void main(String[] args) throws Exception {
        for (int i=0; i<args.length; i++) {
            System.err.println("==== " + args[i]);
            File f = new File(args[i]);
            String text = "";

            // ??? BELOW NEEDS REAL SYSUNITS INSTEAD OF NULL PLACEHOLDER ???
            SBModel sbdoc = new SBModel(text, null, null);
            Writer wrt = new OutputStreamWriter(System.out);
            sbdoc.writeMML(wrt);
        }
    }
}

