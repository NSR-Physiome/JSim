/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MathMl <math> tag processing

package JSim.mathml;

import java.io.*;
import java.util.ArrayList;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.Node; 
import org.w3c.dom.NodeList; 
import org.w3c.dom.CharacterData; 
import org.w3c.dom.Text; 
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class MLMath implements DiagInfo {
	private Element math; // <math> element
	private static MLApply apply;

	// local query info (s/b separate query class for reentracy)
	private MLNameSpace nameSpace;
	private MLCSymbol.NList csymbols;	

	// constructors
	public MLMath(Element e) throws Xcept {
	    math = e;
	    if (apply == null) apply = new MLApply();
	}
	public MLMath(String text) throws Xcept {
	    Document doc = UtilXML.parse(text);
	    math = doc.getDocumentElement();
	    if (apply == null) apply = new MLApply();
	}

	// get Vars
	public StringList getVars() {
	    StringList list = new StringList();
	    addVars(list, math);
	    return list;
	}
	
	// add Vars to StringList
	private void addVars(StringList list, Element elem) {
	    String tag = elem.getNodeName();
	    if (tag.equals("ci")) 
	    	list.addUniq(UtilXML.getText(elem));
	    NodeList nodes = elem.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
		Node node = nodes.item(i);
		if (! (node instanceof Element)) continue;
		addVars(list, (Element) node);
	    }
	}	    

	// make ExprList for this expr
	public Expr.List makeExprList(MLNameSpace ns, 
	MLCSymbol.NList csyms) throws Xcept {
	    nameSpace = ns;
	    csymbols = csyms;
	    return makeExprList(math);
	}

	// make Expr.List from DOM Element
	private Expr.List makeExprList(Element elem) throws Xcept {
	    Expr.List list = new Expr.List(4);
	    NodeList nodes = elem.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
		Node node = nodes.item(i);
		if (! (node instanceof Element)) continue;
		String n = node.getNodeName();
		if (n.equals("rdf:RDF")) continue; // metadata only
		Expr expr = makeExpr((Element) node);
	        list.add(expr);
	    }
	    return list;
	}

	// make Expr from DOM Element
	private Expr makeExpr(Element elem) throws Xcept {
	    String name = elem.getNodeName();
	    if (name.equals("apply")) 
            return makeApplyExpr(elem);
        if (name.equals("cn"))
            return makeConstExpr(elem);
	    if (name.equals("ci"))
            return makeVarExpr(elem);
	    if (name.equals("bvar"))
            return makeBVarExpr(elem);
	    if (name.equals("piecewise"))
            return makePiecewiseExpr(elem);
	    if (name.equals("degree"))
	    	return makeSubExpr(name, elem);
	    if (name.equals("logbase"))
	    	return makeSubExpr(name, elem);
	    if (name.equals("pi"))
	        return Expr.pi;
	    if (name.equals("exponentiale"))
	        return Expr.exponentiale;
	    if (name.equals("true"))
	        return Expr.truex;
	    if (name.equals("false"))
	        return Expr.falsex;
        if (name.equals("csymbol")) 
            return makeCSymbol(elem, new Expr.List(0));
	    throw new Xcept( 
		"Tag <" + name + "> not supported within <math>");
	}

	// make apply expression
	private Expr makeApplyExpr(Element elem) throws Xcept {

	    // separate sub-Elements into op and args
	    NodeList nodes = elem.getChildNodes();
	    Element eop = null;
	    Expr.List args = new Expr.List(4);
	    for (int i=0; i<nodes.getLength(); i++) {
            Node node = nodes.item(i);
            if (! (node instanceof Element)) continue;
            if (eop == null) {
                eop = (Element) node;
            } else {
                Expr arg = makeExpr((Element) node);
                args.add(arg);
            }
	    }
	    if (eop == null) throw new Xcept("<apply> tag missing its operator");
	    String opname = eop.getNodeName();

	    // ci single variable or function
	    if (opname.equals("ci")) {
	        if (args.size() == 0) {
                return makeVarExpr(eop);
            } else {
                String fname = UtilXML.getText(eop);
                Expr fex = nameSpace.funcCall(fname, args);
	    	    if (fex == null) throw new Xcept(this,"Function " + fname + " not defined.");
	    	    return fex;
            }
	    }

	    // csymbols use special lookup
	    if (opname.equals("csymbol")) 
	    	return makeCSymbol(eop, args);

	    // find appropriate MLApply.Op
	    MLApply.Op op = apply.op(opname);
	    if (op != null) 
	        return op.makeExpr(args);

	    throw new Xcept("Can't process <apply> operator " +
	    	opname);
	}

	// make csymbol external definition
	private Expr makeCSymbol(Element elem, Expr.List args) 
	throws Xcept {
	    String name = nameSpace.compNameByElement(elem);
	    MLCSymbol csym = (csymbols == null) ? 
	        null : csymbols.csym(name);
	    if (csym == null) throw new Xcept(this,
	    	"Unsupported csymbol: " + name); 
	    return csym.makeExpr(args);
	}	

	// degree/logbase:  just return sub expr
	private Expr makeSubExpr(String tag, Element elem) throws Xcept {
	    NodeList nodes = elem.getChildNodes();
	    Expr ret = null;
	    for (int i=0; i<nodes.getLength(); i++) {
		Node node = nodes.item(i);
		if (! (node instanceof Element)) continue;
		if (ret != null) throw new Xcept(
		    "<" + tag + "> requires single argument");
		ret = makeExpr((Element) node);
	    }
	    if (ret == null) throw new Xcept(
		"<" + tag + "> requires single argument.");
	    return ret;
	}

	// make piecewise expression
	private Expr makePiecewiseExpr(Element elem) throws Xcept {

	    // separate sub-Elements into pieces and other
	    Expr.List pieces0 = new Expr.List(4);
	    Expr.List pieces1 = new Expr.List(4);
	    Expr other = null;
	    NodeList nodes = elem.getChildNodes();
	    for (int i=0; i<nodes.getLength(); i++) {
            Node node = nodes.item(i);
            if (! (node instanceof Element)) continue;
            String name = node.getNodeName();
            if (name.equals("piece")) {
                Expr.List piece = makeExprList((Element) node);
                if (piece.size() != 2) {
                    throw new Xcept(this,
                                    "<piece> requires 2 components, " +
                                    piece.size() + " found.");
                }
                pieces0.add(piece.expr(0));
                pieces1.add(piece.expr(1));
            } else if (name.equals("otherwise")) {
                if (other != null) {
                    throw new Xcept(this,
                                    "Duplicate <otherwise> tag.");
                }
                Expr.List list = makeExprList((Element) node);
                if (list.size() != 1) {
                    throw new Xcept(this,
                                    "<otherwise> requires 2 components, " +
                                    list.size() + " found.");
                }
                other = list.expr(0);
            }
	    }
	    if (other == null) {
            //In most cases, the reason no 'otherwise' was set is that all the
            // cases were accounted for in the explicit boolean checks.
            // i.e. 'if x>0, 5; if x<=0, 6'.  Our options would be to delete 'x<=0'
            // or, as we do here, simply add a final expression.
            other = Expr.zero;
        }
        
	    // build nested if expr
	    Expr expr = other;
	    int ct = pieces0.size();
	    for (int i=0; i<ct; i++) {
            Expr test = pieces1.expr(ct-1-i);
            Expr val = pieces0.expr(ct-1-i);
            expr = new IfExpr(test, val, expr);
	    }
	    return expr;
	}

	// make variable expression
	private Expr makeBVarExpr(Element elem) throws Xcept {
	    NodeList nodes = elem.getElementsByTagName("ci");
	    if (nodes.getLength() != 1) throw new Xcept(
		"<bvar> tag supports only a single <ci> child");
	    return makeVarExpr((Element) nodes.item(0));
	}

	// make variable expression
	private Expr makeVarExpr(Element elem) throws Xcept {
	    String name = UtilXML.getText(elem);
	    if (Util.isBlank(name)) throw new Xcept(
		"Name text missing in <ci> tag.");
	    Expr e = nameSpace.compByName(name);
	    if (e == null) throw new Xcept(
	    	"Can't find variable " + name);
	    return e;
	}

	// make numeric constant expression
	private Expr makeConstExpr(Element elem) throws Xcept {
	    String type = elem.getAttribute("type");
        if (type.equals("rational")) {
            return getRational(elem);
        }
        else {
            double f;
            if (type.equals("e-notation"))
                f = getValueEnot(elem);
            else {
                f = UtilXML.getValueText(elem);
            }
            String s = elem.getAttribute("cellml:units");
            if (Util.isBlank(s)) return Expr.cons(f);
            Unit u = nameSpace.unitByName(s);
            return new RealConst(f, u);
        }
	}

    private Expr getRational(Element elem) throws Xcept {
        NodeList children = elem.getChildNodes();
        StringList texts = new StringList();
        for (int n=0; n<children.getLength(); n++) {
            Node child = children.item(n);
            if (child instanceof Text) {
                texts.add(((Text) child).getWholeText());
            }
        }
	    if (texts.size() != 2) throw new Xcept(
	    	"Illegal e-notation constant: " + texts);
        ConstExpr numerator = Expr.cons(Util.toInt(texts.str(0).trim()));
        ConstExpr denominator = Expr.cons(Util.toInt(texts.str(1).trim()));
        return numerator.div(denominator);
    }
	// get e-notation numeric value
	private double getValueEnot(Element elem) throws Xcept {
	    StringList texts = new StringList();
	    NodeList childs = elem.getChildNodes();
	    for (int i=0; i<childs.getLength(); i++) {
	    	Node child = childs.item(i);
		if (child instanceof Text)
		    texts.add(((Text) child).getWholeText());
	    }
	    if (texts.size() != 2) throw new Xcept(
	    	"Illegal e-notation constant: " + texts);
	    double base = Util.toDouble(texts.str(0));
	    double exp = Util.toDouble(texts.str(1));
	    double f = base * Math.pow(10, exp);
	    if (Double.isNaN(f)) throw new Xcept(this,
	        "Illegal e-notation constant: " + texts);
	    return f;
	}

	// make lambda for this math
	public MLLambda makeLambda(MLNameSpace ns, 
	MLCSymbol.NList csyms) throws Xcept {
	    MLLambda lambda = new MLLambda(ns);
	    Element elambda = UtilXML.getUniqueElement(math, "lambda");
	    ArrayList<Element> elems = UtilXML.getElements(elambda);
	    if (elems.size() < 1) throw new Xcept(
	    	"No elements within <lambda>");

	    // set arguments
	    for (int i=0; i<elems.size()-1; i++) {
	    	Element earg = elems.get(i);
		if (! earg.getNodeName().equals("bvar")) throw new Xcept(
		    "<lambda> element expected <bvar> child, got "
		    + earg.getNodeName());
		Element eci = UtilXML.getUniqueElement(earg, "ci");
		String argName = UtilXML.getText(eci);
		lambda.addArg(argName);
 	    }

	    // set eval
	    nameSpace = lambda;
	    csymbols = csyms;
	    Element eeval = elems.get(elems.size()-1);
	    Expr eval = makeExpr(eeval);
	    lambda.setEval(eval);

	    // done
	    return lambda;
	}

	// query
	public String toString() { 
	    return "MathML for " + nameSpace; 
	}
	public String diagInfo() { return toString(); }
	protected MLNameSpace nameSpace() { return nameSpace; } // for Lambda
}
