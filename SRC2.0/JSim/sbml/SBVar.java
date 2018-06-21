/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// SBML variable for import

package JSim.sbml;

import JSim.util.*;
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.ArrayList;
import org.sbml.libsbml.*;

public class SBVar implements Named {
    protected SBModel sbmodel;
    protected String name;
    protected String mmlName; // safe name for MML
    protected String role; // species, parameter, compartment, rate
    protected String compartment; // for species
    protected String conversionFactor; //Also for species
    protected String unit;
	protected String notes;  // Any sbml notes associated with it.
    protected boolean isConst; // time invariant?
    protected boolean inDelay;
    protected boolean isPrivate; // private variable?
    protected boolean isBC; // is boundary condition?
    protected boolean isSubstanceOnly; //When constructing ODEs, must we convert to concentration?
    protected String initValue; // IC (entire if isConst) value
    protected boolean initNum; // is initValue numeric?
    protected String alg; // algebraic calculation
    protected String ode; // assigned ode calculation
    protected String deltas; // accumulated ode deltas
    protected boolean hasExternInit; //If this variable exists only to be an extern initial value for a parameter.
    protected ASTNode mInitValueNode;
    protected boolean isImplicit;

    // constructor
    public SBVar(SBModel sm, String n, String role)
        throws Xcept {
        sbmodel = sm;
        name = n;
        mmlName = safeMMLName(name);
        unit = null;
        this.role = role;
        isConst = false;
        inDelay = false;
        isPrivate = false;
        isSubstanceOnly = false;
        isImplicit = false;
        initValue = null;
        mInitValueNode = null;
        hasExternInit = false;
        sbmodel.sbvars.add(this);
		this.notes = new String("");
    }

    // create MML safe name (this routine good for most, not all)
    private String safeMMLName(String name) {
        String safe = name;
        if (! Character.isLetter(name.charAt(0)))
            safe = "xxx" + name;
        else if (name.length() > 1 
                 && name.substring(0,2).equalsIgnoreCase("js")) 
            safe = "" + name.charAt(0) + "_" + name.substring(1);
        else if (MMLUtil.reservedWords.contains(name))
            safe = name + "_";
        return safe;
    }

    // set properties
    public void setUnit(String u) {
        u = sbmodel.getSBUnitSpace().simplifyJSim(u);
        unit = u;
    }
    public void setInitValue(double d) { 
        initNum = true;
        initValue = Double.isNaN(d) ? 
            null : Util.pretty(d); 
    }
    public void setInitValue(String s) { 
        initNum = false;
        initValue = s; 
    }
    public void setInitValue(ASTNode node) throws Xcept { 
        initNum = false;
        String math = libsbml.writeMathMLToString(node);
        initValue = sbmodel.mathExprMML(math);
        mInitValueNode = node;
    }
    public void setConst(boolean b) {
        isConst = b; }
    public void setBC(boolean b) { isBC = b; }
    public void setIsSubstanceOnly(boolean b) {isSubstanceOnly = b; }

    // set calculations
    public void setImplicit() { isImplicit = true;}
    public void setAssign(String e) throws Xcept { 
        if (alg != null) throw new Xcept
            (this, "Multiple algebraic assigns not supported " + this);
        alg = e;
    }
    public void setODE(String e) throws Xcept { 
        if (ode != null) throw new Xcept
            (this, "Multiple ODE assigns not supported");
        ode = e;
    }
    public void addODEDelta(String s) {
        if (isBC) return; 
        deltas = (deltas == null) ? s : (deltas + " + " + s);
    }
    public void setPrivate(boolean b) { isPrivate = b; }

    public boolean compartmentVaries() {
        SBVar comp = sbmodel.getSBVar(compartment);
        if (comp == null) return false;
        return comp.hasKnownVariance();
    }

    //Do we know how the variable changes over time?  Used when writing out declarations and equations in MML.
    public boolean hasKnownVariance() {
        if (isImplicit) return true;
        if (alg != null) return true;
        if (ode != null) return true;
        if (deltas != null) return true;
        if (!isSubstanceOnly && compartmentVaries()) return true;
        return false;
    }

    //Does this variable have an initial value? (used when calculating implicit variables)
    public boolean hasInitValue() {
        return (initValue != null);
    }

    public boolean isExtern() {
        if (hasKnownVariance()) return false;
        if (initValue == null) return true;
        return false;
    }
    // is this variable solvable via ODE?
    public boolean isODE() {
        if (ode != null) return true;
        if (deltas != null) return true;
        return false;
    }
    
    // figure calc methods
    public void checkCalc() throws Xcept {
        if (isODE() && initValue == null) {
            hasExternInit=true;
            initValue = sbmodel.getUniqueInitValue(mmlName);
        }
        int ct = 0;
        String s = "";
        String amountname = mmlName;
        if (!isSubstanceOnly && compartment != null) {
            amountname = "(" + mmlName + "*" + compartment + ")";
        }
        if (alg != null) {
            ct++;
            s = s + " " + mmlName + "=" + alg + ";";
        }
        if (ode != null) {
            ct++;
            s = s + " " + amountname + ":time=" + ode + ";";
        }
        if (deltas != null) {
            ct++;
            s = s + " " + amountname + ":time=" + deltas + ";";
        }
        if (ct > 1) throw new Xcept
            (this, "Multiple calculation methods not supported:" + s);
        return;
    }

    // write rename warning
    public void writeRenameWarning(PrintWriter wrt) {
        if (name.equals(mmlName)) return;
        wrt.println("// WARNING: Renamed variable " + name +
                    " to " + mmlName + 
                    " due to JSim namespace restrictions");
    }

    // write MML variable declarations
    public void writeMMLDecl(PrintWriter wrt) {
        if (isPrivate && isExtern()) return;

        //Figure out the units (if any)
        String sunit = (unit == null || unit.equals("")) ? ";" : 
            (" " + sbmodel.mmlUnitName(unit) + ";");
        SBUnitSpace uc = sbmodel.getSBUnitSpace();
        String junit = uc.toJSimFromSBML(sunit);
        if (junit != null) sunit = junit;
        //If the initial value is an extern variable, we are the only thing that knows about it (there's no corresponding SVar) so we must declare it here.  We will also need to write out its role.
        if (hasExternInit) {
            wrt.println("  extern real " + initValue + sunit);
        }

        //Build up the declaration:
        String decl = new String("  ");
        if (isExtern()) {
            decl += "extern ";
        }
        if (isPrivate) {
            decl += "private ";
        }
        //Note:  when there are events, we will want 'realState' for some variables, not 'real'.  But for now:
        decl += "real " + mmlName;
        if (hasKnownVariance() || inDelay) {
            decl += "(time)";
        }
        else if (initNum) {
            decl += " = " + initValue;
        }
        decl += sunit;
		if(!this.notes.equals("")) { 
			decl =decl.concat( " "+this.notes);
			//this.notes = "";  // Do not use anymore.
		}
        wrt.println(decl);
    }
    
    // write var equations
    public void writeMMLEqn(PrintWriter wrt) {
        //Write the initial value if it exists, if we're not and if we're either an ODE or if the initial value is an equation.
        if (shouldWriteInitValue() ) {
            String eq = mmlName + " = ";
            //If we're an ODE or if we don't vary over time or if our equation has time-dependent variables in it, we must put a 'when time=time.min' conditional in front of the equation.
            if (hasKnownVariance() ||  hasTimeDependence(mInitValueNode)) {
                eq = "when (time=time.min) " + eq;
            }
            eq = "  " + eq + initValue + "; ";
			if(!this.notes.equals("")) { 
					eq = eq.concat(this.notes);
				}			
            wrt.println(eq);
        }

        //Now write the ODE or assignment rule
        if (isODE()) {
            String eq = ode;
            if (eq==null) {
                eq = deltas;
                if (conversionFactor != null) {
                    eq = conversionFactor + "*(" + deltas + ")";
                }
            }
            String amountname = mmlName;
            if (ode == null && !isSubstanceOnly && role.equals("species") && compartment != null) {
                amountname = "(" + mmlName + "*" + compartment + ")";
            }
			if(!this.notes.equals("")) { 
				 wrt.println("  " + amountname + ":time = " + eq + "; " +this.notes);
				}
			else { 	wrt.println("  " + amountname + ":time = " + eq + ";"); }

		}
        else if (alg != null) {
			if(!this.notes.equals("")) {
				wrt.println("  " + mmlName + " = " + alg + "; "+this.notes);
			}
			else { wrt.println("  " + mmlName + " = " + alg + "; "); }
	    }
        else if (!isSubstanceOnly && compartmentVaries()) {
			if(!this.notes.equals("")) {
				wrt.println("  (" + mmlName + "*" + compartment + "):time = 0; "+this.notes);
			}
			else { wrt.println("  (" + mmlName + "*" + compartment + "):time = 0;" ); }
        }
        else if (inDelay && !hasKnownVariance()) {
			if(!this.notes.equals("")) {
				wrt.println("  " + mmlName + ":time = 0; "+this.notes);
				//	this.notes = "";
			}
            else { wrt.println("  " + mmlName + ":time = 0; "); }

        }
    }

    //It's OK in SBML to overspecify a model's initial values, since there's an order of precedence.  This is not the case in JSim; we have to figure out whether to write stuff out or not.
    private boolean shouldWriteInitValue() {
        if (initValue == null) return false;
        if (alg != null) return false;
        if (isODE()) return true;
        if (isImplicit) return false;
        if (inDelay) return true;
        if (hasKnownVariance()) return true;
        return (!initNum);
    }

    //Check to see if any of the variables in this equation are time-dependent.  If so we'll need to prefix the initial value setting to 'when (time=time.min)'
    private boolean hasTimeDependence(ASTNode astn) {
        if (astn == null) return false;
        if (astn.getType() == libsbml.AST_NAME_TIME) return true;
        if (astn.getType() == libsbml.AST_NAME) {
            SBVar subvar = (SBVar) sbmodel.sbvars.getByName(astn.getName());
            if (subvar != null) {
                if (subvar.hasKnownVariance()) return true;
            }
        }
        if (astn.getType() == libsbml.AST_FUNCTION) {
            FunctionDefinition fd = sbmodel.model.getFunctionDefinition(astn.getName());
            //All function definitions are translated to assignment rules in JSim, meaning they are
            // defined as varying in time.
            if (fd != null) return true;
        }
        for (long c = 0; c < astn.getNumChildren() ; c++) {
            if (hasTimeDependence(astn.getChild(c))) return true;
        }
        return false;
    }
        
    // write MML property tags
    protected void writeMMLProp(PrintWriter wrt) {
        if (isPrivate) return;

        // If we have an inidial value that we made up and declared extern, we can write out its role here.
        if (hasExternInit) {
            wrt.println("  " + initValue + ".sbmlRole=\"derived\";");
        }
        // write properties
        if (role != null) 
            wrt.println("  " + mmlName + ".sbmlRole=\"" + 
                        role + "\";");
	if (! name.equals(mmlName))
	     wrt.println("  " + mmlName + ".sbmlName=\"" + 
                        name + "\";");
        if (compartment != null) 
            wrt.println("  " + mmlName + ".sbmlCompartment=\"" + 
                        compartment + "\";");
    }

    // query
    public String name() { return name; }
    public String toString() { return name; }
	public String notes() { return notes; }
	protected boolean setNotes(String newNotes) {
		if(this.notes.equals("")) {
			this.notes = new String(newNotes);
			return true;
		}
		else { // get rid of extra set of comment idents if more than one comment added:
			// ex: /* this note. */ /*The second note */
			this.notes = this.notes.replace("*/",",");
			this.notes = this.notes.concat(newNotes.replace("/*"," "));
			return true;
		}
	}
    public String diagInfo() { return "Var " + name(); }

    // SBVar.List
    public static class NList extends NamedList {
        public NList() { super(); }
        public SBVar sbvar(int i) {
            return (SBVar) get(i);
        }
        public SBVar sbvar(String n) throws Xcept {
            SBVar v = (SBVar) getByName(n);
            if (v == null) throw new Xcept
                ("Unknown SBML variable: " + n);
            return v;
        }
    }
}
