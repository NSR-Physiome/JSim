/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// input variable, dynamics popup func generators

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.gui.*;
import JSim.gui.model.*;

public class GRTInputVar {
	private GRTVar gvar;  // attached to this
	private ASVar asvar;  // model variable, if appropriate
	private GControl gcntl; // GControl attached
	private Box box; // for dynamic only
	private StringList existNames; // existing func gens
	private StringList assignNames; // parsed from cntl()
	private JDialog whichDialog; // dialog for popupWhichDialog
	
	// constructor
	public GRTInputVar(GRTVar gv, ASQuery expr) {
	    gvar = gv;
	    asvar = (expr instanceof ASVar) ?
		((ASVar) expr) : null;
	    gcntl = GControl.create(gvar, gvar.cntl());
	    if (gcntl instanceof GMenuControl && asvar != null)
		((GMenuControl) gcntl).setLabelValues(
		    asvar.labelValues());
	    if (asvar == null || asvar.ndim() == 0) return;

	    // Box with func gen button
	    box = new Box(BoxLayout.X_AXIS);
	    box.add(gcntl.jcomp());
	    GAction act = new InitPress();
	    JButton b = act.button();
	    b.setText(null);
	    b.setIcon(glook().funcgenIcon());
	    int wh = (int) (glook().fontSize()*1.5);
	    b.setPreferredSize(new Dimension(wh, wh));
	    box.add(b);
	}

	// load func gen names
	public void loadNames() {

	    // all existing fgen
	    PModelVars vars = gmodel().pmodel().vars();
	    PNamed.List fgens = vars.children(FuncGen.class);
	    existNames = new StringList(4);
	    for (int i=0; i<fgens.size(); i++) 
		existNames.add(fgens.pnamed(i).name());
	    
	    // all currently assigned fgen
	    StringTokenizer stok = new StringTokenizer(
		cntl().stringVal(), " \t()+-*/^");
	    assignNames = new StringList(4);
	    while (stok.hasMoreTokens()) {
		String tok = stok.nextToken();
		if (existNames.containSame(tok))
		    assignNames.addUniq(tok);
	    }
	}

	// popup "which fgen" dialog
	private void popupWhichDialog() throws Xcept {

	    // create labels/buttons
	    JComponent[] jcomp = new JComponent[existNames.size()+2];
	    int i=0;
	    jcomp[i++] = new JLabel(
		"Assign function generator to " + 
		cntl().name() + "?");
	    jcomp[i++] = new JButton(new AssignFgen(null));
	    for (int j=0; j<existNames.size(); j++) 
		jcomp[i++] = new JButton(
		    new AssignFgen(existNames.str(j)));

	    // create/show dialog
	    JOptionPane pane = new JOptionPane(
		jcomp, JOptionPane.PLAIN_MESSAGE);
	    pane.setOptions(new String[] { "Cancel" });
	    whichDialog = pane.createDialog(jvalue(),
		"Input variable " + cntl().name());
	    setLocation(whichDialog);
	    whichDialog.setVisible(true);
	}

	// popup creation dialog
	private void popupCreateDialog() throws Xcept {
	    GModelPars gpars = gmodel().gpars();
	    PModelVars vars = gmodel().pmodel().vars();
	    String msg = "Enter name for new function generator";
	    String n = vars.newChildName("fgen", true);
	    JTextField jname = new JTextField(n);
	    jname.select(0, n.length());
	    JComponent[] jcomp = new JComponent[2];
	    jcomp[0] = new JLabel(
		"Enter name for new function generator");
	    jcomp[1] = jname;
	    JOptionPane pane = new JOptionPane(
		jcomp, 
	    	JOptionPane.QUESTION_MESSAGE, 
		JOptionPane.OK_CANCEL_OPTION);
	    JDialog dialog = pane.createDialog(jvalue(),
		"Input variable " + cntl().name());
	    setLocation(dialog);
	    dialog.setVisible(true);
	    Object opt = pane.getValue();
	    if (! (opt instanceof Integer)) return;
	    if (((Integer) opt).intValue() != JOptionPane.OK_OPTION)
		return;
	    String nname = jname.getText();
	    if (Util.isBlank(nname)) return;
	    FuncGen f = new FuncGen(vars, nname);
	    f.domain(0).setVal(asvar.domain(0).name());
	    cntl().setVal(nname);
	    gpars.connectRT();
	    gproject().project().revalidate();
	    gproject().refresh();
	    setPage(nname);
	}

	// show FuncGen page
	private void setPage(String n) {
	    GModelPars gpars = gmodel().gpars();
	    gpars.setPage("FuncGen " + n);
	}

	// set dialog location just below input widget
	public void setLocation(JDialog dialog) {
	    Point pt = GUtil.getLocationOnScreen(jvalue());
	    Dimension sz = jvalue().getSize();
	    pt.y += sz.height;
	    dialog.setLocation(pt);
	}

	// query
	public GControl gcntl() { return gcntl; }
	public JComponent jvalue() {
	    return (asvar != null && asvar.ndim()>0) ? 
		box : gcntl.jcomp();
	}
	public Control cntl() { return gcntl.cntl(); }
	public GLook glook() { return gvar.glook(); }
	public GProject gproject() { return gvar.gproject(); }
	public GModel gmodel() { return gvar.gmodel(); }

	// initial button press
	public class InitPress extends GAction {
	    public InitPress() { super(gvar, "initPress"); }

	    // action initiated on button press
	    public void doit() throws Xcept {
		loadNames();
		if (assignNames.size() > 0) {
		    setPage(assignNames.str(0));
		    return;
		}
		if (! gvar.editable()) throw new Xcept(
		    "You may not assign function generators at this time.");
		if (existNames.size() == 0)
		    popupCreateDialog();
		else
		    popupWhichDialog();
	    }

	}

	// assign func gen action
	public class AssignFgen extends GAction {
	    String fgen;
	    public AssignFgen(String f) { 
		super(gvar, 
		    (f == null) ?
		    "Create new function generator..." :
		    "Use existing " + f);
	        fgen = f;
	    }
	    public void doit() throws Xcept {
		if (whichDialog != null)
		    whichDialog.setVisible(false);
		whichDialog = null;
		if (fgen == null)
		    popupCreateDialog();
		else { 
		    cntl().setVal(fgen);
		    gvar.refresh();
		    setPage(fgen);
		}
	    }
	}
}

