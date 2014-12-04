/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// ParSet section selection dialog

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GParSetSelect extends GNode implements ActionListener {
	private GParSetView gview;
	private PNamed.List otherParsets;
	private String[] onames;
	private JDialog dialog;
	private JComboBox ocombo;
	private JCheckBox[] jchecks;
	private GAction a_import, a_dismiss;

	private static final StringList SECTIONS = ParSet.SECTIONS;
	private static final int NSECTIONS = SECTIONS.size();
	private static final String[] STITLES = new String[] {
	    "Model input variables",
	    "Numeric solver settings",
	    "Function generator settings",
	    "Loops configuration",
	    "Sensitivity configuration",
	    "Optimization configuration",
	    "Memory configuation"
	};

	// constructor
	public GParSetSelect(GParSetView gview) {
	    super(gview, null);
	    this.gview = gview;
	}
	
	// show dialog, return true if update selected
	public void show() throws Xcept {
	    // tally importable parsets
	    otherParsets = project().children(ParSet.class);	    
	    otherParsets.remove(parset());
	    if (otherParsets.isEmpty()) throw new Xcept(
	    	"No other parameter sets to import from");
	    onames = new String[otherParsets.size()];
	    for (int i=0; i<onames.length; i++) 
	    	onames[i] = otherParsets.get(i).name();

	    // create/load dialog
	    if (dialog == null) createDialog();
	    ocombo.removeAllItems();
	    for (int i=0; i<onames.length; i++)
	    	ocombo.addItem(onames[i]);
	    ocombo.setSelectedIndex(0);
	    refreshCheckBoxes();
	    dialog.pack();
	        
	    // popup dialog
	    dialog.setLocationRelativeTo(parent().jcomp());
	    dialog.setVisible(true);
	}

	// create dialog
	private void createDialog() {
	    dialog = new JDialog(parent().frame(),
	    	"Import selected parameters from", true);
	    JPanel panel = new JPanel(new BorderLayout());
	    dialog.setContentPane(panel);

	    // other parset selection panel
	    JPanel opanel = new JPanel(new GridLayout(1, 2));
	    opanel.add(new JLabel("Import from"));
	    ocombo = new JComboBox();
	    ocombo.addActionListener(this);
	    opanel.add(ocombo);
	    panel.add(opanel, BorderLayout.NORTH);

	    // sections selection panel
	    JPanel gpanel = new JPanel(new GridLayout(NSECTIONS, 1));
	    jchecks = new JCheckBox[NSECTIONS];
	    for (int i=0; i<NSECTIONS; i++) {
	        jchecks[i] = new JCheckBox(
		    SECTIONS.get(i) + " (" + STITLES[i] + ")");
		jchecks[i].setHorizontalAlignment(SwingConstants.LEFT);
	        gpanel.add(jchecks[i]);
		jchecks[i].addActionListener(this);
	    }
	    panel.add(gpanel, BorderLayout.CENTER);

	    // import/dismiss buttons
	    a_import = new GAction(this, "Import") {
	    	public void doit() throws Xcept {
		    doImport();
		}
		public boolean sbEnabled() {
	    	    return ! getSelectedSections().isEmpty();
		}
	    };
	    
	    a_dismiss = new GAction(this, "Cancel") {
	    	public void doit() throws Xcept {
		    dialog.setVisible(false);
		}
	    };
	    
	    JPanel bpanel = new JPanel(new GridLayout(1, 2));
	    bpanel.add(new JButton(a_import));
	    bpanel.add(new JButton(a_dismiss));
	    panel.add(bpanel, BorderLayout.SOUTH);

	}

	// refresh checkboxes based on selected parset
	private void refreshCheckBoxes() {
	    ParSet oparset = getSelectedParSet();
	    StringList sections = parset().getSectionNames();
	    StringList osections = (oparset == null) ? 
	    	new StringList() : oparset.getSectionNames();
	    for (int i=0; i<NSECTIONS; i++) {
	    	boolean b = sections.containSame(SECTIONS.str(i));
	    	boolean ob = osections.containSame(SECTIONS.str(i));
		jchecks[i].setEnabled(ob);
		jchecks[i].setSelected(ob && !b);
	    }
	    refreshButtons();
	}

	// refresh import button enabled
	private void refreshButtons() {
	    a_import.setEnabled();
	}

	// do the import
	private void doImport() throws Xcept {
	    ParSet oparset = getSelectedParSet();
	    if (oparset == null) throw new Xcept(
	    	"No parset selected to import");
	    StringList sections = getSelectedSections();
	    if (sections.isEmpty()) throw new Xcept(
	    	"No parameter sections selected for import");
	    parset().importSections(oparset, sections);
 	    dialog.setVisible(false);
	    gview.refresh();
	}

	// action performed
	public void actionPerformed(ActionEvent event) {
	    if (event.getActionCommand().equals("comboBoxChanged"))
	    	refreshCheckBoxes();
	    else
	    	refreshButtons();
	}
	
	// get selected parset, or null
	private ParSet getSelectedParSet() {
	    int inx = ocombo.getSelectedIndex();
	    if (inx < 0 || inx >= otherParsets.size()) return null;
	    return (ParSet) otherParsets.get(inx);
	}

	// get selected sections
	private StringList getSelectedSections() {
	    StringList sections = new StringList();
	    for (int i=0; i<NSECTIONS; i++) 
	    	if (jchecks[i].isSelected())
		    sections.add(SECTIONS.str(i));
	    return sections;
	}

	// simple query
	public ParSet parset() { return gview.parset(); }
	public Project project() { return gproject().project(); }

}
