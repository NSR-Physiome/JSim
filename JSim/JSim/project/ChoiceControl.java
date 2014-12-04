/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Choice control parameter for project

package JSim.project;

import JSim.util.*;

import java.util.*;

public class ChoiceControl extends IntControl {
	private String[] labels; // labels
	private Hashtable<String, String> legacyLabels;

	// constructor
	public ChoiceControl(PNamed p, String n, int d, String[] l) 
	throws Xcept {
	    super(p, n, d);
	    value = defaultValue = d;
	    labels = l;
	    if (labels.length < 1) throw new Xcept(this,
		"ChoiceControl must have at least 1 label");
	}

	// specific methods
	public int nLabels() { return labels.length; }
	public String stringVal() { return labels[value]; }
	public String stringVal(int i) { return labels[i]; }
	public String stringDef() { return labels[defaultValue]; }
	public void setVal(int i) throws Xcept {
	    if (i<0 || i>=labels.length) throw new Xcept(this,
		"Choice value set out of range");
	    value = i;
	    update();
	}
	public void setVal(String s) throws Xcept { 
	    if (legacyLabels != null && legacyLabels.get(s) != null)
	    	s = legacyLabels.get(s);
	    for (int i=0; i<labels.length; i++) {
	    	if (s.equals(labels[i])) {
		    value = i;
		    update();
		    return;
		}
	    }
	    throw new Xcept(this, "Invalid value \"" +
		s + "\"");
	}

	// add legacy label 
	protected void addLegacyLabel(String oldLabel, String newLabel) {
	    if (legacyLabels == null)
	    	legacyLabels = new  Hashtable<String, String>();
	    legacyLabels.put(oldLabel, newLabel);
	}
}

