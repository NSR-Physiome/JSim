/*NSRCOPYRIGHT
  Copyright (C) 1999-2018 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/


// Import SBML notes element as JSim comment 

package JSim.sbml; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*; 

import java.io.*;
import java.util.*;
import org.w3c.dom.*;

import org.sbml.libsbml.*;

public class SBNotes {

	private String note;

	public static final String XHTML_TAG = new String("<p xmlns=\"http://www.w3.org/1999/xhtml\">");
	public static final String END_P_TAG = new String("</p>");
	public static final String NOTE_TAG = new String("<notes>");
	public static final String END_NOTE_TAG = new String("</notes>");

	public SBNotes(String sbmlNote) throws Xcept {
		note = new String(sbmlNote);
	
	}

	public SBNotes(XMLNode sbmlNote) throws Xcept {
		note = new String(sbmlNote.toXMLString());
	
	}

	public void removeXMLTags() {
		note = note.replace(NOTE_TAG, "");
		note = note.replace(END_NOTE_TAG, "");
		note = note.replace(XHTML_TAG,"");
		note = note.replace(END_P_TAG,"");
		note = note.trim();
	}

	public void addCommentIdentifiers() {

		String newString = new String("/* ");
		newString = newString.concat(note);
		note = newString.concat(" */") ;
	}

	public String getNote() { return note; }

}

