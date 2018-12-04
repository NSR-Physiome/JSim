/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// listener for data update

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

interface GDataUpdateListener {

	void updatedCSVData(String data, String title) throws Xcept; // returns updated data in CSV form 

}
