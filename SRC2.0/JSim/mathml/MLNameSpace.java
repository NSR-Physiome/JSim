/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Namespace for MathML process

package JSim.mathml;

import java.io.*;
import org.w3c.dom.Element; 
import JSim.util.*;
import JSim.expr.*;

public interface MLNameSpace extends NameSpace {

	// comp by element
	public String compNameByElement(Element e) throws Xcept;
}
