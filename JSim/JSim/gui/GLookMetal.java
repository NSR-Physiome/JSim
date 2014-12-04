/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim Metal-theme customizations

package JSim.gui;

import java.awt.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.plaf.metal.*;
import java.net.*;

import JSim.util.*;

public class GLookMetal extends DefaultMetalTheme {
	private GLook glook;
	private final ColorUIResource pri1, pri2, pri3, 
	    sec1, sec2, sec3;

	// constructor
	public GLookMetal(GLook glook) {
	    super();
	    this.glook = glook;
	    pri1 = glook.deep();
	    pri2 = glook.bg_lighter();
	    pri3 = glook.bright(); // icons in file select, help popups
	    sec1 = new ColorUIResource(Color.black);
	    sec2 = glook.bg_darker();
	    sec3 = glook.bg();
	    MetalLookAndFeel.setCurrentTheme(this);
	}

	public String getName() { return "JSimMetal"; }

	public ColorUIResource getPrimary1() { return pri1; }
	public ColorUIResource getPrimary2() { return pri2; }
	public ColorUIResource getPrimary3() { return pri3; }
	public ColorUIResource getSecondary1() { return sec1; }
	public ColorUIResource getSecondary2() { return sec2; }
	public ColorUIResource getSecondary3() { return sec3; }
	public ColorUIResource getMenuDisabledForeground() {
	    return glook.disabledColor();
	}
	public ColorUIResource getInactiveSystemTextColor() {
	    return glook.disabledColor();
	}
	public ColorUIResource disabledColor() { return glook.disabledColor(); }

	public FontUIResource getWindowTitleFont() { return glook.bigFont(); }
	public FontUIResource getMenuTextFont() { return glook.bigFont(); }
	public FontUIResource getControlTextFont() { return glook.baseFont(); }
	public FontUIResource getUserTextFont() { return glook.textFont(); }
	public FontUIResource getSubTextFont() { return glook.baseFont(); }
	public FontUIResource getSystemTextFont() { return glook.baseFont(); }
}

