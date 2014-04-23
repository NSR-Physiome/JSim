/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test linking to Desktop WWW browser 

package JSim.tests;

import java.net.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

import edu.stanford.ejalbert.*;

//import java.awt.Desktop.Action;

//import javax.jnlp.*;
//import java

//import JSim.util.*;

public class DesktopTest implements MouseListener  {
	private URL url;

	// constructor
	public DesktopTest(String text) throws Exception {
	    JFrame frame = new JFrame("BrowserTest");
	    JPanel panel = new JPanel();
	    JLabel l = new JLabel(text);
	    panel.add(l);
	    panel.addMouseListener(this);
	    frame.setContentPane(panel);
	    frame.setSize(300, 300);
	    frame.setVisible(true);
	    url = new URL(text);
	}
	
	
	// Mouse listener
	public void mouseClicked(MouseEvent e) { 
	    System.err.println("Loading: " + url);
	    try {
//	    	showDocumentAWT();
		showDocumentBL();
	    } catch (Exception x) {
	    	System.err.println("" + x);
	    }
		
	}
  	public void mouseEntered(MouseEvent e) { }
  	public void mouseExited(MouseEvent e) { }
  	public void mousePressed(MouseEvent e) { }
 	public void mouseReleased(MouseEvent e) { }

	// show Document via awt.Desktop
	private void showDocumentAWT() throws Exception {
	    System.err.println("Desktop support=" + 
	    	Desktop.isDesktopSupported());
	    Desktop desktop = Desktop.getDesktop();
	    System.err.println("Desktop open support=" +
	        desktop.isSupported(Desktop.Action.BROWSE));
	    URI uri = url.toURI();
System.err.println("URI=" + uri);
	    desktop.browse(uri);
	}

	// show Document via BrowserLauncher2
	private void showDocumentBL() throws Exception {
System.err.println("new BrowserLauncher");
	    BrowserLauncher bl = new BrowserLauncher();
System.err.println("URL=" + url);
	    bl.openURLinBrowser(url.toString());
	}

	// show Document via javax.jnlp
/*	private void showDocumentJNLP() throws Exception {
	    BasicService bs;
	       = (BasicService)ServiceManager.lookup("javax.jnlp.BasicService"); 
	    bs.showDocument(url);
	}
*/	
	// mainline
	public static void main(String[] args) throws Exception {
	   new DesktopTest(args[0]);
	}
}
