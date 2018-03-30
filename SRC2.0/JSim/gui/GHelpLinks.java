/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim built-in connection to on-line help via WWW browser

package JSim.gui;

import JSim.util.*;
import java.net.*;
import java.awt.*;
import java.applet.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import org.w3c.dom.*;

import edu.stanford.ejalbert.*; // in BrowserLauncher2.jar

public class GHelpLinks {
	public static final String HTTP = "http://";
	// 	Help pages may use https, redirects confuse the xml parser (SAX)??:
	public static final String HTTPS = "https://";
	public static final String PHYS = "www.physiome.org";
	public static final String PHYSIOMEHTTP = HTTP + PHYS;
    public static final String JSIMLINKS = "/jsim/help/helplinks.php";
	private String PHYSIOME = HTTP + PHYS;
	private String HELPLINKS = PHYSIOME + JSIMLINKS;

//	alt HELPLINKS consts to test failure & slow response
//	public String HELPLINKS = PHYSIOME + "/jsim/help/nonexistent.php";
//	public String HELPLINKS = PHYSIOME + "/jsim/help/slow.php";

	// control asynch loading of helplinks doc
	private Thread loadThread, monitorThread; // load doc & monitor thread
	private static final long MAX_LOAD_TIME = 10000; // msecs to allow for load

	// internal state
	private GMain gmain;
	private Document doc; // help-links document
	private String baseURL = PHYSIOME;
	private Hashtable<String, Element> nodeMenus;

	// alternatives for browser launch
	private Desktop desktop; // try this 1st (java.awt.Desktop)
	private BrowserLauncher browserLauncher; // try 2nd (edu.stanford.ejalbert.BrowserLauncher)

	// constructor
	public GHelpLinks(GMain gmain)  {
	    this.gmain = gmain;
	    try {
	    	loadThread = new LoadThread();
		loadThread.start();
		monitorThread = new MonitorThread();
		monitorThread.start();
	    } catch (Exception e) {
	    	e.printStackTrace();
	    }
	}

	// load doc thread
	public class LoadThread extends Thread {
	    public LoadThread() { super("HelpLinksLoadThread"); }
	    public void run() {
	    	try {
			// Check if help website using http or redirected to https:
			URL urlObj = new URL(PHYSIOMEHTTP);
			boolean useHTTPS = isHTTPS(urlObj);
			if(useHTTPS) {
				PHYSIOME = HTTPS + PHYS;
				HELPLINKS = PHYSIOME + JSIMLINKS;
			}
			//System.out.println("Physiome url: ... " + PHYSIOME);

		    loadDoc();
		} catch (Exception e) {
	    	    System.err.println("Warning: Failure loading helplinks.php");
		}
	    }
	};    

	// monitor load doc thread
	public class MonitorThread extends Thread {
	    public MonitorThread()  { super("HelpLinksMonitorThread"); }
	    public void run() {
	        try {
		    loadThread.join(MAX_LOAD_TIME); // wait decent interval
		    if (loadThread.isAlive()) {
			Util.verbose("Killing helpLinks load thread ...");
		    	loadThread.stop(); // kill load thread
		    } else {
		    	Util.verbose("GHelplinks: load thread terminated normally");
		    }
		    loadThread = null;
		} catch (Exception e) {
		    System.err.println("" + e);
		}
	    }
	}

	// load helplinks document
	private void loadDoc() throws Exception {
	    String urlText = HELPLINKS 
	    	+ "?version=" + Util.version() 
		+ "&os=" + Util.jsimOSName(); 
	    URL url = new URL(urlText);
	    String text = UtilIO.readText(url);
	    doc = UtilXML.parse(text);
	    Element root = doc.getDocumentElement();

	    // load baseURL
	    NodeList nodes = root.getElementsByTagName("default");
	    for (int i=0; i<nodes.getLength(); i++) {
	    	Element e = (Element) nodes.item(i);
		String s = e.getAttribute("baseURL");
		baseURL = s;
	    }

	    // load nodeMenus
	    nodeMenus = new Hashtable<String, Element>();
	    nodes = root.getElementsByTagName("menu");
	    for (int i=0; i<nodes.getLength(); i++) {
	    	Element e = (Element) nodes.item(i);
	    	String gnode = e.getAttribute("gnode");
		if (Util.isBlank(gnode)) continue;
		nodeMenus.put(gnode, e);
	    }
	}

	private boolean isHTTPS(URL urlCheck) throws Exception {
		URL obj = new URL(PHYSIOME);
		HttpURLConnection conn = (HttpURLConnection) obj.openConnection();
		conn.setReadTimeout(5000);
		conn.addRequestProperty("Accept-Language", "en-US,en;q=0.8");
		conn.addRequestProperty("User-Agent", "Mozilla");
		conn.addRequestProperty("Referer", "google.com");

		//System.out.println("Request URL ... " + obj);
		boolean redirect = false;

		// normally, code 3xx is redirect
		int status = conn.getResponseCode();
		if (status != HttpURLConnection.HTTP_OK) {
			if (status == HttpURLConnection.HTTP_MOVED_TEMP
				|| status == HttpURLConnection.HTTP_MOVED_PERM // Code 301
					|| status == HttpURLConnection.HTTP_SEE_OTHER) // code 303
			redirect = true;
		}

		//System.out.println("Response Code ... " + status);
		conn.disconnect();
		return redirect;
	}


	// add gnode help menu to menubar
	public void addHelpMenu(GNode gnode, JMenuBar mbar) {
	    try {
		HelpMenu menu = new HelpMenu(gnode);
		mbar.add(menu);
		loadHelpMenu(menu);
	    } catch (Exception e) {
	        e.printStackTrace();
	    }    
	}

	// load menu contents (WWW + context help)
	public void loadHelpMenu(HelpMenu menu) {

	    // add WWW links
	    String gname = menu.gnode.getClass().getName();
	    int inx = gname.lastIndexOf(".");
	    if (inx >= 0) gname = gname.substring(inx+1);
	    if (nodeMenus != null) {
	    	loadHelpMenuWWW(menu.gnode, nodeMenus.get(gname), menu);
	    } else { 
	    	addTextMenuItem(menu, 
		    "WWW Documentation is not currently reachable.");
	    }
	    menu.isFinal = nodeMenus != null || loadThread == null; 
	    
	    // add context-sensitive help info
	    GAction a = new GAction(menu.gnode, "Context-sensitive help ...") {
	    	public void doit() throws Xcept {
		    showContextHelp(gnode);
		}
	    };
	    menu.add(a.item());
	}

	// load dynamic WWW links portion of Help menu
	private void loadHelpMenuWWW(GNode gnode, Element emenu, 
	HelpMenu menu) {
	    if (emenu == null) return;
	    boolean isEmpty = true;
	    NodeList items = emenu.getElementsByTagName("item");
	    for (int i=0; i<items.getLength(); i++) {
	    	Element eitem = (Element) items.item(i);
		String text = eitem.getAttribute("text");
		String url = eitem.getAttribute("url");
		if (Util.isBlank(text)) continue;
		if (isEmpty) 
		    addTextMenuItem(menu, "WWW Documentation Links");
		if (! Util.isBlank(url) && !url.startsWith(HTTP)) 
		    url = baseURL + url;
		if (Util.isBlank(url)) {
		    addTextMenuItem(menu, text);
		} else {
		    GAction a = new HelpAction(
		    	gnode, text + " ...", url);
		    menu.add(a.item());
		}
		isEmpty = false;
	    }
	    if (! isEmpty)
	    	menu.addSeparator();
	}

	// add text item to menu
	private void addTextMenuItem(JMenu menu, String text) {
	    JMenuItem item = new JMenuItem(text);
	    item.setEnabled(false);
	    menu.add(item);
	}

	// HelpMenu class
	public class HelpMenu extends GMenu  {
	    public GNode gnode;
	    public boolean isFinal;
	    public HelpMenu(GNode gnode) { 
	    	super("Help", gnode);
		this.gnode = gnode;
	    }

	    // menu selected (clicked on before popup)
	    // reload items & cleanup monitor thread as needed
	    public void menuSelected(MenuEvent e) {
	    	if (! isFinal && nodeMenus != null) {
		    removeAll();
		    loadHelpMenu(this);
		}
		super.menuSelected(e);
		if (loadThread == null && monitorThread != null) 
		    joinMonitorThread();
	    }
	}

	// join (finalize) monitor thread
	private void joinMonitorThread() {
	    try {
		Util.verbose("GHelpLinks: Joining monitor thread ...");
		monitorThread.join();
		monitorThread = null;
	    } catch (Exception e) {
	        e.printStackTrace();
	    }    
	}

	// context help popup
	private void showContextHelp(GNode gnode) {
	    String msg = "Programmer help edit mode is currently active.";
	    switch (gnode.ghelp().permMode()) {
	    case GHelp.HIDE: 
	    	msg = "<html>Context-sensitive help is currently disabled.<br />" +
		"Type F1 to enable hover help,<br />" +
		"Type Shift-F1 to enable mouse-over help.</html>";
		break;
	    case GHelp.HOVER: 
		msg = "<html>Hover help is currently enabled.<br />" + 
		"Type F1 or Shift-F1 to disable help.</html>";
		break;
	    case GHelp.FAST: 
		msg = "<html>Mouse-over help is currently enabled.<br />" + 
		"Type F1 or Shift-F1 to disable help.</html>";
		break;
	    }
	    JOptionPane.showMessageDialog(gnode.gproject().jcomp(),
		msg, "Context-Sensitive Help",
		JOptionPane.INFORMATION_MESSAGE);
	}


	// HelpAction
	public class HelpAction extends GAction {
	    public String url;
	    public HelpAction(GNode g, String na, String url) {
	    	super(g, na);
		this.url = url;
	    }
	    public void doit() throws Xcept {
	    	String msg = "Linking browser to: " + url;
		GProject gproj = gnode.gproject();
		gproj.message(msg);
		try {
		    if (connectDesktop()) return;
		} catch (Exception e) {
		    // message?
		}
		try {
		    if (connectBrowserLauncher()) return;
		} catch (Exception e) {
		    // message?
		}
		connectError();
	    }

	    // show link via Applet showDocument()
	    // this isn't used because help doc replaces applet 
	    //   window,  thus closing the applet
	    public boolean connectApplet() throws Exception {
		AppletContext ctxt = gnode.gmain().getAppletContext();
		if (ctxt == null) return false;
System.err.println("using AppletContext");
		URL u = new URL(url);
		ctxt.showDocument(u);
		return true;
	    }

	    // show link via AWT Desktop
	    public boolean connectDesktop() throws Exception {
		if (desktop == null) {
		    if (! Desktop.isDesktopSupported()) return false;
		    desktop = Desktop.getDesktop();
		}
		if (! desktop.isSupported(Desktop.Action.BROWSE))
		    return false;
		URL u = new URL(url);
		URI uri = u.toURI();
		desktop.browse(uri);
		return true;
	    }

	    // show link via BrowserLauncher
	    public boolean connectBrowserLauncher() throws Exception {
		if (browserLauncher == null)
		    browserLauncher = new BrowserLauncher();
		browserLauncher.openURLinBrowser(url);
		return true;
	    }
	
	    public void connectError() {
	    	JOptionPane.showMessageDialog(gnode.gproject().jcomp(),
		    "<html>" +
		    "JSim is unable to automatically connect to you web browser.<br />" +
		    "To get requested help, point your web browser to: <br />" + 
		    url + 
		    "</html>",
		    "Web Browser Connection Error",
		    JOptionPane.INFORMATION_MESSAGE);
	    }
	}

}

