/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// interface for Application functionality in Project

package JSim.project; 

import java.io.*;
import java.net.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import JSim.rclient.*;

public class PApplication implements ASServer.Messenger {

	// common args
	public JSReadable projFile;	// project file
	public JSReadable.List projFileLoads; // files to load into project
	public boolean stackTrace; // print track on error?
	private String securityMgr; // install security manager?
	protected NamedVal.NList serverOptions; // server options
	private String buildDirSfx; // specify build dir .jsim/.workSFX

	// working state
	private File userDir; // user startup dir
	private URL userURL; // for command-line files only
	private DataFormat.List dataFormats; // fmts supported
	private Plugin.List plugins; // client-side plugins
	protected Plugin.List serverPlugins; // server-side plugins
	protected ASInfo.Sandbox sandbox; // sandbox info
	protected ASServer server;  // local or remote JSim server
	private PNamed.List projects; // curr projs for this app
	protected NamedVal.List buildOptions; // compiler options

	// constructors
	public PApplication() throws Xcept {
	    this(new String[0]);
	}
	public PApplication(StringList args) throws Xcept {
	    this(args.array());
	}

	public PApplication(String[] args) throws Xcept {
	    projects = new PNamed.List(4);

	    // defaults
	    projFileLoads = new JSReadable.List(4);
	    StringList xargs = new StringList(4);
	    plugins = new Plugin.List();
	    serverPlugins = new Plugin.List();
	    serverOptions = new NamedVal.NList();
	    buildOptions = new NamedVal.List();

	    // strip args quotes (for Win32)
	    if (args == null) args = new String[0];
	    if (args.length == 1 && Util.isQuoted(args[0]))
		args = new String[] { 
		    "-f" , Util.stripQuotes(args[0])
		};
	    for (int i=0; i<args.length; i++)
		args[i] = Util.stripQuotes(args[i]);

	    // parse command switches
	    for (int i=0; i<args.length; i++) {
	    	if (args[i].equals("-f")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-') {
		    	JSReadable readable = new JSReadable(args[++i]);
		    	if (readable.isProject()) {
			    if (projFile != null) throw new Xcept(
			   	"Only one project may be specified on command line");
			    projFile = readable;
		    	} else
			    projFileLoads.add(readable);
		    }
		} else if (args[i].equals("-plugin")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-') {
			File f = new File(args[++i]);
			Plugin p = new Plugin(f);
			if (p.serverSide())
			    serverPlugins.add(p);
			else
			    plugins.add(p);
		    }
		} else if (args[i].equals("-stack")) {
		    stackTrace = true;
		} else if (args[i].equals("-noscrub")) {
		    serverOptions.setVal("scrub", false);
		} else if (args[i].equals("-sandbox")) {
		    sandbox = new ASInfo.Sandbox();
		    if (i+1<args.length && args[i+1].charAt(0) != '-') {
			sandbox.writePath.addPath(args[++i], File.pathSeparator, 0);
		    	if (i+1<args.length && args[i+1].charAt(0) != '-') 
			    sandbox.readPath.addPath(args[++i], File.pathSeparator, 0);
		    }
		} else if (args[i].equals("-nosandbox")) {
		    sandbox = null;
		} else if (args[i].equals("-usage")) {
		    System.err.println("Switches:\n" + usage());
		    System.exit(0);
		} else if (args[i].equals("-server")) {
		    serverOptions.setVal("server", args[++i]);
		} else if (args[i].equals("-mp") && i+1<args.length) {
		    serverOptions.setVal(
		    	"maxProc", Util.toInt(args[++i]));
		} else if (args[i].equals("-mpdebug") && i+1<args.length) {
		    serverOptions.setVal(
		    	"maxProcDebug", Util.toInt(args[++i]));
		} else if (args[i].equals("-path") && i+1<args.length) {
		    serverOptions.setVal("path", args[++i]);
		} else if (args[i].equals("-securityMgr") && i+1<args.length) {
		    securityMgr = args[++i];
		} else if (args[i].equals("-userdir") && i+1<args.length) {
		    userDir = parseUserDir(args[++i]);
		} else if (args[i].equals("-builddirsfx") && i+1<args.length) {
		    serverOptions.setVal("buildDirSfx", args[++i]);
		} else if (args[i].equals("-userurl") && i+1<args.length) {
		    try {
		    	userURL = new URL(args[++i]);
		    } catch (MalformedURLException e) {
			throw Xcept.wrap(e);
		    }
		} else if (args[i].equals("-v")) {
		    Util.verbose = true;
		} else if (args[i].equals("-c")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			addBuildOption(args[++i]);
	    	} else {
		    xargs.add(args[i]);
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			xargs.add(args[++i]);
		}
	    }

	    // install security manager?
	    if (securityMgr != null) 
	        Util.installSecurityMgr(securityMgr);

	    // update readables with -userurl
	    if (userURL != null)
	    	setRelativeTo(new JSReadable(userURL));
	    else if (userDir != null)
	    	setRelativeTo(new JSReadable(userDir));

	    // set user dir if not set
	    if (userDir == null) 
	    	setUserDir(parseUserDir(null));

	    // basic data formats
	    dataFormats = createDataFormats();

	    // application-specific switches
	    parse_xargs(xargs.array());

	    // load sandbox
	    if (sandbox != null) {
	    	if (projFile != null && projFile.isFile())
		     sandbox.readPath.add(projFile.file().getPath());
	    	sandbox.readPath.addAll(sandbox.writePath);
	    	for (int i=0; i<projFileLoads.size(); i++) {
		    JSReadable r = projFileLoads.readable(i);
		    if (r.isFile())
		        sandbox.readPath.add(r.file().getPath());
	    	}
	    	if (userURL != null)
		    sandbox.hosts.add(userURL.getHost());
		sandbox.readGUI = isGUI();
	    }

	    // initialize server
	    server = ASServer.create(serverOptions, this, sandbox);
		
	    // add server plugins
	    for (int i=0; i<serverPlugins.size(); i++)
	    	server.addPlugin(serverPlugins.plugin(i));
	}
	
	// set projFile and projFileLoads relative to base
	private void setRelativeTo(JSReadable base) throws Xcept {
	    if (projFile != null) 
	    	projFile.setRelativeTo(base);
	    for (int i=0; i<projFileLoads.size(); i++) 
	    	projFileLoads.readable(i).setRelativeTo(base);
	}

	// add build option
	private void addBuildOption(String s) throws Xcept {
	    int inx = s.indexOf('=');
	    if (inx < 0) throw new Xcept(
	    	"Compiler option missing '=': " + s);
	    String n = s.substring(0, inx);
	    String val = s.substring(inx+1, s.length());
	    NamedVal nval = NamedVal.guess(n, val);
	    buildOptions.add(nval);
	}

	// usage message
	public String usage() throws Xcept {
	    return "";
	}

	// simple query
	public ASServer server() { return server; }
	public Plugin.List plugins() { return plugins; }

	// parse extra args
	protected void parse_xargs(String[] args) throws Xcept {
	    if (args.length > 0) throw new Xcept(
		"PApplication.parse_xargs() not implemented");
	}

	// project list management
	protected void addProject(Project p) {
	    projects.addUniq(p);
	}
	public void removeProject(Project p) {
	    projects.remove(p);
	}

	// get PModel for a ASModel.modelID()
	public PModel pmodelForID(String modelID) {
	    if (Util.isBlank(modelID)) return null;
	    for (int i=0; i<projects.size(); i++) {
	    	Project proj = (Project) projects.pnamed(i);
		for (int j=0; j<proj.nChild(); j++) {
		    if (! (proj.child(j) instanceof PModel))
		    	continue;
		    PModel pmodel = (PModel) proj.child(j);
		    if (pmodel.rt().modelID().equals(modelID))
		    	return pmodel;
		}
	    }
	    return null;
	}

  	// print message from server
	public void message(ASInfo.Message msg) {
	    String s = msg.warning ? "warning: " : "";
	    s = s + msg.text;
	    System.err.println(s);
	}

	// import XML message
	public void importXMLMessage(Project proj, String s) {
	    Util.verbose(s);
	}

	// defaults
	public int defaultPlotLineThickness() { 
	    return 1; // medium
	}

	// need AWT+... classes?
	public boolean isGUI() { return false; }

	// public userDir methods
	public File userDir() {
	    if (!isDir(userDir)) userDir = safeUserDir();
	    return userDir;
	}
	public void setUserDir(File f) {
	    userDir = f;
	    if (f == null) return; 
	    userDir = userDir.getAbsoluteFile();
	    if (! userDir.isDirectory()) 
		userDir = userDir.getParentFile();
	}

	// safe default
	private File safeUserDir() {
	    try {
	    	String s = System.getProperty("user.home");
	    	return new File(s);
	    } catch (SecurityException e) {
		return null;
	    }
	}

	// check whether dir exists
	private boolean isDir(File f) {
	    if (f == null) return false;
	    if (!f.exists()) return false;
	    if (!f.isDirectory()) return false;
	    return true;
	}

	// parse userDir in command line
	private File parseUserDir(String s) {
	    try {
	    	if (Util.isBlank(s)) 
		    s = System.getProperty("jsim.userdir");
	    	if (Util.isBlank(s) || s.equals("CURRENT")) 
		    s = System.getProperty("user.dir");
	    	else if (s.equals("HOME"))
		    s = System.getProperty("user.home");
	    	return new File(s);
	    } catch (SecurityException e) {
		return null;
	    }
	}

	// standard data formats
	protected DataFormat.List createDataFormats() 
	throws Xcept {
	    DataFormat.List list = new DataFormat.List();

	    // add plugin data formats
	    for (int i=0; i<plugins.size(); i++) {
		Plugin p = plugins.plugin(i);
		if (! p.type().equals("DataFormat")) continue;
		DataFormat fmt = (DataFormat) p.newInstance(
		    new Class[0], new Object[0]);
		list.add(fmt);
	    }

	    return list;
	}
	public DataFormat.List dataFormats() {
	    return dataFormats;
	}

	// get Data.List from JSReadable
	public Data.List readData(JSReadable readable) throws Xcept {
	    String sfx = readable.fileSuffix();
	    DataFormat fmt = dataFormats().forSuffix(sfx);
	    String s = readable.readText();
	    DataReader rdr = fmt.createReader(s);
	    rdr.setFileName(readable.prettyPath());
	    return rdr.readData();
	}

	// read source for PModel
	public String readSource(JSReadable r, String options) 
	throws Xcept {
	    String text = r.readText();
	    switch (r.fileType()) {
	    case JSReadable.MML:
	    	return text;
	    case JSReadable.ANTIMONY:
	        return "antimony {{\n" + 
		    "  " + text + "\n}}\n";
	    case JSReadable.JAVA:
		String hdr = text.substring(0, text.indexOf('\n'));
		if (! hdr.startsWith(ASModel.JAVA_HDR)) throw new Xcept(
		    "Java file does not conform to JSim model standard");
		String cname = hdr.substring(ASModel.JAVA_HDR.length());
 		text = text.replaceAll(cname, ASModel.JSIM_MODEL_CLASS);
	    	return text;
	    case JSReadable.XML:
	    	return server().translateModelText(
		    ASModel.TEXT_XML, ASModel.TEXT_MML, text, options);
	    default:
	    	throw new Xcept(r, "Unknown model source file type");
	    }
	}

}

