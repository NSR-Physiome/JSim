/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Local server with native method and file write access

package JSim.lserver;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.StringTokenizer;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.mml.*;
import JSim.aserver.*;
import JSim.jruntime.*;
import JSim.nml.ode1.ODE1Solver;
import JSim.nml.pde1.PDE1Solver;
import JSim.nml.opt.*;

// phase out 
import org.w3c.dom.Document;

public final class LSServer extends ASServer {


	// settable properties
	private String jsimPath; // JSim search path
	protected File buildDir;  // build directory
	private int maxProc; // max # proc to use for MP
	private int maxBuildTime; // in seconds
	protected boolean scrub; // scrub work dir on disconnect

	// local stuff
	private static boolean nativeMethodsLoaded;
	private int maxProcAlloc; 
	protected ASServer.Messenger messenger; // or null
	protected ASInfo.Sandbox sandbox; // sandbox info, or null
	protected Plugin.List plugins; // server-side plugins
	private String libPath; // native library search path
	private Hashtable<String,String> commonTexts;
	private Hashtable<String,UnitNList> commonUnits;
	private OptimFactory optimFactory; // optimizer factory
	private LSJavac javac; // java compiler
	protected ClassLoader classLoader; // for loading models
	protected NativeCompiler nativeCompiler; // compiler
	protected LSTranslator translator; // latest translation
	private int lastTempInx; // index to temp file name

	// constructor
	public LSServer(NamedVal.NList options, 
	ASServer.Messenger msgr, ASInfo.Sandbox sandbox) 
	throws Xcept {
	    messenger = msgr;
	    jsimPath = options.stringVal("path", null);
	    maxBuildTime = options.intVal("maxBuildTime", 0);
	    scrub = options.boolVal("scrub", true);
	    int mp = options.intVal("maxProcDebug", 0);
	    if (mp > 0)
	    	setMaxProc(mp, true);
	    else 
	    	setMaxProc(options.intVal("maxProc", 1), false);
	    this.sandbox = sandbox;
	    commonTexts = new Hashtable<String,String>();
	    commonUnits = new Hashtable<String,UnitNList>();

	    // prevent sandboxed models from creating
	    //   unsandboxed server to do their dirty work
	    if (System.getSecurityManager() != null) 
		throw new Xcept(
		   "May not create LSServer in JSim sandbox");

	    // initialize build dir
	    try {
	    	initBuildDir(options);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }

	    // calculate jsimPath, libPath
	    if (Util.isBlank(jsimPath))
		jsimPath = System.getProperty("jsim.path");
	    if (Util.isBlank(jsimPath)) {
		jsimPath = Util.userHome() + 
		    File.separator + ".jsim" + 
		    File.separator + "local" + 
		    File.pathSeparator + 
		    Util.jsimHome() + File.separator + "local" + 
		    File.pathSeparator + 
		    Util.jsimHome() + File.separator + "common";
	    }

	    // tools[45].jar
	    String s = System.getProperty("java.version");
	    char javaVers = (s.length()>2) ? s.charAt(2) : '4';
	    jsimPath = jsimPath + File.pathSeparator + 
		Util.jsimHome() + File.separator + "lib" +
		    File.separator + "tools" + javaVers + ".jar";

	    // add current dir to path?
	    if (sandbox == null)
		jsimPath = jsimPath + File.pathSeparator + ".";
		
	    // more 
	    libPath = jsimPath;
	    jsimPath = jsimPath + File.pathSeparator + buildDir;
	    if (sandbox == null) 
		libPath = libPath + File.pathSeparator + buildDir;

	    // create model class loader
            classLoader = new JSClassLoader(jsimPath, libPath);

	    // initialize plugins, optimizers
	    plugins = new Plugin.List();

	    // load native compiler
	    nativeCompiler = new NativeCompiler(this);

	    // set up sandbox
	    if (sandbox != null) 
		System.setSecurityManager(
		    new LSSandbox(this, sandbox));
	}

	// initialize buildDir
	private void initBuildDir(NamedVal.NList options) throws Exception {
	    String bd = options.stringVal("buildDir", null);
	    if (bd != null) buildDir = new File(bd);
	    if (buildDir == null) {
	        File jdir = initJSimDir();
	    	String sfx = options.stringVal("buildDirSfx", null);
		if (sfx == null)
		    makeNextBuildDir(jdir);
		else
		    makeSfxBuildDir(jdir, sfx);
	    }
	    if (! buildDir.isAbsolute()) {
		try {
		    buildDir = buildDir.getCanonicalFile();
		} catch (IOException e) {
		    throw Xcept.wrap(e);
		}
	    }
	    if (! buildDir.exists())
	    	buildDir.mkdir();

	    scrubBuildDir();
	}

	// init $HOME/.jsim directory
	private File initJSimDir() throws Exception {
	    File dir = new File(Util.userHome() +
		File.separator + ".jsim");
	    makeDir(dir);
	    makeDir(new File(dir, "local"));
	    scrubOldDirs(dir.listFiles());
	    return dir;
	}

	// create next available build dir
	private void makeNextBuildDir(File jdir) throws Exception {
	    int RETRIES = 1000000;
	    for (int sfx=0; sfx<RETRIES; sfx++) {
		buildDir = new File(jdir, "work" + sfx);
		if (buildDir.exists()) continue;
	        if (buildDir.mkdir()) return;
	    }
	    throw new Xcept("Failed to create JSim build directory after "
	        + RETRIES + " attempts.");
	}	    

	// create specific sfx build dir
	//   throw Xcept if already exists
	private void makeSfxBuildDir(File jdir, String sfx) throws Exception {
	    buildDir = new File(jdir, "work" + sfx);
	    if (buildDir.exists()) throw new Xcept(
	    	"Can't use requested build directory suffix: " + 
		buildDir +  " already exists");
	    if (! buildDir.mkdir()) throw new Xcept(
	    	"Failed to create build directory: " + buildDir);
	}
	
	// make a directory, remove junk if in the way
	private void makeDir(File f) {
	    if (! f.exists()) 
		f.mkdir();
	    if (! f.isDirectory()) {
		f.delete();
		f.mkdir();
	    }
	}

	// set scrubbing on 30 days old dirs
	private void scrubOldDirs(File[] files) {
	    if (! scrub) return;
	    if (files == null) return;
	    for (int i=0; i<files.length; i++) {
		File f = files[i];
		if (! f.getName().startsWith("work")) continue;
		if (UtilIO.dirNesting(f, 2) > 1) continue;
		long age = System.currentTimeMillis() - UtilIO.lastModifiedDir(f);
		age = age / (24*3600*1000); // convert age to days
		if (age > 30) UtilIO.deleteOnExit1(f);
	    }
	}

	// set scrubbing on build dir
	protected void scrubBuildDir() {
	    if (! scrub) return;
	    UtilIO.deleteOnExit1(buildDir);
	}

	// default solver parms
	public NamedVal.NList getSolverDefaults() {
	    return RTSolverSettings.getDefaults();
	}

	// query/create optimizer list
	private void initOptimFactory() {
	    if (optimFactory == null) 
	    	optimFactory = new OptimFactory();
	}
	public OptimFactory optimFactory() { 
	    initOptimFactory();
	    return optimFactory; 
	}
	public OptimAlg.NList optimAlgs() throws Xcept {
	    initOptimFactory();
	    return optimFactory.algs();
	}
	public OptimAlg.Info[] optimAlgsInfo() throws Xcept {
	    initOptimFactory();
	    return optimFactory.algInfo();
	}

	// translate model text
	public String translateModelText(int srcType, int destType,
	String srcText, String options) throws Xcept {
	    translator =  
	    	new LSTranslator(this, srcType, srcText);
	    return translator.getText(destType, options);
	}
	
	// get tranlator warnings
	public String[] getTranslatorWarnings() throws Xcept {
	    if (translator == null) return null;
	    StringList w = translator.getWarnings();
	    if (w == null) return null;
	    return w.array();
	}
	    
	// create local model
	public ASModel newModelRT() throws Xcept {
	    return new LSModel(this);
	}

	// properties
	public int maxBuildTime() { return maxBuildTime; }
	public Plugin.List plugins() { return plugins; }

	// get/set dynamic server properties
	public void setProperties(NamedVal[] nvals) throws Xcept {
	    if (nvals == null) return;
	    for (int i=0; i<nvals.length; i++) 
	    	setProperty(nvals[i]);
	}
	public void setProperty(NamedVal nval) throws Xcept {
	    String n = nval.name();
	    if (n.equals("maxProc"))
	    	setMaxProc(nval.intVal(), false);
	    else if (n.equals("maxProcDebug"))
	    	setMaxProc(nval.intVal(), true);
	    else 
		throw new Xcept(nval, "Unrecognized JSim server property");
	}
	public NamedVal getProperty(String n) throws Xcept {
	    if (n.equals("maxProc"))
	    	return NamedVal.create(n, maxProc());
	    return null;
	}

	// manage maxProc
	public int maxProc() { return maxProc; }
	void setMaxProc(int i, boolean debug) {
	    int sproc = Runtime.getRuntime().availableProcessors();
	    maxProc = i;
	    if (! debug && sproc < maxProc) maxProc = sproc;
	    if (maxProc < 1) maxProc = 1;
	    Util.verbose("Now utilizing " + 
	    	maxProc + " compute threads on " + 
		sproc + " system processors.");
//	    allocNativeThreads(2*maxProc + 1); // MOPT is worst-case 
	}

	// disconnect server
	public void disconnect() {
	    scrubBuildDir();
	}

	// init native libs, allocate for native threads
	public void prepNativeMethods() throws Xcept {

	    // native libraries
	    if (! nativeMethodsLoaded) {
	    	try {
	    	    System.loadLibrary("odesolver");
	    	    System.loadLibrary("pdesolver");
	    	    System.loadLibrary("opt");
		    nativeMethodsLoaded = true;
	        } catch (Exception e) {
		    throw Xcept.wrap(e);
	        }
	    }	    

	    // allocate native thread slots
	    if (maxProc <= maxProcAlloc) return;
	    int n = 2*maxProc + 2;
	    ODE1Solver.allocNativeThreads(n);
	    PDE1Solver.allocNativeThreads(n);
	    Optimizer.allocNativeThreads(n);
	    maxProcAlloc = maxProc;
	}

	// paths
	protected String jsimPath() { return jsimPath; }
	protected String libPath() { return libPath; }

	// common texts
	public String getCommonText(String fileName) throws Xcept {
	    String fileText = commonTexts.get(fileName);
	    if (fileText != null) return fileText;
	    
	    File f = new File(Util.jsimHome() + File.separator 
	    	+ "common" + File.separator + fileName);
	    fileText = UtilIO.readText(f);
	    commonTexts.put(fileName, fileText);
	    return fileText;
	}

	// get common units
	public UnitNList getCommonUnits(String fileName) throws Xcept {
	    UnitNList units = commonUnits.get(fileName);
	    if (units != null) return units;

	    String text = getCommonText(fileName);
	    Model umodel = new ModelReader(text);
	    units = umodel.units;
	    commonUnits.put(fileName, units);
	    return units;
	}

	// memory usage message
	public String memoryMessage() {
	    return "Memory: " + Util.memoryMessage();
	}

	// get name of work file in buildDir
	protected File newWorkFile(String baseName, String sfx) 
	throws Xcept {
	    try {
	    	String name = baseName + "_" + lastTempInx++;
	    	if (! Util.isBlank(sfx))
	            name = name + "." + sfx;
	    	File f = new File(buildDir, name);
	    	f = f.getCanonicalFile();
	    	return f;
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// Java compiler
	protected LSJavac javac() throws Xcept {
	    if (javac == null) javac = new LSJavac(this);
	    return javac;
	}
	    
	// add plugins
	public void addPlugin(Plugin plugin) throws Xcept {
	    plugins.add(plugin);
	    optimFactory().add(plugin);
	}

	// dump settings
	private void dumpSettings() {		
	    System.out.println("Local Server Settings:");
	    System.out.println("  path=" + jsimPath);
	    System.out.println("  buildDir=" + buildDir);
	    System.out.println("  maxProc=" + maxProc);
	    System.out.println("  maxBuildTime=" + maxBuildTime);
	    System.out.println("  scrub=" + scrub);
	    System.out.println("  sandbox=" + sandbox);
	}
}
