/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// harvest models from file-system for testing

package JSim.plan;

import java.io.*;	
import java.util.*;
import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public class Harvest {
	public File destDir;
	public PrintWriter inxwrt;
	public int ninxs;
	public Hashtable<Integer,String> mmlIDs;
	public PApplication pappl;
	
	public static String INDEX = "harvest.inx";

	// constructor
	public Harvest(File destDir) throws Exception {
	    this.destDir = destDir;
	    if (! destDir.exists()) 
	    	destDir.mkdir();
	    if (! destDir.isDirectory()) throw new Exception(
	    	"Can't create dest dir " + destDir);
	    File finx = new File(destDir, INDEX);
	    inxwrt = new PrintWriter(new FileWriter(finx), true);
	    ninxs = 0;
	    mmlIDs = new Hashtable<Integer,String>();
	    pappl = new PApplication(new String[0]);
	}
	
	// harvest a dir/file
	public void harvest(File f) throws Exception {
	    String name = f.getName();
	    if (name.startsWith(".")) return;
	    if (! f.canRead()) {
	    	System.out.println("Unreadable: " + f);
		return;
	    }
	    if (f.isDirectory()) 
	    	harvestDir(f);
	    else if (name.endsWith(".mod"))
	    	harvestMod(f);
	    else if (name.endsWith(".proj")) 
	    	harvestProj(f);
	}

	// harvest directory
	public void harvestDir(File dir) throws Exception {
	    File f = new File(dir, INDEX);
	    if (f.exists()) {
	    	System.out.println("Ignoring harvest dest " + dir);
		return;
	    }
	    System.out.println("Searching " + dir + " ...");
	    File[] fs = dir.listFiles();
	    for (int i=0; i<fs.length; i++)
		harvest(fs[i]);
	}

	// harvest mod file
	public void harvestMod(File f) throws Exception {
	    String mml = UtilIO.readText(f);
	    processMML(f, null, mml);
	}
	
	// harvest proj file
	public void harvestProj(File f) throws Exception {
	    Project proj = new Project("proj", pappl);
	    JSReadable r = new JSReadable(f);
	    try {
	    	proj.importXML(r);
	    	for (int i=0; i<proj.nChild(); i++) {
		    if (! (proj.child(i) instanceof PModel)) continue;
		    PModel pmodel = (PModel) proj.child(i);
		    String mml = pmodel.modelSource.stringVal();
		    processMML(f, pmodel.name(), mml);
		}
	    } catch (Exception e) {
	    	System.out.println("Ignoring read error " + f);
	    }
	}


	// process MML
	public void processMML(File f, String pmodel, String mml) 
	throws Exception {
	    String id = f.getPath();
	    if (pmodel != null) id = id + "#" + pmodel;
	    mml = mml.trim() + "\n";
	    Integer hmml = new Integer(mml.hashCode());
	    String oid = mmlIDs.get(hmml);
	    if (oid != null) {
	    	System.out.println("Ignoring " + id + " matches " + oid);
		return;
	    }
	    mmlIDs.put(hmml, id);
	    int inx = 1000 + ++ninxs;
	    String fname = "" + inx + ".mod";
	    inxwrt.println("" + fname + "\t" + hmml + "\t" + id);
	    File fout = new File(destDir, fname);
	    UtilIO.writeText(fout, mml);
	}

	// launch program
	public static void main(String[] args) throws Exception {
	    if (args.length < 2) throw new Exception(
	    	"Usage: harvest destDir [ srcDir ... ]");
	    Harvest h = new Harvest(new File(args[0]));
	    for (int i=1; i<args.length; i++)
	    	h.harvest(new File(args[i]));
	}
}














