/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// optimizer factory

package JSim.nml.opt;

import java.lang.reflect.*;
import java.io.File;
import java.util.ArrayList;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

public class OptimFactory {
 	private StringList names;
	private ArrayList<ClassLoader> cloaders;
	private ArrayList<OptimAlg.Info> infos;
	private OptimAlg.NList algs;

	// constructor
	public OptimFactory() {
	    names = new StringList();
	    infos = new ArrayList<OptimAlg.Info>();
	    cloaders = new ArrayList<ClassLoader>();
	    algs = new OptimAlg.NList();
	    add(Simplex.algInfo());
	    add(GGopt.algInfo());
	    add(GridSearch.algInfo());
	    add(NelderMead.algInfo());
	    add(NL2sol.algInfo());
	    add(Sensop.algInfo()); 
	    add(SimAnneal.algInfo()); 
	    add(Genetic.algInfo()); 
	}

	// add new Info
	private void add(OptimAlg.Info info) {
	    add(OptimFactory.class.getClassLoader(), info);
	}
	private void add(ClassLoader cloader, OptimAlg.Info info) {
	    names.add(info.name);
	    cloaders.add(cloader);
	    infos.add(info);
	    algs.add(new OptimAlg(info));
	}

	// add plugins
	public void add(Plugin.List list) throws Xcept {
	    for (int i=0; i<list.size(); i++) 
	        add(list.plugin(i));
	}
	public void add(Plugin plugin) throws Xcept {
	    if (! plugin.type().equals("Optimizer")) return;
	    ClassLoader cloader = plugin.classLoader();
	    OptimAlg.Info info = getInfo(
		cloader, plugin.className());
	    info.name = plugin.variant();
	    add(cloader, info);
	}

	// create Optimizer
	public Optimizer createOptimizer(String name) throws Xcept {
	    int inx = names.indexOf(name);
	    if (inx < 0) throw new Xcept(
	    	name + ": Unknown optimization algorithm"); 
	    ClassLoader cloader = cloaders.get(inx);
	    String className = infos.get(inx).optimClassName;
	    try {
	    	return (Optimizer) Plugin.newInstance(
	    	    cloader, className, new Class[0], new Object[0]);
	    } catch (ClassCastException e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// create OptimAlg
	private static OptimAlg.Info getInfo(ClassLoader cloader,
	String className) throws Xcept {
	    try {
	    	Class<?> clss = cloader.loadClass(className);
		Method meth = clss.getMethod(
		    "algInfo", new Class[0]);
		return (OptimAlg.Info) meth.invoke(
		    null, new Object[0]);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}
	    
	// query
	public OptimAlg.Info[] algInfo() { 
	    return infos.toArray(new OptimAlg.Info[0]);
	}
	public OptimAlg.NList algs() { 	return algs; }

	// test program
	public static void main(String[] args) throws Exception {
	    String name = args[0];
	    OptimFactory factory = new OptimFactory();
	    OptimAlg alg = factory.algs().alg(name);
	    System.err.println("alg=" + alg);
	    Optimizer opt = factory.createOptimizer(name);
	    System.err.println("opt=" + opt);
	    
	}
}

	    
