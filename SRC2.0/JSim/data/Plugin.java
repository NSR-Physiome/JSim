/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim plug-in functionality

package JSim.data;

import java.lang.reflect.*;
import java.io.*;
import java.util.*;
import java.util.jar.*;

import JSim.util.*; 

public class Plugin {
	private File file; // plugin file
	private String type; // DataFormat, ...
	private String variant; // fortran, simulink, ...
	private String className; // class to instantiate
	private ClassLoader cloader; // loads class from file
	
	// constructor
	public Plugin(File f) throws Xcept {
	    try {
	    	file = f;
	    	JarFile jar = new JarFile(file);
	    	Manifest man = jar.getManifest();
	    	Attributes attrs = man.getMainAttributes();
	    	type = attrs.getValue("JSimPluginType");
	    	variant = attrs.getValue("JSimPluginVariant");
	    	className = attrs.getValue("JSimPluginClass");
		jar.close();
		if (Util.isBlank(type) 
		|| Util.isBlank(variant)
		|| Util.isBlank(className))
		    throw new Xcept("Plugin file " + file +
		    	" manifest missing one of required entries: " +
			"JSimPluginType, JSimPluginVariant, JSimPluginClass");
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
  	    }		
	}

	// create new instance of className 
	public Object newInstance(Class[] argClasses, Object[] args)
	throws Xcept {
	    return newInstance(classLoader(), className, argClasses, args);
	}	

	// simple query
	public String toString() { return file.getName(); }
	public File file() { return file; }
	public String type() { return type; }
	public String variant() { return variant; }
	public String className() { return className; }
	public boolean serverSide() {
	    if (type.equals("Optimizer")) return true;
	    return false;
	}
	public ClassLoader classLoader() { 
	    if (cloader == null)
	        cloader = new JSClassLoader(file.getPath(), "");
	    return cloader; 
	}

	//// static methods

	// create new instance of any class
	public static Object newInstance(ClassLoader cloader,
	String className, Class[] argClasses, Object[] args) 
	throws Xcept {
	    try {
	    	Class<?> clss = cloader.loadClass(className);
	    	Constructor cons = clss.getConstructor(argClasses);
	    	return cons.newInstance(args);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
  	    }
	}	    

	// create new 
	public static Object runMethod(Object obj, String methName,
	Class[] methClasses, Object[] args) throws Xcept {
	    try {
	    	Class<?> clss = obj.getClass();
		Method meth = clss.getMethod(methName, methClasses);
		return meth.invoke(obj, args);
	    }  catch (Exception e) {
	    	throw Xcept.wrap(e);
  	    }
	}	

	// Plugin.List
	public static class List extends ArrayList<Plugin> {
	    public List() { super(); }
	    public Plugin plugin(int i) { return (Plugin) get(i); }
	    public Plugin plugin(String t, String v) {
	    	for (int i=0; i<size(); i++) {
		    Plugin p = plugin(i);
		    if (! p.type().equals(t)) continue;
		    if (Util.isBlank(v)) return p;
		    if (p.variant().equals(v)) return p;
		}
		return null;
	    }
	    public List plugins(String t) {
	    	List list = new List();
		for (int i=0; i<size(); i++) {
		    Plugin p = plugin(i);
		    if (p.type().equals(t)) list.add(p);
		}
		return list;
	    }
	}

	//// test program
	public static void main(String[] args) throws Exception {
	    File f = new File(args[0]);
	    Plugin plugin = new Plugin(f);
	    System.err.println("type=" + plugin.type());
	    System.err.println("variant=" + plugin.variant());
	    DataFormat fmt = 
	    	(DataFormat) plugin.newInstance(new Class[0], new Object[0]);
	    DataWriter wrt = fmt.createWriter();
	    Data.List dlist = new Data.List(1);
	    Data grid = new RegularGridData("TIME", null, 0.0, 5, 9);
	    dlist.add(grid);
	    wrt.writeData(System.out, dlist);
	}
}

