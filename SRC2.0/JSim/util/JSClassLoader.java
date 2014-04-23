/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim customized class loader

package JSim.util;

import java.util.*;
import java.util.jar.*;
import java.util.zip.*;
import java.io.*;

public class JSClassLoader extends ClassLoader {
	private StringList classPath; // for java classes
	private File[] classArch; // java class archives (.jar/dir)
	private JarFile[] classJars; // if classArch is .jar
	private Hashtable<String, Class> classCache; // cache for loaded classes
	private StringList libPath; // for native libraries
	private File[] libArch; // native lib archives (dirs)
	
	// constructor
  	public JSClassLoader(String cpath, String lpath) {
	    super(getSystemClassLoader());	
    	    classPath = new StringList(cpath, File.pathSeparator);
	    int n = classPath.size();
	    classArch = new File[n];
	    classJars = new JarFile[n];
	    for (int i=0; i<n; i++) {
		File f = new File(classPath.str(i));
		classArch[i] = f;
		if (f.isFile() && f.getName().endsWith(".jar")) {
		    try {
		    	classJars[i] = new JarFile(f);
		    } catch (Exception e) {
		    	classJars[i] = null; // not really needed
		    }
		}
	    }
	    classCache = new Hashtable<String,Class>();
	    libPath = new StringList(lpath, File.pathSeparator);
	    libArch = new File[libPath.size()];
	    for (int i=0; i<libPath.size(); i++) 
		libArch[i] = new File(libPath.str(i));
  	}

	// load a class
	public Class loadClass(String name, boolean resolve) 
	throws ClassNotFoundException {
	    Class c = (Class) classCache.get(name);
	    if (c == null) c = loadNewClass(name);
    	    if (resolve) resolveClass(c);
	    return c;
	}

	// load a class not already in classCache
	private Class loadNewClass(String name)
	throws ClassNotFoundException {
	    Class c = null;
	    try {
	        ClassLoader parent = getClass().getClassLoader();
	        if (parent != null) 
		    c = parent.loadClass(name);
	        else
		    c = findSystemClass(name);
	    } catch (ClassNotFoundException e) {
		byte[] data = loadClassData(name);
	        c = defineClass(name, data, 0, data.length);
	    } catch (NoClassDefFoundError e) {
	        byte[] data = loadClassData(name);
	        c = defineClass(name, data, 0, data.length);
	    }
	    classCache.put(name,c);
	    return c;
    	}

	// load class data for given class name
	public byte[] loadClassData(String name) throws ClassNotFoundException {
	    name = name.replace('.', File.separator.charAt(0))+".class";
	    for (int i=0; i<classArch.length; i++) {
		byte[] data = null;
		if (classJars[i] == null) 
		    data = loadDirFile(classArch[i], name);
		else
		    data = loadJarEntry(classJars[i], name);
		if (data != null) return data;
	    }
    	    throw new ClassNotFoundException("Class " + name + " not found");
  	}

	// load class data from a file	
	private byte[] loadDirFile(File f, String name) 
	throws ClassNotFoundException {
	    f = new File(f, name);
	    try {
		InputStream istr = new FileInputStream(f);
		return UtilIO.readBytes(istr);
	    } catch (FileNotFoundException e) {
		return null;
	    } catch (Exception e) {
		throw new ClassNotFoundException(
		    "Error loading class " + f + ": " + e);
	    }
	}

	// load class data from jar entry
	private byte[] loadJarEntry(JarFile jarFile, String name) 
	throws ClassNotFoundException {
	    name = name.replace('\\', '/'); // win32 hack
	    try {
	    	ZipEntry zent = jarFile.getEntry(name);
	    	if (zent == null) return null;
	    	InputStream istr = jarFile.getInputStream(zent);
	    	return UtilIO.readBytes(istr);
	    } catch (Exception e) {
		throw new ClassNotFoundException(
		    "Error reading jarfile " + jarFile + " class " + name + ": " + e);
	    }	
	}
	
	// get java resource as stream
  	public InputStream getResourceAsStream(String name) {
	    InputStream result = getSystemResourceAsStream(name);
	    if (result != null) return result;
	    for (int i=0; i<classArch.length; i++) {
	        if (classJars[i] == null) {
		    File f = new File(classArch[i], name);
		    try {
		        if (f.exists()) return new FileInputStream(f);  
		    } catch (IOException e) {
		        // nothing to do
		    }
		} else {
		    try {
		    	ZipEntry zent = classJars[i].getEntry(name);
		    	if (zent == null) continue;
		    	return classJars[i].getInputStream(zent);
		    } catch (IOException e) {
		    	// nothing to do
		    }
		}
	    }
	    return null;
	}

	// find native library in path
	protected String findLibrary(String libname) {
	    String natname = System.mapLibraryName(libname);
	    for (int i=0; i<libArch.length; i++) {
		File f = new File(libArch[i], natname);
		if (f.exists()) {
		    String ret = f.getAbsolutePath();
//		    System.out.println("findLibrary() found" + ret);
		    return ret;
		}
	    }
//	    System.out.println("findLibrary() did not find " + natname);
	    return null;
	}
}

