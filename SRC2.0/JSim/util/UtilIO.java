/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim File and IO utility routines

package JSim.util;

import java.util.*;
import java.io.*;
import java.net.URL;

public class UtilIO {

	//// File/path name utilities

	// find file in path
	public static String pathFind(String fname, String path)
	throws Xcept {
	    StringTokenizer tok = 
		new StringTokenizer(path, File.pathSeparator);
	    int ct = tok.countTokens();
	    for (int i=0; i<ct; i++) {
		String dir = tok.nextToken();
		String tname = dir + File.separator + fname;
		try {
		    FileInputStream str = new FileInputStream(tname);
		    str.close();
		    return tname;
		} catch (IOException e) {
		    // nope,  not this one
		}
	    }
	    throw new Xcept("Cannot find file " + fname + 
		" in path " + path);
	}

	// pretty absolute path, security safe
	public static String prettyPath(File f) {
	    if (f == null) return "<untitled>";
	    try {
		return f.getAbsolutePath();
	    } catch (SecurityException e) {
		return f.getPath();
	    }
	}

	// get . separated suffix from path name
	public static String fileSuffix(File f) {
	    String s = f.getName();
	    int inx = s.lastIndexOf('.');
	    return (inx<0) ? "" : s.substring(inx+1);
	}

	// short file name,  minus . separated suffix
	public static String fileBaseName(File f) {
	    String name = f.getName();
	    int inx = name.lastIndexOf('.');
	    return (inx<0) ? name : name.substring(0, inx);
	}

	// bump file number
	public static File bump(File f) throws Xcept {
	    String s = f.getName();
	    int pinx = s.lastIndexOf('.');
	    if (pinx<0) pinx=s.length();
	    int ninx = pinx;
	    while (ninx > 0 && 
	    Character.isDigit(s.charAt(ninx-1)))
		ninx--;
 	    int n = (ninx<pinx) ? 
		Util.toInt(s.substring(ninx, pinx)) : 0;
	    String s1 = s.substring(0, ninx) + (n+1) +
		s.substring(pinx);
	    String p = f.getParent();
	    if (p != null)
		s1 = p + File.separator + s1;
	    return new File(s1);
	}

	// nesting level up to max 
	public static int dirNesting(File dir, int max) {
	    if (max<1 || !dir.isDirectory()) return 0;
	    if (max == 1) return 1;
	    File[] f = dir.listFiles();
	    if (f == null) f = new File[0];
	    int fnest = 0;
	    for (int i=0; i<f.length; i++) {
		int j = dirNesting(f[i], max-1);
		if (j>fnest) fnest = j;
	    }
	    return fnest+1;
	}

	// last mod time for dir components
	public static long lastModifiedDir(File dir) {
	    long t = dir.lastModified();
	    File[] f = dir.listFiles();
	    if (f == null) f = new File[0];
	    for (int i=0; i<f.length; i++) {
		long ft = lastModifiedDir(f[i]);
		if (ft>t) t = ft;
	    }
	    return t;
	}

	// remove dir and 1st level contents on exit
	public static void deleteOnExit1(File dir) {
	    dir.deleteOnExit();
	    File[] files = dir.listFiles();
	    if (files == null) files = new File[0];
	    for (int i=0; i<files.length; i++) {
		File f = files[i];
		f.deleteOnExit();
	    }
	}

	// remove dir and 1st level contents now
	public static boolean deleteDir1(File dir) {
	    File[] files = dir.listFiles();
	    if (files == null) files = new File[0];
	    for (int i=0; i<files.length; i++) {
		File f = files[i];
		if (! f.delete())
		    return false;
	    }
	    return dir.delete();
	}

	// remove dir and 1st & 2st level contents now
	public static boolean deleteDir2(File dir) {
	    File[] files = dir.listFiles();
	    if (files == null) files = new File[0];
	    for (int i=0; i<files.length; i++) {
		File f = files[i];
		if (! deleteDir1(f))
		    return false;
	    }
	    return dir.delete();
	}

	// query dir nesting level, up to provided max
	public static int dirNestingLevel(File dir, int maxNest) 
	throws Xcept {
//System.err.println("Checking " + dir.getAbsoluteFile());
	    if (! dir.canRead()) throw new Xcept(
	    	"Can't read " + dir.getAbsoluteFile());
	    if (maxNest <= 0) return 0;
	    if (! dir.isDirectory()) return 0;
	    File[] f = dir.listFiles();
	    if (f == null) return 0;
	    int nest = 0;
	    for (int i=0; i<f.length; i++) {
	    	int n = 1 + dirNestingLevel(f[i], maxNest-1);
		if (n <= nest) continue;
		nest = n;
		if (nest >= maxNest) break;
	    }
	    return nest;
	}

	//// file/url read/write utilities

	// read text file
	public static String readText(Reader r) throws Xcept {
	    BufferedReader rdr = new BufferedReader(r);
	    StringWriter wrt = new StringWriter();
	    try {
		String s; 
		while ((s = rdr.readLine()) != null) 
		    wrt.write(s);
	    } catch (IOException e) {
		throw new Xcept("IOException during readText(Reader)");
	    }
	    return wrt.toString();
	}
	public static String readText(InputStream s) throws Xcept {
	    return readByteStr(s).toString();
	}	    
	public static String readText(File f) throws Xcept {
	    return readByteStr(f).toString();
	}	    
	public static byte[] readBytes(InputStream s) throws Xcept {
	    return readByteStr(s).toByteArray();
	}
	public static byte[] readBytes(File f) throws Xcept {
	    return readByteStr(f).toByteArray();
	}	    
	public static String readText(URL url) throws Xcept {
	    return readByteStr(url).toString();
	}	    
	public static byte[] readBytes(URL url) throws Xcept {
	    return readByteStr(url).toByteArray();
	}	    

	// convert bytes to String
	// compensate for JVM buffer error
	public static String readText(byte[] b) {
	    try {
		return new String(b);
	    } catch (Exception e) { 
	    	// compensation for java.nio.BufferOverflowException
	    	//    this class missing from some MacOS browsers
	    	System.err.println("compensating for " + e.getClass() + " ...");
	    	byte[] b1 = new byte[b.length+1];
	    	for (int i=0; i<b.length; i++)
		    b1[i] = b[i];
	    	b1[b.length] = '\n';
	    	return new String(b1);
	    }
	}

	// read stream contents
	private static ByteArrayOutputStream readByteStr(URL url) 
	throws Xcept {
	    Util.verbose("Reading URL " + url);
	    try {
	    	return readByteStr(url.openStream());
	    } catch (IOException e) {
		throw new Xcept("Error reading URL: " + url);
	    }
	}
	private static ByteArrayOutputStream readByteStr(File f) 
	throws Xcept {
	    Util.verbose("Reading file " + f);
	    try {
	    	FileInputStream fs = new FileInputStream(f);
		ByteArrayOutputStream bs = readByteStr(fs);
		fs.close();
		return bs;
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}
	private static ByteArrayOutputStream readByteStr(InputStream fstr) 
	throws Xcept {
	    try {
		ByteArrayOutputStream bstr = 
		    new ByteArrayOutputStream();
		while (true) {
		    int ct = fstr.available();
		    if (ct == 0) {
			int b = fstr.read();
			if (b<0) break;
			bstr.write(b);
		    } else {
		    	byte[] bs = new byte[ct];
		    	int ct1 = fstr.read(bs);
			if (ct1<0) break;
		    	bstr.write(bs, 0, ct1);
		    }
		}
		return bstr;
	    } catch (IOException e) {
		throw new Xcept("IO error reading input stream");
	    }
	}

	// write byte file
	public static void writeBytes(File file, byte[] data)
	throws Xcept {
	    try {
	    	FileOutputStream out = new FileOutputStream(file);
	    	out.write(data);
	    	out.close();
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}
	    	

	// write text file
	public static void writeText(File file, String text)
	throws Xcept {
	    try {
		PrintStream fstr = new PrintStream(
		    new FileOutputStream(file));
		fstr.print(text);
		fstr.close();
	    } catch (IOException e) {
		throw new Xcept(
		    "IO error writing text file: " + file);
	    }
	}	    

	//// Debug only utilities

	// show file size (debugging only)
	public static void showFileSize(File f) {
	    try {
	    	String s = readText(f);
	    	System.err.println(f.toString() + " size=" + s.length());
	    } catch (Exception e) {
		System.err.println(f.toString() + " " + e);
	    }
	}

	// debugable FileOutputStream
	public static class DebugFileOutputStream extends FileOutputStream {
	    public int ct = 0;
	    public DebugFileOutputStream(String n) throws FileNotFoundException { 
		super(n); 
	    }
	    public void write(byte[] b) throws IOException {
		System.err.println("dbstream write byte[]");
		super.write(b);
		ct += b.length;
	    }
	    public void write(byte[] b, int off, int len) throws IOException {
		System.err.println("dbstream write byte[] ofs");
		super.write(b, off, len);
		ct += len;
//		int nullct = 0;
//		for (int i=off; i<off+len; i++) 
//		    if (b[i] == 0) nullct++;
//		if (nullct > 0) 
//		    System.err.println("mystream writing nulls " + nullct + "/" + len);
//		if (nullct > 0) 
//		    Thread.currentThread().dumpStack();
	    }
	    public void write(byte b) throws IOException {
		System.err.println("mystream write byte");
		super.write(b);
		ct += 1;
	    }
	}

	// test harness
	public static final void main(String[] args) throws Exception {
	    File f = new File(args[0]);
	    int maxNest = Util.toInt(args[1]);
	    int nest = dirNestingLevel(f, maxNest);
	    System.out.println("File " + f + " nest=" + nest);
	    if (nest > 2) return;
	    System.out.print("Delete file " + f + "(yes/no)? ");
	    BufferedReader rdr = new BufferedReader(
	    	new InputStreamReader(System.in));
	    String yn = rdr.readLine();
	    if (! yn.equals("yes")) return;
	    boolean stat = false;
	    if (nest == 2) {
	    	System.out.println("deleteDir2 " + f);
		stat = deleteDir2(f);
	    } else {
	    	System.out.println("deleteDir1 " + f);
		stat = deleteDir1(f);
	    }
	    System.out.println("Success=" + stat);	    
	}
}

