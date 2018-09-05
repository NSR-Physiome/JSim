/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// general-purpose JSim utility routines

package JSim.util;

import java.io.*;
import java.util.Locale;
import java.lang.reflect.Constructor;

public class Util {
	//// JSim environment utitilies

	// JSim version control
	public static final String version() {
	    return "2.18";
	}

	// Java/OS versions
	public static final String javaOSVersion() {
	    return System.getProperty("os.name") + "(" +
		System.getProperty("os.arch") + ") Java " +
	        System.getProperty("java.version");
	}

	// JSIMHOME directory
	public static String jsimHome() throws Xcept {
	    String home = System.getProperty("jsim.home");
	    if (home == null) throw new Xcept(
		"jsim.home property not defined");
	    return home;
	}

	// USER HOME directory
	public static String userHome() throws Xcept {
	    String uhome = System.getProperty("user.home");
	    String ehome = System.getenv("HOME");
	    String home = uhome;
	    if (! isDir(home))
	        home = ehome;
	    if (! isDir(home)) throw new Xcept(
	    	"Neither Java user.home property: Directory <" + uhome + 
		"> nor $HOME environment variable <" + ehome + ">  exist");
	    return home;
	}

	// does string represent and existing dir?
	private static boolean isDir(String s) {
	    if (Util.isBlank(s)) return false;
	    File f = new File(s);
	    return f.isDirectory();
	}

	// is running under MacOS?
	//    recommended EL 19 May 2004
	public static boolean isMacos() {
	    String s = System.getProperty("os.name");
	    if (s == null) return false;
	    return s.toLowerCase().startsWith("mac os x");
	}

	// is PPC architecture?
	public static boolean isPPC() {
	    String s = System.getProperty("os.arch");
	    if (s == null) return false;
	    return s.toLowerCase().startsWith("ppc");
	}
	    

	// is running Windows?
	public static boolean isWin32() {
	    String s = System.mapLibraryName("dummy");
	    return s.endsWith(".dll");
	}

	// is using JVM 1.5 or above?
	public static boolean isJava15() { 
	    return System.getProperty("java.version").startsWith("1.5");
	}

	// JSim OS name: linux, macos or win32 
	public static String jsimOSName() {
	    if (isWin32()) return "win32";
	    if (isMacos()) return "macos";
	    return "linux";
	}

	/////  memory utils	
	private static final long meg = 1024*1024;
        private static long maxMemUsed = 0;

	// get memory message
	public static String memoryMessage() {
	    int[] stats = new int[4];
	    Runtime r = Runtime.getRuntime();
	    if((r.maxMemory()- r.freeMemory()) > maxMemUsed) {
		maxMemUsed = r.totalMemory()- r.freeMemory();// maxMemory() or totalMemory()?
	    }
	    stats[0] = (int) (r.totalMemory()/meg);
	    stats[1] = (int) (r.freeMemory()/meg);
	    long max = r.maxMemory();
	    if (max == Long.MAX_VALUE) max = 0;
	    stats[2] = (int) (max/meg);
	    stats[3] = (int)(maxMemUsed/meg);
	    String sfreep = "" + (100*stats[1]/stats[0]) + "% free";
	    String smaxp = "";
	    if (stats[2] > 0) 
	    	smaxp = ", " + (100*stats[0]/stats[2]) + 
		    "% of max allowed";	    	
	    return " Mem Used:"+stats[3]+" MB," + stats[0] + "MB allocated (" +
		sfreep + smaxp + ")";
	}	    

	// dump memory to stderr
	public static void dumpMemory() {
	    System.err.println("Memory: " + memoryMessage());
	}

	// verbose message
	public static boolean verbose;
	public static void verbose(String s) {
	    if (verbose) System.err.println(s);
	}

	//// String and numeric utilities

	// equivalent to Double.toString()
	public static final int DOUBLE_PRECISION = 19;
	// best match to Float.toString()
	public static final int SINGLE_PRECISION = 8;

	private static PrettyFormat doubleFormat;
	private static PrettyFormat singleFormat;

	// class initializer
	static {
	    doubleFormat = new PrettyFormat(DOUBLE_PRECISION); 
	    singleFormat = new PrettyFormat(SINGLE_PRECISION);
	}

	// convert String to important data types
	public static double toDouble(String s) {
	    try {
	    	return Double.valueOf(s).doubleValue();
	    } catch (NumberFormatException e) {
		return Double.NaN;
	    }
	}
	public static float toFloat(String s) {
	    try {
	    	return Float.valueOf(s).floatValue();
	    } catch (NumberFormatException e) {
		return Float.NaN;
	    }
	}
	public static int toInt(String s) throws Xcept {
	    try {
		return Integer.parseInt(s);
	    } catch (Exception e) {
		throw new Xcept("Could not convert \"" + 
		    s + "\" to a integer");
	    }
	}
	public static boolean toBoolean(String s) throws Xcept {
	    if (s.equals("true")) return true;
	    if (s.equals("false")) return false;
	    throw new Xcept("Illegal boolean value: " + s);
	}

	// round double to nearest delta
	public static double round(double d, double delta) {
	    if (delta <= 0) return d;
	    if (Double.isNaN(d)) return d;
	    long l = Math.round(d/delta);
	    if (l == Long.MAX_VALUE) return d;
	    return (float) l*delta;
	}

	// bound a double
	public static double bound(double value, double min, double
	max) {
	    if (Double.isNaN(value)) return min;
	    if (value < min) return min;
	    if (value > max) return max;
	    return value;
	}
	public static double bound(double value, double[] grid) {
	    return bound(value, grid[0], grid[grid.length-1]);
	}

	// number almost 1
	public static boolean nearlyZero(double d) {
	    double delta = 1e-6;
	    return (d < delta && d > 0-delta);
	}
 
	// arrays same?
	public static boolean isSame(double[] a, double[] b) {
	    if (a == null && b == null) return true;
	    if (a == null || b == null) return false;
	    if (a.length != b.length) return false;
	    for (int i=0; i<a.length; i++)
		if (a[i] != b[i]) return false;
	    return true;
	}

	// pretty numeric print
	public static String pretty(double d) {
	    return pretty(d, false);
	} 
	public static String pretty(double d, boolean dbl) {
	    if (dbl)
		return doubleFormat.format(d);
	    else
		return singleFormat.format(d);
	}
	public static String pretty(double[] d) {
	    if (d == null) return "null";
	    StringBuffer buf = new StringBuffer("[");
	    for (int i=0; i<d.length; i++) {
	    	if (i>0) buf.append(" ");
		buf.append(pretty(d[i]));
	    }
	    buf.append("]");
	    return buf.toString();
	}
	public static String pretty(int[] d) {
	    if (d == null) return "null";
	    StringBuffer buf = new StringBuffer("[");
	    for (int i=0; i<d.length; i++) {
	    	if (i>0) buf.append(" ");
		buf.append("" + d[i]);
	    }
	    buf.append("]");
	    return buf.toString();
	}


	// bump numeric suffix in string
	public static String bump(String s) throws Xcept {
	    if (Util.isBlank(s)) return "1";

	    // find start of trailing numeric tag
	    int tx = s.length();
	    while (tx>0 && Character.isDigit(s.charAt(tx-1)))
		tx--;
	    String base = s.substring(0,tx);
	    String sfx = s.substring(tx,s.length());
	    int sx = 0;
	    if (!Util.isBlank(sfx)) try {
		sx = Util.toInt(sfx);
	    } catch (Xcept x) { }
	    return base + (sx+1);
	}

	// name with bad chars removed
	//    because of legacy proj files
	//    must allow periods and numeric starting chars
	public static String safeName(String s) {
	    StringBuffer buf = new StringBuffer(s);
	    for (int i=0; i<buf.length(); i++) {
		char c = buf.charAt(i);
		boolean ok = c == '.' || Character.isLetterOrDigit(c);
		if (ok) continue;
		c = (i==0) ? 'y' : '_';
		buf.setCharAt(i, c);
	    }
	    return new String(buf);
	}

	// count occurances of char in String
	public static int count(String s, char c) {
	    int ct = 0;
	    for (int i=0; i<s.length(); i++) 
	    	if (s.charAt(i) == c) ct++;
	    return ct;
	}

	// is String blank
	public static boolean isBlank(String s) {
	    if (s == null) return true;
	    for (int i=0; i<s.length(); i++) 
		if (! Character.isWhitespace(s.charAt(i)))
		    return false;
	    return true;
	}

	// capitalize 1st character in string
	public static String capitalize(String s) {
	    if (s == null || s.length() < 1) return "";
	    StringBuffer buf = new StringBuffer(s);
	    char c = Character.toUpperCase(s.charAt(0));
	    buf.setCharAt(0, c);
	    return buf.toString();
	}

	// replace whitespace segments with single space
	public static String crushWhitespace(String text) {
	    text = text.trim();
	    StringBuffer buf = new StringBuffer();
	    boolean lastWhite = false;
	    for (int i=0; i<text.length(); i++) {
	    	char c = text.charAt(i);
		if (Character.isWhitespace(c)) {
		    if (! lastWhite) buf.append(' ');
		    lastWhite = true;
		} else {
		    buf.append(c);
		    lastWhite = false;
		}
	    }
	    return buf.toString();
	}

	// wrap lines longer than width on whitespace
	public static String wrap(String text, int width) {
	    StringBuffer buf = new StringBuffer(text);
	    int ct = 0;
	    for (int i=0; i<buf.length(); i++) {
	    	ct++;
		char c = buf.charAt(i);
		if (c == '\n') ct = 0;
		if (ct < width) continue;
		if (! Character.isWhitespace(c))
		    continue;
		buf.setCharAt(i, '\n');
		ct=0;
	    }
	    return buf.toString();
	}
		
	// check double arrays for NaNs
	public static boolean hasNaNs(double[] arr) {
	    if (arr == null) return false;
	    for (int i=0; i<arr.length; i++)
		if (Double.isNaN(arr[i])) return true;
	    return false;
	}

	// is this string double quoted
	public static boolean isQuoted(String s) {
	    if (s == null || s.length()<2) return false;
	    return s.charAt(0) == '"' &&
		s.charAt(s.length()-1) == '"';
	}

	// strip quotes
	public static String stripQuotes(String s) {
	    if (! isQuoted(s)) return s;
	    return s.substring(1, s.length()-1);
	}

	// only letters and digits?
	public static boolean onlyLettersAndDigits(String s) {
	    for (int i=0; i<s.length(); i++) 
		if (!Character.isLetterOrDigit(s.charAt(i)))
	    	    return false;
	    return true;
	}

	// remove CR characters from String
	public static String removeCR(String s) {
	    StringBuffer buf = new StringBuffer();
	    for (int i=0; i<s.length(); i++) {
		char c = s.charAt(i);
		if (c == '\r') continue;
		buf.append(c);
	    }
	    return buf.toString();
	}

	// remove trailing whitespace
	public static String removeTrailingWhitespace(String s) {
	    int i = s.length();
	    while (i>0 && Character.isWhitespace(s.charAt(i-1)))
		i--;
	    return s.substring(0,i);
	}

	// crop out middle of string if too long
	public static String midcrop(String s, int len) {
	    if (s.length() <= len) return s;
	    int l2a = len/2-1;
	    int l2b = l2a;
	    if (l2a + l2b + 2 < len) l2a++;
	    return s.substring(0,l2a) + ".." +
	    	s.substring(s.length()-l2b);
	}

	// time difference string
	public static String timeDiffStringOld(long diff) {
	    long[] arr = timeDiffArray(diff);
	    String ret = "";
	    if (arr[4] > 0) ret = ret + arr[4] + " days "; 
	    if (arr[3] > 0) ret = ret + arr[3] + " hours "; 
	    if (arr[2] > 0) ret = ret + arr[2] + " min "; 
	    if (arr[1] > 0) ret = ret + arr[1] + " sec "; 
	    if (arr[0] > 0) ret = ret + arr[0] + " msec "; 
	    return ret;
	}
	public static String timeDiffString(long diff) {
	    long[] arr = timeDiffArray(diff);
	    if (arr[0] >= 500) arr[1] += 1;
	    String ssec = "" + arr[1];
	    if (ssec.length() == 1) ssec = "0" + ssec;
	    String smin = "" + arr[2];
	    if (smin.length() == 1) smin = "0" + smin;
	    String shr = "" + arr[3];
	    if (shr.length() == 1) shr = "0" + shr;
	    String ret = shr + ":" + smin + ":" + ssec;
	    if (arr[4] > 0)
	    	ret = "" + arr[4] + " days " + ret;
  	    return ret;
	}
	 
	public static long[] timeDiffArray(long diff) {
	    long[] arr = new long[5];
	    arr[0] = (int) (diff % 1000); // msec
	    arr[1] = (int) (diff / 1000); // sec
	    arr[2] = 0;  // min
	    arr[3] = 0;  // hr
	    arr[4] = 0;  // days 
	    if (arr[1] > 60) {
		arr[2] = arr[1] / 60;
		arr[1] = arr[1] % 60;
	    } 
	    if (arr[2] > 60) {
		arr[3] = arr[2] / 60;
		arr[2] = arr[2] % 60;
	    } 
	    if (arr[3] > 24) {
		arr[4] = arr[3] / 24;
		arr[3] = arr[3] % 24;
	    }
	    return arr;
	}


	// encode String for java code generation
	public static String javaEncode(String s) {
	    if (s == null) return "null";
	    StringBuffer buf = new StringBuffer();
	    for (int i=0; i<s.length(); i++) {
		char c = s.charAt(i);
		switch (c) {
		case '"' : buf.append("\\\""); break;
		case '\\' : buf.append("\\\\"); break;
		default: buf.append(c); break;
		}
	    }
	    return buf.toString();
	}

	// decode String as java would
	public static String javaDecode(String s) {
	    s = s.replaceAll("\\\\n", "\n");
	    return s;
	}

	// set default locale
	public static void setDefaultLocale(String s) throws Xcept {
	    Locale[] locs = Locale.getAvailableLocales();
	    Locale loc = null;
	    for (int i=0; i<locs.length; i++) 
	        if (locs[i].toString().equals(s))
		    loc = locs[i];
	    if (loc == null) throw new Xcept(
	        "Locale " + s + 
		" not available on this computer.");
	    Locale.setDefault(loc);
	    verbose("Setting default locale to " + s);
	}

	// install named security manager
	public static void installSecurityMgr(String name) throws
	Xcept {
	    Util.verbose("Installing security manager " + name);
	    try {
	    	Class<?> clss = Class.forName(name);
		Constructor cons = clss.getConstructor(
		    new Class[] {} ); // no arguments constructor
		if (! SecurityManager.class.isAssignableFrom(clss))
		    throw new Xcept(name + 
		    	" is not a SecurityManager sub-class.");
		SecurityManager mgr = (SecurityManager) cons.newInstance(
		    new Object[] {});
		System.setSecurityManager(mgr);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// stack trace string
	public static String stackTraceString(Throwable t) {
	    StringWriter swrt = new StringWriter();
	    t.printStackTrace(new PrintWriter(swrt));
	    return swrt.toString();
	}

	// strip comment
	public static String stripComments(String text, String begin, String end) {
	    StringBuffer b = new StringBuffer(text);
	    while (true) {
	    	int i = b.indexOf(begin);
		if (i<0) break;
		int j = b.indexOf(end, i+begin.length());
		if (j<0) break;
//		System.err.println("found i=" + i + " j=" + j);
		b.replace(i, j+end.length(), "");
	    }
	    return b.toString();
	}

	// test userHome()
	public static void main(String[] args) throws Exception {
	    String s = userHome();
	    System.err.println("userHome() returns '" + s + "'");
	}	
}
