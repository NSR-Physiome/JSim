/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.util;

import java.io.*;
import java.util.StringTokenizer;
import java.rmi.RemoteException;
import java.rmi.ServerException;
import java.lang.reflect.*;

// Exception for JSim
public class Xcept extends Exception {	
	public DiagInfo dinfo1, dinfo2;
	public XceptPos pos;	// position info, if any
	public Throwable wrapped; // wrapped Exception/other

	// constructors
	public Xcept(String msg) { 
	    super(msg);
	}
	public Xcept(String msg, XceptPos p) { 
	    super(msg);
	    pos = p;
	}
	public Xcept(DiagInfo d, String msg) {
	    super(msg);
	    dinfo1 = d;
	}
	public Xcept(DiagInfo d, String msg, XceptPos p) {
	    super(msg);
	    dinfo1 = d;
	    pos = p;
	}
	public Xcept(DiagInfo d1, DiagInfo d2, String msg) {
	    super(msg);
	    dinfo1 = d1;
	    dinfo2 = d2;
	}
	public Xcept(Throwable w, String msg) { 
	    super(msg);
	    wrapped = w;
	}
	public Xcept(Throwable w) { 
	    this(w, getMessage(w));
	}

	private static String getMessage(Throwable w) {
	    String msg = w.getMessage();
	    if (w instanceof ArrayIndexOutOfBoundsException)
	        msg = "Array index exception: " + msg;
	    return msg;
	}

	// String rep
    	public String getMessage() {
	    String s = "";
	    if (pos != null) s = s + pos + " ";
	    if (dinfo1 != null) s = s + dinfo1.diagInfo() + "\n";
	    if (dinfo2 != null) s = s + dinfo2.diagInfo() + "\n";
	    if (super.getMessage() != null)
	    	s = s + super.getMessage() + "\n";
	    return s;
	}

	// clean message (without exception name)
	public String cleanMessage() {
	    String s = getMessage();
	    StringTokenizer stok = new StringTokenizer(s);
	    String ret = null;
	    while (stok.hasMoreTokens()) {
		String tok = stok.nextToken();
		if (tok.indexOf("Xcept") >= 0) continue;
		if (ret == null) 
		    ret = tok;
		else
		    ret = ret + " " + tok;
	    }
	    return (ret == null) ? "" : ret;
	}

	// wrap any Exception in Xcept
	public static Xcept wrap(Throwable e) {
	    if (e == null)
		return new Xcept("<null> Exception");
	    if (e instanceof ServerException 
	    && ((ServerException) e).getCause() != null)
	    	e = ((ServerException) e).getCause();
	    if (e instanceof Xcept)
		return (Xcept) e;
	    if (e instanceof RemoteException) 
		return new Xcept(e);
	    if (e instanceof InvocationTargetException)
	        return wrap(
		    ((InvocationTargetException) e).getTargetException()); 
	    if (! Util.isBlank(e.getMessage()))
	    	return new Xcept(e);
	    Class c = e.getClass();
	    if (c != null)
		return new Xcept(e, "wrapped " + c);
	    return new Xcept(e, "Informationless Exception");
	}

	// print stack trace
	public void printStackTrace(PrintStream out) {
	    if (wrapped != null)
		wrapped.printStackTrace(out);
	    super.printStackTrace(out);
	}

	// clean message for any Throwable
	public static String cleanMessage(Throwable e) {
	    return (e instanceof Xcept) ?
	    	((Xcept) e).cleanMessage() : e.getMessage();
	}

	// stack trace string
	public static String stackTraceString(Throwable e) {
	    StringWriter wrt = new StringWriter();
	    e.printStackTrace(new PrintWriter(wrt));
	    return wrt.toString();
	}
}

