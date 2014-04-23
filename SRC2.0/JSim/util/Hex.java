/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Hex encoding and decoding

package JSim.util;

import java.util.StringTokenizer;
import java.io.*;

public class Hex {

	//// base-10 (standard) numeric routines

	// base-10 encoding
	public final static String encode10(double[] data, boolean single)
	throws Xcept {
	    StringBuffer buf = new StringBuffer(
		data.length*(single?8:18));
	    for (int i=0; i<data.length; i++) {
		if (i%8 == 0 && i>0) buf.append('\n');
		if (single) 
		    buf.append(" " + ((float) data[i]));
		else 
		    buf.append(" " + data[i]);
	    }
	    return buf.toString();
	}

	// base-10 decoding
	public final static void decode10(String s, double[] data)
	throws Xcept {
	    StringTokenizer stok = new StringTokenizer(s);
	    for (int i=0; i<data.length; i++) {
		String tok = stok.nextToken();
		if (tok == null) throw new Xcept(
		    "Data set underflow during base-10 decode");
	        data[i] = Util.toDouble(tok);
	    }
	    if (stok.hasMoreTokens()) throw new Xcept(
		"Data set overflow during base-10 decode");
	}

	//// 4 bits/byte routines

	// encode double[]
	public final static String encode4(double[] data, boolean single)
	throws Xcept {
	    int bct = single ? 4 : 8;
	    StringBuffer buf = new StringBuffer(data.length*bct);
	    for (int i=0; i<data.length; i++) {
		if (i%8 == 0 && i>0) buf.append('\n');
		String s = single ?
		    Integer.toHexString(Float.floatToRawIntBits((float) data[i])) :
		    Long.toHexString(Double.doubleToRawLongBits(data[i]));
		buf.append(" " + s);
	    }
	    return buf.toString();
	}

	// decode double[]
	public final static void decode4(String s, boolean single, double[] data)
	throws Exception {
	    StringTokenizer stok = new StringTokenizer(s);
	    for (int i=0; i<data.length; i++) {
		String tok = stok.nextToken();
		if (tok == null) throw new Xcept(
		    "Data set underflow during base-4 decode");
//	        try {
		    data[i] = single ?
			Float.intBitsToFloat(Integer.parseInt(tok, 16)) :
			Double.longBitsToDouble(Long.parseLong(tok, 16));
//		} catch (NumberFormatException e) {
//		    throw Xcept.wrap(e);
//		}
	    }
	    if (stok.hasMoreTokens()) throw new Xcept(
		"Data set overflow during base-4 decode");
	}


	// encode 4 bits/type
	public final static String encode4(byte[] bytes) {
	    StringBuffer s = new StringBuffer(bytes.length*2);
	    for (int i=0; i<bytes.length; i++) {
		if (i>0 && (i%32) == 0) s.append("\n");
		int j = bytes[i] & 0xff;
		String s0 = Integer.toHexString(j);
	 	if (s0.length() == 1) s0 = "0" + s0;
		s.append(s0);
	    }
	    return s.toString();
	}	

	// decode 4 bits/byte
	public final static byte[] decode4(String s) throws Xcept {
	    ByteArrayOutputStream bstr = 
		new ByteArrayOutputStream();
	    for (int i=0; i<s.length(); i++) {
		char c1 = s.charAt(i);
		if (Character.isWhitespace(c1)) continue;
		i++;
		if (i>=s.length()) throw new Xcept(
		     "Error decoding project image data");
		char c2 = s.charAt(i);
		int j = decode4(c1)*16 + decode4(c2);
		byte b = (byte) (j & 255);
		bstr.write(b);
	    }
	    return bstr.toByteArray();
	}

	// decode 1 char @ 4 bits/byte
	private final static int decode4(char c) throws Xcept {
	    if (Character.isDigit(c))
		return c-'0';
	    if (c<'a' || c>'f') throw new Xcept(
		"Illegal hex digit '" + c + "'");
	    return c-'a' + 10;
	}

	//// test program
	public static final void main(String[] args) throws Exception {

	    // command line parsing
	    if (args.length != 3) throw new Xcept(
		"Usage: Hex ct encoding single");
	    int ct = Util.toInt(args[0]);
	    int encoding = Util.toInt(args[1]);
	    boolean single;
	    if (args[2].equals("t"))
		single = true;
	    else if (args[2].equals("f"))
		single = false;
	    else 
		throw new Xcept("single arg must be t or f");

	    // create data
	    double[] data = new double[ct];
	    for (int i=0; i<ct; i++) {
		data[i] = Math.exp(30*(Math.random()-0.5));
		if (Math.random()<0.5) data[i] *= -1;
	    }

	    // encode data
	    System.err.println("start encoding");
	    long t = System.currentTimeMillis();
	    String s;
	    switch (encoding) {
	    case 10:
	        s = encode10(data, single);
		break;
	    case 4:
		s = encode4(data, single);
		break;
	    default:
		throw new Xcept("Illegal encoding");
	    }
	    System.err.println("encoding in " + 
		(System.currentTimeMillis()-t) + " msec");
	    System.err.println("s=" + s);
	    System.err.println("s.length() = " + s.length());

	    // decode data
	    t = System.currentTimeMillis();
	    double[] rdata = new double[ct];
	    switch (encoding) {
	    case 10:
	        decode10(s, rdata);
		break;
	    case 4:
		decode4(s, single, rdata);
		break;
	    }
	    System.err.println("decoding in " + 
		(System.currentTimeMillis()-t) + " msec");
	    
	    // compare data
	    for (int i=0; i<ct; i++) {
		if (Math.abs(rdata[i]-data[i]) <=
		    Math.abs(data[i]) * 1e-6) continue;
		System.err.println(" " + data[i] + " " + rdata[i]);
	    }
	}
}

