/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataWriter for I4 Bullseye format

package JSim.data;

import JSim.util.*;
import JSim.expr.*;
import java.io.*;
import java.net.URL;

public class I4BullDataWriter extends DataWriter {

	// private state
	private PrintWriter writer;
	
	// constructor
	public I4BullDataWriter(DataFormat f) { 
	    super(f);
	}

	// write data
	public void writeData(Writer wrt, Data.List dlist) throws Xcept {

	    // internal bullseye data structure
	    Data.List plist = new Data.List(dlist.size());
	    GridData s = null, z = null, t = null;
	    int tct = 0, dim = 0;
	    RealNData angles = null;

	    // load plist, angles
	    for (int i=0; i<dlist.size(); i++) {
		Data d = dlist.data(i);
		
		// special case for bull structural data
		if (d instanceof RealNData && d.desc().equals("angles")) {
		    angles = (RealNData) d;
		    continue;
		}

		// 1st time parameter determines dimensions
		if (dim == 0) {
		    dim = d.ndim();
		    if (dim < 2 || dim > 3) throw new Xcept(
		    	d, "Invalid dimension for I4 Bullseye data");
		    s = d.grid(0);
		    z = d.grid(1);
		    if (dim == 3) {
			t = d.grid(2);
		        tct = t.ct();
		    } else {
			tct = 1;
		    }
		}

		// check for all parameters
		boolean ok = d.ndim() == dim 
		    && s.sameSamplesAs(d.grid(0))
		    && z.sameSamplesAs(d.grid(1));
		if (dim == 3)
		    ok = ok && t.sameSamplesAs(d.grid(2));
		if (!ok) throw new Xcept(plist.data(0), d,
		    "Inconsistent dimensions between output bullseye parameters");		
		plist.add(d);
	    }

	    // any parameters?
	    if (plist.size() == 0) throw new Xcept(this,
		"No 2D or 3D bullseye parameters specified for output");

	    // default angles
	    if (angles == null) {
		angles = new RealNData("angles", null, 
		    new GridData[] { z });
		for (int i=0; i<z.ct(); i++)
		    angles.set(i, 90);
	    }
	    if (angles.nsamples() != z.ct()) throw new Xcept(
		z, angles, "Mismatched angles dimension for Bullseye output");
	    
	    // write header line
	    writer = new PrintWriter(wrt);
	    writer.println("I4bull1.0");
	    writer.println("");
	    writer.println("sztp\t" + s.ct() + "\t" + z.ct() +
		"\t" + tct + "\t" + plist.size());
	    writer.println("format\tascii");
	    int zofs = (int) (z.min()-1);
	    if (zofs < 0) zofs = 0;

	    // timebins
	    for (int i=0; i<tct; i++) {
		String hdr = (i==0) ? "timebin" : "";
		double tstart = 0, tstop = 1;
		if (t != null) {
		    tstart = t.realVal(i);
		    if (i < tct-1)
			tstop = t.realVal(i+1);
		    else 
			tstop = 2*t.realVal(i) - t.realVal(i-1);
		}
		writer.println(hdr + "\t" + pretty(tstart)
		    + "\t" + pretty(tstop));
	    }

	    // parms
	    for (int i=0; i<plist.size(); i++) {
		Data p = plist.data(i);
		Unit u = p.unit();
		if (u == null) u = Unit.scalar();
		String hdr = (i==0) ? "parm" : "";
		writer.println(hdr + "\t\"" + p.desc()
		    + "\"\t\"" + u.pubName() + "\"");
	    }

	    // zoffset, angles
	    writer.println("zoffset\t" + zofs);
	    for (int i=0; i<z.ct(); i++) {
		String hdr = (i==0) ? "angles" : "";
		writer.println(hdr + "\t" + pretty(angles.realVal(i)));
	    }

	    // data array
	    int[] gpos = new int[dim];
	    writer.println("data");
	    for (int px=0; px<plist.size(); px++) {
		RealNData p = (RealNData) plist.data(px);
		for (int tx=0; tx<tct; tx++) {
		    if (dim > 2) gpos[2] = tx;
		    writer.println("\n# t=" + (tx+1) + " p=" + (px+1));
		    for (int sx=0; sx<s.ct(); sx++) {
			gpos[0] = sx;
			for (int zx=0; zx<z.ct(); zx++) {
			    gpos[1] = zx;
			    double d = p.realVal(gpos);
			    writer.print("\t" + pretty(d));
			}
			writer.println("");
		    }
		}
	    }
	}

}
