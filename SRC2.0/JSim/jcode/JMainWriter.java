// write java code for model mainline
 
package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

public class JMainWriter extends JMethodWriter {
	protected JPlanWriter planWriter;

	// constructor
	public JMainWriter(JPlanWriter planWriter, SeqBlock block) 
	throws Xcept {
	    super(null, block);
	    this.planWriter = planWriter;
	    Iterator<JCacheVar> cvs = planWriter.cacheVars.iterator();
	    while (cvs.hasNext()) {
	    	JCacheVar cv = cvs.next();
		cache.add(cv.v);
	    }
	    addSubWriterBlock(block);
	}

	// plan writer
	public JPlanWriter planWriter() { return planWriter; }

	// write mainline method
	public void writeMethods() throws Xcept {
	    println("// ctxt mainline");
	    startMethod("mainline", "", "");
	    writeBlock((SeqBlock) item);
	    stopMethod();
	    println("");
	}
}

