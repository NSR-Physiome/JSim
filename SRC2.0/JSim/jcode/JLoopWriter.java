// write java code for domain loop
 
package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

public class JLoopWriter extends JMethodWriter {
	protected SeqBlock.Loop block;
	protected Integer phase; // if loop is a mainline phase

	// constructor
	public JLoopWriter(JMethodWriter parent, 
	SeqBlock.Loop block, int loopInx) throws Xcept {
	    super(parent, block);
	    this.block = block;
	    id = "" + loopInx + xname();
	    if (parent.id != null)
	        id = parent.id + "__" + id;
	    addSubWriterBlock(block);
	    phase = planWriter().loopPhases.get(block);
	}

	// write call to loop
	public void writeLoopsCall() throws Xcept {
	    String sv = vstruct(x());
	    if (phase != null)
	    	println("startPhase(" + phase.intValue() + ", "
		    + sv + ");");
	    println("for (setLHBC(" + sv + "); isSet(" + sv
	        + "); moveRight(" + sv + ")) "
		+ methodName() + "();");
	    if (phase != null)
	    	println("startPhase(" + 
		    (phase.intValue()+1) + ", null);");
	}

	// write mainline method
	public void writeMethods() throws Xcept {
	    println("// ctxt loop iteration for domain " + x());
	    startMethod(methodName(), "", "");
	    writeBlock(block);
	    if (phase != null) {
	    	String sv = vstruct(x());
	    	println("updatePhase(" + sv + ");");
	    }
	    stopMethod();
	    println("");
	}

	// query
	public Domain x() { return block.x(); }  
	public String xname() { return vname(x()); }
	public String methodName() { return "loop__" + id; }
}

