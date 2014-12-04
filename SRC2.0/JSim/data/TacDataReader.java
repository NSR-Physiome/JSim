/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// DataReader for TacDataFormat

// This routine was originally written by ZL as a quick & dirty
// expedient.  EB cleaned it up somewhat,  but a complete
// rewrite is really in order.  Numerous errors,  including
// NullPointerExceptions occur reading some existing TAC
// files.  A complete rewrite should address inconsistencies
// between the TAC standard document,  the NSR checktac 
// program and existing TAC files at NSR.  StreamTokenizer
// methods/fields need to be wrapped to support comment
// lines, blank lines and peek ahead in a readable fashion.
// The current spaghetti code is hard to follow and 
// probably contains many more bugs
// than simple comparison against existing files can reveal.

package JSim.data;

import java.util.*;
import java.io.*;
import java.text.*;

import JSim.util.*;
import JSim.expr.*;

public class TacDataReader extends TextDataReader {

	// read state variables
	private StreamTokenizer inp;
	private int fieldCount;   
  	private int rowCount;
  	private int quoteChar = '\''; // by single quote
  	private int delimChar = -4;  // by whitespace
	private int curr_run;	// current run
	private int curr_type;	// current type
	private TacData tacData; // data under construction
 
	// constructor
	public TacDataReader(TacDataFormat f, Reader r) 
	throws Xcept {
	    super(f, r);
	}

	// read data
	public Data.List readData() throws Xcept {
	    try {
    		inp = new StreamTokenizer(rdr());
		inp.resetSyntax();
      		inp.eolIsSignificant(true);
		tacData = new TacData();
 		retrieveData();
		return tacData;
   	    } catch(IOException e) {
  		throw new Xcept(this, "IO error reading TAC data");
       	    }
	}

	// parse Tac data from stream
  	private void retrieveData() throws Xcept, IOException {
	    fieldCount = 0;
	    inp.whitespaceChars(0,' ');
	    inp.wordChars('\u0021','\u00ff');
	    inp.quoteChar(quoteChar);

	    if (delimChar != -4) {
	      	inp.ordinaryChar(delimChar);
	      	inp.wordChars(' ', ' '); // spaces are now part of the word, not token separators
	    }

	    // check headers
	    getAllTokensInLine("TAC file name", false);
	    if (getAllTokensInLine("experiment type:", true)) {
	      	getAllTokensInLine("experiment description:", false);
	      	getAllTokensInLine("experiment name:", false);
	    }

	    // aux data
	    String token = getNextTokenInLine("# of auxiliary data:");
	    if (token != null) {
	      	int naux = Integer.parseInt(token);
	      	for (int i = 0; i < naux; i++) {
		    String key = peek();
		    if (key.equals("FIM_FORMAT")) 
			loadFimFormat(getNextTokenInLine(key, true));
		    else
		    	getAllTokensInLine(null, false);
		}
	    }

	    // loop over runs
	    token = getNextTokenInLine("# of runs:");
	    int nrun = Integer.parseInt(token);
	    for (int i = 0; i < nrun; i++) {
		tacData.addRun();	
		curr_run = i;
		curr_type = 0;
 	      	getAllTokensInLine("run description:", false);

		// run aux data
	        token = getNextTokenInLine("# of auxiliary data:", true);
	      	if (token != null) {
	            int naux = Integer.parseInt(token);
	            for (int j = 0; j < naux; j++) 
	          	getAllTokensInLine(null, false);
	      	}

		// load data sets
		//    this routine loads some "lost" curves
		//    files that pass the NSR checktac program
		while (true) { 
		    String p = peek();
		    if (p == null) break;
		    if (p.equals("# of physiol datasets:")) {
			curr_type = TacData.PHYSIOL;
 		        getDataTokens("physiol", i+1);
		    } else if (p.equals("# of input function datasets:")) {
			curr_type = TacData.INPUT;
 		        getDataTokens("input function", i+1);
		    } else if (p.equals("# of sample datasets:")) {
			curr_type = TacData.SAMPLE;
 		        getDataTokens("sample", i+1);
		    } else if (p.equals("run description:")) {
			break;
		    } else {
			throw new Xcept(
			    "Unexpected token \"" + p + "\"");
		    }
		}
	    }
 	}

	// load FIM_FORMAT auxiliary data
	private void loadFimFormat(String fmt) throws Xcept {
	    StringTokenizer stok = new StringTokenizer(fmt);
	    String tok = stok.nextToken();
	    if (! tok.startsWith("BULLSEYE")) return;
	    tok = stok.nextToken(); 
	    if (tok == null) return;
	    tacData.addAuxValue("s.ct", Util.toInt(tok));
	    tok = stok.nextToken(); 
	    if (tok == null) return;
	    tacData.addAuxValue("z.ct", Util.toInt(tok));
	    tok = stok.nextToken(); 
	    if (tok == null) return;
	    tacData.addAuxValue("z.offset", Util.toInt(tok));
	}

	// read one row of data
  	private void readDataRow() throws Xcept, IOException {
	    int currentField = 0;

	    //read a number, put it into data construct
	    while (inp.ttype != inp.TT_EOL && inp.ttype != inp.TT_EOF) {
	        if (currentField != 0 && inp.ttype == delimChar) inp.nextToken();
	        if (inp.ttype == delimChar || inp.ttype == inp.TT_EOL) {
	            if (currentField >= fieldCount) throw new Xcept(this, "Too many fields");
		    currentField++;
      		} else {
    		    if (inp.ttype != inp.TT_WORD && inp.ttype != quoteChar) 
			throw new Xcept(this, "Wrong field type");
    		    if (currentField >= fieldCount) 
			throw new Xcept(this, "Too many fields");
		    currentField++;
	            inp.nextToken();
      		}
    	    }

	    // check for too few values on a row
	    if (currentField < fieldCount) throw new Xcept(this, "Too few fields");
  	}

	// check tokens in the same line
	private boolean missedLastTime = false;

	private String getNextTokenInLine(String head) throws Xcept, IOException {
	    return getNextTokenInLine(head, false);
	}

  	// read next token in line
	private String getNextTokenInLine(String head, boolean optional)
    	throws Xcept, IOException {
	    String result = null;

	    // check missed token ???
	    if (!missedLastTime)  inp.nextToken();
	    if (!inp.sval.startsWith(head)) {
	        missedLastTime = true;
	    	if (!optional)
        	    throw new Xcept ("Expected token \"" 
			+ head + "\" found \"" + 
			inp.sval + "\"");
      	    	return result;
    	    }
    	    missedLastTime = false;

	    inp.nextToken();
	    if (inp.ttype != inp.TT_EOL && inp.ttype != inp.TT_EOF)
	      	result = inp.sval;

	    while (inp.ttype != inp.TT_EOL && inp.ttype != inp.TT_EOF)
	        inp.nextToken();

	    return result;
  	}

	// peek next token
	private String peek() throws IOException {
	    if (missedLastTime) return inp.sval;
	    inp.nextToken();
	    String ret = inp.sval;
	    inp.pushBack();
	    return ret;
	}

	// all tokens in a line
	private boolean getAllTokensInLine(String head, boolean optional)
    	throws Xcept, IOException {
	    String result = null;
	    if (!missedLastTime) inp.nextToken();

	    // check missed token???
	    if (head != null) {
  		if (!inp.sval.startsWith(head)) {
        	    missedLastTime = true;
        	    if (!optional)
          		throw new Xcept(this, "Expected \"" + head + 
			    "\" found \"" + inp.sval + "\"");
 		    return false;
      		}
	    }
	    missedLastTime = false;

	    result = inp.sval;

	    while (inp.ttype != inp.TT_EOL && inp.ttype != inp.TT_EOF) {
	        inp.nextToken();
      		if (inp.ttype != inp.TT_EOL && inp.ttype != inp.TT_EOF)
        	    result += " " + inp.sval;
    	    }
	    return true;
  	}

	// read some # of data sets,  return whether # datasets token found
  	private boolean getDataTokens(String head, int runNumber)
        throws Xcept, IOException {
	    String type = head.substring(0,1);

	    String token = getNextTokenInLine(
		"# of " + head + " datasets:");
	    if (token == null) return false;
	    int ninput = Integer.parseInt(token);
	    int ns = 0;

	    while (ns < ninput) {
	        getAllTokensInLine(head + " dataset description:", false);
	        token = getNextTokenInLine("# of lines:");
	        int nline = Integer.parseInt(token);
  	        token = getNextTokenInLine("# of fields:");
	        int nfield = Integer.parseInt(token);
		IrregularGridData grid = 
		    new IrregularGridData("dummy", Unit.scalar());
		String[] datanames = new String[nfield];
		double[][] datavals = new double[nfield][nline];

	    	for (int k2 = 0; k2 < nfield; k2++) {
	            inp.nextToken();
	            if (k2 == 0)  
			grid.setDesc(inp.sval);
	            else 
		    	datanames[k2] = inp.sval.replace('(', '_').replace(
			    ')', '_').replace('.', '_');
      	    	}

      	    	inp.nextToken();
      	    	int inx = 0;
      	    	for (int k = 0; k < nline; k++) {
        	    for (int k2 = 0; k2 < nfield; k2++) {
          	    	inp.nextToken();
			if (inp.sval == null) throw new Xcept(this,
			    "Unexpected end-of-file");
          	    	if (k2 == 0) 
            		    inx = grid.addPoint(Util.toDouble(inp.sval));
          	    	else
            		    datavals[k2][k] = Util.toDouble(inp.sval);
          	    }
        	    if (inp.ttype != inp.TT_EOL || inp.ttype != inp.TT_EOF) inp.nextToken();
      	    	}

		for (int i=1; i<nfield; i++) {
		    RealNData curve = new RealNData(datanames[i], 
			Unit.scalar(), new GridData[] { grid });
		    curve.set(datavals[i]);
		    tacData.addCurve(curve, curr_run, curr_type);
		}
      	    	ns += nfield - 1;
    	    }

	    return true;
  	}

}
