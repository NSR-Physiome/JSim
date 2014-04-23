/*NSRCOPYRIGHT
  Copyright (C) 1999-2011 University of Washington
  Developed by the National Simulation Resource
  Department of Bioengineering,  Box 355061
  University of Washington, Seattle, WA 98195-5061.
  Dr. J. B. Bassingthwaighte, Director
  END_NSRCOPYRIGHT*/

// Check text to see if looks like Antimony model

package JSim.data;

import JSim.util.*;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class AntimonyCheck {    
    protected String mPotentialModel;

    // constructor
    public AntimonyCheck(String text) throws Xcept {
        mPotentialModel = text;
    }

    public boolean isProbablyAntimony() {
        String text = new String(mPotentialModel);
        text = "\n" + text + "\n";
        double antimonyish = 0;
        double nomatch = 0;

        // Comments of the form /* ... */
        Pattern pattern = Pattern.compile("/\\*.*?\\*/", Pattern.DOTALL);
        Matcher match = pattern.matcher(text);
        text = match.replaceAll("");

        String[] lines = text.split("(\r\n)|\r|\n|\u0085|\u2028|\u2029");
        for (int l=0; l<lines.length; l++) {
            String line = lines[l];
            //System.out.println("--|" + line + "|--");
            pattern = Pattern.compile("\\/\\/.*$");
            match = pattern.matcher(line);
            if (match.find()) {
                line = match.replaceFirst("");
            }
            //Blank lines do not count one way or another towards the antimony vs. not count (even if they were only comments).
            if (Pattern.matches("^\\s*$", line)) {
                continue;
            }
            //Remove leading and trailing whitespace
            line = line.replaceFirst("^\\s*", "");
            line = line.replaceFirst("\\s*$", "");

            //model or function
            if (Pattern.matches("^((model)|(module)|(function))\\s+\\S+(\\(.*\\))?\\s*$", line)) {
                //System.out.println("line defines a model: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //end
            if (Pattern.matches("^end$", line)) {
                //System.out.println("line defines the end of a model: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //A reaction
            if (Pattern.matches(".*->.*;.*", line)) {
                //System.out.println("line defines a reaction: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //An assignment
            if (Pattern.matches("[A-Za-z_$][A-Za-z0-9_]*'?\\s*:?=.*", line)) {
                //System.out.println("line defines an assignment: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //Some sort of definition
            if (Pattern.matches("[A-Za-z_$][A-Za-z0-9_]*:.*", line)) {
                //System.out.println("line has a definition: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //One of my 'is' lines.
            if (Pattern.matches("[A-Za-z_$][A-Za-z0-9_]*\\s+is\\s+[A-Za-z_$][A-Za-z0-9_]*", line)) {
                //System.out.println("line has a definition: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //An 'is' line to a quoted name
            if (Pattern.matches("[A-Za-z_$][A-Za-z0-9_]*\\s+is\\s+\".*\";?", line)) {
                //System.out.println("line has a definition: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //Starts with an antimony keyword and then only words and commas.
            if (Pattern.matches("((species)|(reaction)|(formula)|(DNA)|(gene)|(operator)|(compartment)|(var)|(const))\\s+[A-Za-z_$][A-Za-z0-9_,$\\s]*;?", line)) {
                //System.out.println("line starts with an antimony keyword: --|" + line + "|--");
                antimonyish++;
                continue;
            }
            //System.out.println("No match: --|" + line + "|--");
            nomatch++;
        }
        //System.out.println("Total number of antimonyish lines: " + antimonyish);
        //System.out.println("Total number of unmatched lines: " + nomatch);
        if (antimonyish + nomatch == 0) return false; //It's nothing but blank lines.
        if (antimonyish/(antimonyish+nomatch) > .3) return true;
        return false;
    }

    // mainline test
    public static final void main(String[] args) throws Exception {
        for (int i=0; i<args.length; i++) {
            File f = new File(args[i]);
            String text = UtilIO.readText(f);
            AntimonyCheck sbcheck = new AntimonyCheck(text);
            String output;
            output = "the file " + f.getName();
            if (sbcheck.isProbablyAntimony()) {
                output = "YES:  " + output + " is probably an Antimony file.";
            }
            else {
                output = "NO:  " + output +  " is probably not an Antimony file.";                
            }
            System.out.println(output);
        }
    }
}

