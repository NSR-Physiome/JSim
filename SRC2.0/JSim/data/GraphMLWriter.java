/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// write 1 Graph from an GraphML document

package JSim.data;

import java.io.*;
import java.util.*;
import org.w3c.dom.*;

import JSim.util.*;

public class GraphMLWriter extends XMLWriter {
	private String graphID;

	// constructor
	public GraphMLWriter() { super(); }
	
	// write to String
	public String writeString(Document doc, String graphID)
	throws Xcept {
	    this.graphID = graphID;
	    return super.writeString(doc);
	}
	
	// write 1 node
	//   special case document node
	public void writeNode(Node node, Writer writer, 
    	String indentLevel) throws IOException {

	    // if not graphml top element, use super class 
	    if (node.getNodeType() != Node.ELEMENT_NODE
	    || ! node.getNodeName().equals("graphml")) {
	    	super.writeNode(node, writer, indentLevel);
		return;
	    }
	    
            NodeList nodes = node.getChildNodes();
            if (nodes == null) throw new IOException(
	    	"Empty GraphML document");
	    ArrayList<Node> children = new ArrayList<Node>();
            for (int i=0; i<nodes.getLength(); i++) {
	    	Node n = nodes.item(i);
		if (! (n instanceof Element)) continue;
		Element e = (Element) n;
		if (! e.getNodeName().equals("graph")) continue;
		if (! e.getAttribute("id").equals(graphID)) continue;
 		children.add(n);
	    }
	    if (children.isEmpty()) throw new IOException(
	    	"GraphID=" + graphID + 
		" not found in GraphML document");
	    writeElement(node, writer, "", children);
	}

	// test harness
	public static final void main(String[] args) 
	throws Exception {
	    File f  = new File(args[0]);
	    String graphID = args[1];
	    Document doc = UtilXML.parse(f);
	    GraphMLWriter wrt = new GraphMLWriter();
	    String out = wrt.writeString(doc, graphID);
	    System.out.println(out);
	}
}

