/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// write XML file from DOM structures

package JSim.data;

import java.io.*;
import java.util.*;
import org.w3c.dom.*;

import JSim.util.*;

public class XMLWriter {

	// private state
    	private String indent;
    	protected String lineSeparator;

	// header
	public static final String XMLHDR = 
	    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

    	// constructor
    	public XMLWriter() {
            indent = "  ";
            lineSeparator = "\n";
    	}

	// write to doc file
	public void write(Document doc, File file) 
	throws Xcept {
   	    try {
	    	write(doc, new FileWriter(file));
	    } catch (IOException x) {
	    	throw Xcept.wrap(x);
	    }
	}

	// write to doc stream
    	public void write(Document doc, OutputStream out)
        throws Xcept {
            Writer writer = new OutputStreamWriter(out);
            write(doc, writer);	
	}

	// write element to stream
    	public void write(Element e, OutputStream out)
        throws Xcept {
   	    try {
          	Writer writer = new OutputStreamWriter(out);
            	writer.write(XMLHDR);
            	writer.write(lineSeparator);
            	writeNode(e, writer, "");
	    	writer.flush();	
	    } catch (IOException x) {
	    	throw Xcept.wrap(x);
	    }
	}

	// write String
	public String writeString(Document doc) throws Xcept {
	    StringWriter swrt = new StringWriter();
	    write(doc, swrt);
	    return swrt.toString();
	}

	// write to doc Writer
    	public void write(Document doc, Writer writer)
    	throws Xcept {
 	    try {
            	writeNode(doc, writer, "");
            	writer.flush();
	    } catch (IOException e) {
	    	throw Xcept.wrap(e);
	    }
    	}

    	// write 1 node
    	protected void writeNode(Node node, Writer writer, 
    	String indentLevel) throws IOException {

            // Determine action based on node type
            switch (node.getNodeType()) {
            case Node.DOCUMENT_NODE:
                writer.write(XMLHDR);
                writer.write(lineSeparator);
                NodeList nodes = node.getChildNodes();
                if (nodes != null) 
                    for (int i=0; i<nodes.getLength(); i++) 
                        writeNode(nodes.item(i), writer, "");
                break;
            
            case Node.ELEMENT_NODE:
		ArrayList<Node> children = UtilXML.getNodes(node);
	        writeElement(node, writer, indentLevel, children);
                break;
            
            case Node.TEXT_NODE:
                writer.write(safe(node.getNodeValue()));
                break;

            case Node.CDATA_SECTION_NODE:
                writer.write("<![CDATA[" +
                             node.getNodeValue() + "]]>");
                break;

            case Node.COMMENT_NODE:
                writer.write(indentLevel + "<!-- " +
                             node.getNodeValue() + " -->");
                writer.write(lineSeparator);
                break;
            
            case Node.PROCESSING_INSTRUCTION_NODE:
                writer.write("<?" + node.getNodeName() +
                             " " + node.getNodeValue() +
                             "?>");                
                writer.write(lineSeparator);
                break;
            
            case Node.ENTITY_REFERENCE_NODE:
                writer.write("&" + node.getNodeName() + ";");    
                break;
                
            case Node.DOCUMENT_TYPE_NODE: 
                DocumentType docType = (DocumentType)node;
                writer.write("<!DOCTYPE " + docType.getName());
                if (docType.getPublicId() != null)  
                    System.out.print(" PUBLIC \"" + 
                        docType.getPublicId() + "\" ");                    
                else 
                    writer.write(" SYSTEM ");
                writer.write("\"" + docType.getSystemId() + "\">");                                
                writer.write(lineSeparator);
                break;                
	    }        
    	}

	// write Element with children
	protected void writeElement(Node node,  Writer writer, 
    	String indentLevel, ArrayList<Node> children) throws IOException {
              	String name = node.getNodeName();
                writer.write(indentLevel + "<" + name);
                NamedNodeMap attributes = node.getAttributes();
                for (int i=0; i<attributes.getLength(); i++) {
                    Node current = attributes.item(i);
                    writer.write(" " + current.getNodeName() +
                                 "=\"" + safe(current.getNodeValue()) +
                                 "\"");
                }

		// use long form?
		boolean longForm = false;
		boolean addLineSep = true;
		for (int i=0; i<children.size(); i++) {
		    int typ = children.get(i).getNodeType();
		    if (typ != Node.ATTRIBUTE_NODE) longForm = true;
		    if (typ == Node.TEXT_NODE) addLineSep = false;
		}
		if (! longForm) {
		    writer.write("/>");
                    writer.write(lineSeparator);
		    return;
		}
		writer.write(">");
                if (addLineSep) writer.write(lineSeparator);
               
                // write kids
		boolean textSpacing = false;
                for (int i=0; i<children.size(); i++) {
		    Node n = children.get(i);                       
                    writeNode(n, writer,
                            indentLevel + indent);
		    if (n.getNodeType() == Node.TEXT_NODE)
			textSpacing = true;
		}
		if (!textSpacing) writer.write(indentLevel);
                writer.write("</" + name + ">");
                writer.write(lineSeparator);
	}

	// entity-safe string
	public static String safe(String s) {
	    StringBuffer buf = new StringBuffer(s);
	    for (int i=0; i<buf.length(); i++) {
		char c = buf.charAt(i);
		String e = null;
		switch (c) {
		case '<': e = "&lt;"; break;
		case '&': e = "&amp;"; break;
		case '>': e = "&gt;"; break;
		case '"': e = "&quot;"; break;
		case '\'': e = "&apos;"; break;
		}
		if (e == null) continue;
		buf.replace(i, i+1, e);
		i++;
	    }
	    return buf.toString();
	}

	// write safe text for debugging
	public static  void main(String[] args)
	throws Exception {
	    File f = new File(args[0]);
	    String s = UtilIO.readText(f);
	    s = safe(s);
	    System.out.println(s);
	}
}
