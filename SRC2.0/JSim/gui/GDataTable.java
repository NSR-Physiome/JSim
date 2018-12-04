/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Currently not implimented.

// JSim Data display/edit table

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;

public class GDataTable extends JPanel {
	private GDataTableModel model;
	private JTable table;
	private JScrollPane scroll;

	// constructor
	public GDataTable() {
	    super(new BorderLayout());
	    model = new GDataTableModel();
	    table = new JTable(model);
	    TableCellEditor editor = new DefaultCellEditor(
	    	new JTextField());
	    table.setCellEditor(editor);
	    editor.addCellEditorListener(model);
	    scroll = new JScrollPane(table);
	    add(scroll, BorderLayout.CENTER);
	    JPanel bpanel = new JPanel(new GridLayout(1, 2));
		JButton upButton = new JButton("Update");
		upButton.addActionListener(new ActionListener(){  
				public void actionPerformed(ActionEvent e){  
					//	System.out.println(" Update button pressed"); 
					try {
					model.writeToFile();
					 
					} catch (Xcept err) {	
						System.err.println( "Unable to save edited data to file: "+err.getMessage() );
					}
				}  
			});
	    bpanel.add(upButton);
	    bpanel.add(new JButton("Cancel"));
	    add(bpanel, BorderLayout.SOUTH);
	}

	// set properties
	public void setHeaderBackground(Color c) {
	    table.getTableHeader().setBackground(c);
	}	    

	// clear table
	
	// load table
	public void loadData(Data.List datas) throws Xcept {
	    model.loadData(datas);
	}
	
	// test harness
	public static void main(String[] args) throws Xcept {
	    if (args.length != 1) throw new Xcept(
	    	"Usage: GDataTable data-file");
	    File f = new File(args[0]);
	    Data.List datas = (new DataFormat.List()).readData(f);
	    JFrame frame = new JFrame("GDataTable");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	    JRootPane root = frame.getRootPane();
	    GDataTable table = new GDataTable();
	    table.setHeaderBackground(Color.lightGray);
	    table.loadData(datas);
	    root.setContentPane(table);
	    frame.setSize(new Dimension(300,500));
	    frame.setVisible(true); 
	}
}
