/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// class to make line style icons for GUI

package JSim.gui; 

import javax.swing.*;
import java.awt.*;
import JSim.gui.graph.GraphData;

public class GLineIcon implements Icon {
	// construct-time fields
        private int width, height, line, thickness;
        private Color fg, bg;
	private BasicStroke stroke;

	// paint-time fields
	private int x, y;
        private int y2, w2;
	private Graphics2D g2;

        // constructor
        public GLineIcon(int w, int h, 
	Color f, Color b, int l, int t) {
            width = w;
            height = h;
            fg = f;
            bg = b;
	    line = l;
            thickness = t;

	    // setup stroke
            switch (thickness) {
            case GraphData.LINE_MEDIUM:
                stroke = new BasicStroke(2.5f);
                break;
            case GraphData.LINE_THICK:
                stroke = new BasicStroke(3.5f);
                break;
            default:
                stroke = new BasicStroke(1.5f);
                break;
            }
        }

 	// dimension query
        public int getIconWidth() { return width; }
        public int getIconHeight() { return height; }

        // paint 
        public void paintIcon (
        Component c, Graphics g, int xx, int yy) {
	    x = xx;
	    y = yy;
            g.setColor(bg);
            g.fillRect(x, y, width, height); // changed from clearRect
            g.setColor(fg);
            y2 = y + (height / 2);
            w2 = width / 20;   // twenty-segment lines
            g2 = (Graphics2D) g;
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                                RenderingHints.VALUE_ANTIALIAS_ON);
	    g2.setStroke(stroke);
	    switch (line) {
            case GraphData.LINE_SOLID: paintSolid(); break;
            case GraphData.LINE_SHORTDASH: paintSDash(); break;
            case GraphData.LINE_LONGDASH: paintLDash(); break;
            case GraphData.LINE_DOT: paintDot(); break;
            case GraphData.LINE_DOTDASH: paintDotDash(); break;
            case GraphData.LINE_DOTDOTDASH: paintDotDotDash(); break;
            case GraphData.LINE_DOTDOTDOTDASH: paintDotDotDotDash(); break;
            case GraphData.LINE_SDASHLDASH: paintSDashLDash(); break;
            case GraphData.LINE_DOTSDASHLDASH: paintDotSDashLDash(); break;
            case GraphData.LINE_DOTSDASHDOTLDASH: paintDotSDashDotLDash(); break;
	    }
       }

	// solid line
	private void paintSolid() {
	    g2.drawLine(x, y2, x + width, y2);
	}

	// dot line
	private void paintDot() {
            g2.drawLine(x, y2, x + w2, y2);
            g2.drawLine(x + (4 * w2), y2, x + (5 * w2), y2);
            g2.drawLine(x + (8 * w2), y2, x + (9 * w2), y2);
            g2.drawLine(x + (12 * w2), y2, x + (13 * w2), y2);
            g2.drawLine(x + (16 * w2), y2, x + (17 * w2), y2);
            g2.drawLine(x + (20 * w2), y2, x + (21 * w2), y2);
	}

	// dash line
	private void paint() {
            g2.drawLine(x, y2, x + (3 * w2), y2);
            g2.drawLine(x + (6 * w2), y2, x + (9 * w2), y2);
            g2.drawLine(x + (12 * w2), y2, x + (15 * w2), y2);
            g2.drawLine(x + (18 * w2), y2, x + (21 * w2), y2);
	}

	// short dash line
	private void paintSDash() {
            g2.drawLine(x, y2, x + (3 * w2), y2);
            g2.drawLine(x + (6 * w2), y2, x + (9 * w2), y2);
            g2.drawLine(x + (12 * w2), y2, x + (15 * w2), y2);
            g2.drawLine(x + (18 * w2), y2, x + (21 * w2), y2);
	}

	// long dash line
	private void paintLDash() {
            g2.drawLine(x, y2, x + (6 * w2), y2);
            g2.drawLine(x + (9 * w2), y2, x + (15 * w2), y2);
            g2.drawLine(x + (18 * w2), y2, x + (24 * w2), y2);
	}

	// dot dash line
	private void paintDotDash() {
            g2.drawLine(x, y2, x + w2, y2);
            g2.drawLine(x + (4 * w2), y2, x + (7 * w2), y2);
	}

	// dot dot dash line
	private void paintDotDotDash() {
            g2.drawLine(x, y2, x + w2, y2);
            g2.drawLine(x + (4 * w2), y2, x + (5 * w2), y2);
            g2.drawLine(x + (8 * w2), y2, x + (11 * w2), y2);
	}

	// dot dot dot dash line
	private void paintDotDotDotDash() {
            g2.drawLine(x, y2, x + w2, y2);
            g2.drawLine(x + (4 * w2), y2, x + (5 * w2), y2);
            g2.drawLine(x + (8 * w2), y2, x + (9 * w2), y2);
            g2.drawLine(x + (12 * w2), y2, x + (15 * w2), y2);
	}

	// short dash long dash line
	private void paintSDashLDash() {
            g2.drawLine(x, y2, x + (3 * w2), y2);
            g2.drawLine(x + (6 * w2), y2, x + (12 * w2), y2);
	}

	// dot short dash long dash line
	private void paintDotSDashLDash() {
            g2.drawLine(x, y2, x + w2, y2);
            g2.drawLine(x + (4 * w2), y2, x + (7 * w2), y2);
            g2.drawLine(x + (10 * w2), y2, x + (16 * w2), y2);
	}

	// dot short dash dot long dash line
	private void paintDotSDashDotLDash() {
            g2.drawLine(x, y2, x + w2, y2);
            g2.drawLine(x + (4 * w2), y2, x + (7 * w2), y2);
            g2.drawLine(x + (10 * w2), y2, x + (11 * w2), y2);
            g2.drawLine(x + (14 * w2), y2, x + (20 * w2), y2);
	}

}
