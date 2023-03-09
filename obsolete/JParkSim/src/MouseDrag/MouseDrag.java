package MouseDrag;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Label;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;


import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

/**
 * MouseDrag -- implement simple mouse drag in a window.
 */
@SuppressWarnings("serial")
public class MouseDrag extends JFrame implements MouseListener,
    MouseMotionListener {
  /**
	 * 
	 */


/** The Image we are to paint */
  Image curImage;

  /** Kludge for showStatus */
  static Label status;

  /** true if we are in drag */
  boolean inDrag = false;

  /** starting location of a drag */
  int startX = -1, startY = -1;

  /** current location of a drag */
  int curX = -1, curY = -1;
  

  // "main" method
  public static void Printing(String[] av) {
    JFrame f = new JFrame("Mouse Dragger");

//    String[] av = {"C:\\Users\\janusz\\Desktop\\Eddy FOO\\CurrentA.png"}; 

    
    if (av.length < 1) {
      System.err.println("Usage: MouseDrag imagefile");
      System.exit(1);
    }
//    Image im = Toolkit.getDefaultToolkit().getImage(av[0]);
    ImageIcon image = new ImageIcon(av[0]); //imports the image
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //this is your screen size
    JLabel lbl = new JLabel(image); //puts the image into a jlabel

    f.getContentPane().add(lbl); //puts label inside the jframe

    f.setSize(image.getIconWidth(), image.getIconHeight()); //gets h and w of image and sets jframe to the size

    int x = (screenSize.width - f.getSize().width)/2; //These two lines are the dimensions
    int y = (screenSize.height - f.getSize().height)/2;//of the center of the screen

    f.setLocation(x, y); //sets the location of the jframe
    f.setVisible(true); //makes the jframe visible
    // create a MouseDrag object

    
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  }

  // "Constructor" - creates the object
  public MouseDrag(Image i) {
    super();
    curImage = i;
    setSize(300, 200);
    addMouseListener(this);
    addMouseMotionListener(this);
  }

  public void showStatus(String s) {
    status.setText(s);
  }

  // Five methods from MouseListener:
  /** Called when the mouse has been clicked on a component. */
  public void mouseClicked(MouseEvent e) {
  }

  /** Called when the mouse enters a component. */
  public void mouseEntered(MouseEvent e) {
  }

  /** Called when the mouse exits a component. */
  public void mouseExited(MouseEvent e) {
  }

  /** Called when the mouse has been pressed. */
  public void mousePressed(MouseEvent e) {
    Point p = e.getPoint();
    System.err.println("mousePressed at " + p);
    startX = p.x;
    startY = p.y;
    inDrag = true;
  }

  /** Called when the mouse has been released. */
  public void mouseReleased(MouseEvent e) {
    inDrag = false;
    System.err.println("SELECTION IS " + startX + "," + startY + " to "
        + curX + "," + curY);
  }

  // And two methods from MouseMotionListener:
  public void mouseDragged(MouseEvent e) {
    Point p = e.getPoint();
    // System.err.println("mouse drag to " + p);
    showStatus("mouse Dragged to " + p);
    curX = p.x;
    curY = p.y;
    if (inDrag) {
      repaint();
    }
  }

  public void paint(Graphics g) {
    int w = curX - startX, h = curY - startY;
    Dimension d = getSize();
    g.drawImage(curImage, 0, 0, d.width, d.height, this);
    if (startX < 0 || startY < 0)
      return;
    System.err.println("paint:drawRect @[" + startX + "," + startY
        + "] size " + w + "x" + h);
    g.setColor(Color.red);
    g.fillRect(startX, startY, w, h);
  }

  public void mouseMoved(MouseEvent e) {
    showStatus("mouse Moved to " + e.getPoint());
  }

}
 
