package uk.ac.cam.cares.jps.building.test;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

public class DrawShapes extends JFrame {

	private static final long serialVersionUID = 1L;

	public DrawShapes(final Shape... shapes) {

		setSize(new Dimension(320, 320));
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);

		JPanel p = new JPanel() {
			@Override
			public void paintComponent(Graphics g) {
				Graphics2D g2 = (Graphics2D) g;
				
				for (Shape current : shapes) {
					g2.draw(current);
				}
			}
		};
		this.getContentPane().add(p);
	}

	public static void draw(final Shape... shapes) {

		SwingUtilities.invokeLater(new Runnable() {

			@Override
			public void run() {
				
				new DrawShapes(shapes);
			}
		});
	}
}
