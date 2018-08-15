package uk.ac.cam.cares.jps.building.test;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.openimaj.math.geometry.point.Point2d;
import org.openimaj.math.geometry.shape.Polygon;

import uk.ac.cam.cares.jps.building.SimpleShapeConverter;
import uk.ac.cam.cares.jps.building.SimpleShapeConverter.SimpleShape;

public class DrawShapes extends JFrame {

	private static final long serialVersionUID = 1L;
	private static final Color[] colors = new Color[] {Color.BLUE, Color.RED}; //, Color.GREEN, Color.PINK};

	public DrawShapes(final Shape... shapes) {
		this(600, 600, shapes);
	}
		
	public DrawShapes(int xmaxScreen, int ymaxScreen, final Shape... shapes) {
		
		setSize(new Dimension(xmaxScreen, ymaxScreen));
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
		
		JPanel p = new JPanel() {
			@Override
			public void paintComponent(Graphics g) {
				Graphics2D g2 = (Graphics2D) g;
						
				for (int i=0; i<shapes.length; i++) {		
					if (i < colors.length) {
						g2.setColor(colors[i]);
					} else {
						g2.setColor(Color.BLACK);
					}
					Shape current = shapes[i];
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
	
	public static void drawWithAutosize(int xmaxScreen, int ymaxScreen, SimpleShape simpleShape, List<Polygon> polygons) {
		
		if (simpleShape.shapeType == 0) {
			Polygon box = SimpleShapeConverter.createPolygon(simpleShape);
			polygons.add(0, box);
		}
		
		float xmin = -1;
		float ymin = -1;
		float xmax = -1;
		float ymax = -1;
		
		for (Polygon currentPolygon : polygons) {

			for (Point2d p : currentPolygon.getVertices()) {
				float x = p.getX();
				float y = p.getY();
				if (xmin == -1) {
					xmin = x;
					xmax = x;
					ymin = y;
					ymax = y;
				} else {
					xmin = Math.min(xmin, x);
					xmax = Math.max(xmax, x);
					ymin = Math.min(ymin, y);
					ymax = Math.max(ymax, y);
				}
			}
		}
		
		float xf = (xmaxScreen) / (xmax - xmin);
		float yf = (ymaxScreen) / (ymax - ymin);
		double factor = Math.min(xf, yf);
		
		Polygon[] array = polygons.toArray(new Polygon[0]);
		Shape[] awtShapes = createAWTPolygons((int) -xmin, (int) -ymin, factor, array);
		
		if (simpleShape.shapeType == 0) {
			System.out.println("simple shape is a box");
			new DrawShapes(xmaxScreen, ymaxScreen, awtShapes);
		} else {
			System.out.println("simple shape is a circle");
			Shape circle = createAWTCirlce((int) -xmin, (int) -ymin, factor, simpleShape);
			Shape[] awtShapesWithCircle = new Shape[awtShapes.length + 1];
			awtShapesWithCircle[0] = circle;
			for (int i=0; i<awtShapes.length; i++) {
				awtShapesWithCircle[i+1] = awtShapes[i];
			}
			new DrawShapes(xmaxScreen, ymaxScreen, awtShapesWithCircle);
		}
	}
	
	public static void drawWithAutosize(int xmaxScreen, int ymaxScreen, SimpleShape simpleShape, SimpleShape simpleShape2, List<Polygon> polygons) {
		
		if (simpleShape.shapeType == 0) {
			Polygon box = SimpleShapeConverter.createPolygon(simpleShape);
			polygons.add(0, box);
		}
		if (simpleShape2 != null) {
			Polygon box = SimpleShapeConverter.createPolygon(simpleShape2);
			polygons.add(1, box);
		}
		
		float xmin = -1;
		float ymin = -1;
		float xmax = -1;
		float ymax = -1;
		
		for (Polygon currentPolygon : polygons) {

			for (Point2d p : currentPolygon.getVertices()) {
				float x = p.getX();
				float y = p.getY();
				if (xmin == -1) {
					xmin = x;
					xmax = x;
					ymin = y;
					ymax = y;
				} else {
					xmin = Math.min(xmin, x);
					xmax = Math.max(xmax, x);
					ymin = Math.min(ymin, y);
					ymax = Math.max(ymax, y);
				}
			}
		}
		
		float xf = (xmaxScreen) / (xmax - xmin);
		float yf = (ymaxScreen) / (ymax - ymin);
		double factor = Math.min(xf, yf);
		
		Polygon[] array = polygons.toArray(new Polygon[0]);
		Shape[] awtShapes = createAWTPolygons((int) -xmin, (int) -ymin, factor, array);
		
		if (simpleShape.shapeType == 0) {
			System.out.println("simple shape is a box");
			new DrawShapes(xmaxScreen, ymaxScreen, awtShapes);
		} else {
			System.out.println("simple shape is a circle");
			Shape circle = createAWTCirlce((int) -xmin, (int) -ymin, factor, simpleShape);
			Shape[] awtShapesWithCircle = new Shape[awtShapes.length + 1];
			awtShapesWithCircle[0] = circle;
			for (int i=0; i<awtShapes.length; i++) {
				awtShapesWithCircle[i+1] = awtShapes[i];
			}
			new DrawShapes(xmaxScreen, ymaxScreen, awtShapesWithCircle);
		}
	}
	
	public static void draw(int movex, int movey, double factor, Polygon... polygons) {
		java.awt.Polygon[] awtpolygons = createAWTPolygons(movex, movey, factor, polygons);
		new DrawShapes(awtpolygons);
	}
	
	private static java.awt.Polygon[] createAWTPolygons(int movex, int movey, double factor, Polygon... polygons) {
		java.awt.Polygon[] awtpolygons = new java.awt.Polygon[polygons.length];
		
		for (int i=0; i<polygons.length; i++) {
			if (polygons[i] != null) {
				awtpolygons[i] = createAWTPolygon(movex, movey, factor, polygons[i]);
			}
		}
		
		return awtpolygons;
	}
	
	private static Ellipse2D createAWTCirlce(int movex, int movey, double factor, SimpleShape shape) {
		double radius = shape.length / 2.;
		double upperleftx = (shape.centerX - radius + movex) * factor ;
		double upperlefty = (shape.centerY - radius + movey) * factor;
		double width = shape.length * factor;
		double height = shape.length * factor;
		return new Ellipse2D.Double(upperleftx, upperlefty, width, height);
	}
	
	private static java.awt.Polygon createAWTPolygon(int movex, int movey, double factor, Polygon p) {
		
		if (p == null) {
			return null;
		}
		
		System.out.println("Show points");
		
		java.awt.Polygon s = new java.awt.Polygon();
		for (Point2d current : p.getVertices()) {
			
			int x = (int) Math.round((current.getX() + movex) * factor);
			int y = (int) Math.round((current.getY() + movey) * factor);
			s.addPoint(x, y);
			
			System.out.println("("+x+","+y+")");
		}
		return s;
	}
}
