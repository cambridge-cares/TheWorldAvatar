package uk.ac.cam.cares.jps.building.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.openimaj.math.geometry.point.Point2d;
import org.openimaj.math.geometry.point.Point2dImpl;
import org.openimaj.math.geometry.shape.Polygon;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.PolygonUtil;
import uk.ac.cam.cares.jps.building.SimpleShapeConverter;
import uk.ac.cam.cares.jps.building.SimpleShapeConverter.SimpleShape;

public class TestPolygonUtil extends TestCase {

	public static void main(String[] args) {
		
		new TestPolygonUtil().testSimpleShapeForSimpleRotatedBox();
	}
	
	public static BuildingQueryPerformer createQueryPerformerForTheHague() {
		return new BuildingQueryPerformer();
	}
	
	private double area(Polygon p) {
		return PolygonUtil.calculateArea(p.getVertices());
	}
	
	private Polygon createPolygon(String points) {
		
		Polygon result = new Polygon();
		
		StringTokenizer tokenizer = new StringTokenizer(points, " ");
		while (tokenizer.hasMoreTokens()) {
			float x = Float.valueOf(tokenizer.nextToken());
			float y = Float.valueOf(tokenizer.nextToken());
			result.addVertex(x, y);
		}
		
		return result;
	}
	
	private Polygon move(Polygon p, float x, float y) {
		
		Polygon result = new Polygon();
		
		for (Point2d current : p.getVertices()) {
			result.addVertex(current.getX() + x, current.getY() + y);
		}
		
		return result;
	}
	
	private void draw(int movex, int movey, double factor, Polygon... polygons) {
		
		java.awt.Polygon[] awtpolygons = new java.awt.Polygon[polygons.length];
		
		for (int i=0; i<polygons.length; i++) {
			awtpolygons[i] = createAWTPolygon(movex, movey, factor, polygons[i]);
		}
		
		new DrawShapes(awtpolygons);
	}
	
	private java.awt.Polygon createAWTPolygon (int movex, int movey, double factor, Polygon p) {
		
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
	
	private Polygon createLShapedPolygonClockwise() {
		Polygon p = new Polygon();
		p.addVertex(200, 200);
		p.addVertex(250, 200);
		p.addVertex(250, 220);
		p.addVertex(230, 220);
		p.addVertex(230, 300);
		p.addVertex(200, 300);
		p.addVertex(200, 200);
		return p;
	}
	
	private Polygon createLShapedPolygonAntiClockwise() {
		Polygon p = new Polygon();
		
		p.addVertex(200, 200);
		p.addVertex(200, 300);
		p.addVertex(230, 300);
		p.addVertex(230, 220);
		p.addVertex(250, 220);
		p.addVertex(250, 200);
		p.addVertex(200, 200);
		
		return p;
	}
	
	private List<Polygon> createComplexPolygonList() {
				
		int factor = 10;
		
		List<Integer> p1xlist = multiplyWith(factor, Arrays.asList("80863.431","80853.517","80853.497","80822.575","80822.763","80821.608","80820.429","80819.324","80818.288","80817.345","80816.518","80815.806","80815.232","80814.807","80814.518","80814.41","80814.4","80864.665","80863.431"));
		List<Integer> p1ylist = multiplyWith(factor, Arrays.asList("452961.702","452962.853","452962.684","452966.363","452967.772","452967.947","452968.289","452968.778","452969.414","452970.179","452971.064","452972.047","452973.114","452974.251","452975.435","452976.644","452977.819","452971.872","452961.702"));
		List<Integer> p2xlist = multiplyWith(factor, Arrays.asList("80864.665","80780.85","80776.066","80766.327","80765.928","80765.482","80765.429","80765.46","80765.864","80766.632","80767.711","80775.483","80775.709","80775.75","80784.302","80790.057","80792.334","80868.872","80864.665"));
		List<Integer> p2ylist = multiplyWith(factor, Arrays.asList("452971.872","452981.87","452978.41","452991.982","452992.679","452994.074","452995.538","452995.805","452997.233","452998.504","452999.524","453005.113","453005.221","453005.164","453011.318","453014.937","453015.812","453006.554","452971.872"));
		
		return createPolygonList(p1xlist, p1ylist, p2xlist, p2ylist);
	}
	
	private List<Polygon> createComplexPolygonListWithThreeParts() {
		
		int factor = 10;
		
		List<Integer> p1xlist = multiplyWithNew(factor, Arrays.asList(79418.245,79416.506,79415.114,79414.09,79413.461,79411.968, 79411.585,79410.827,79410.029,79409.371,79408.599,79407.913,79398.617,79418.245));
		List<Integer> p1ylist = multiplyWithNew(factor, Arrays.asList(458721.842,458719.215,458716.39,458713.411,458710.389,458697.381,458695.729,458694.212, 458693.205,458692.799 ,458692.77 ,458693.125,458701.979,458721.842));	
		List<Integer> p2xlist = multiplyWithNew(factor, Arrays.asList(79391.232,79387.749,79387.829 ,79384.555, 79384.268,79384.54,79382.855, 79381.99 ,79343.928,79346.307,79334.419,79365.692,79365.689,79366.105,79367.035,79368.257,79369.694,79371.257,79372.848,79374.368,79375.721,79376.822,79377.604, 79377.627,79412.288,79409.315,79414.555,79408.547,79406.125,79401.199,79396.738,79388.398,79390.79,79387.349,79404.306,79397.012,79397.904,79391.232));
		List<Integer> p2ylist = multiplyWithNew(factor, Arrays.asList(458711.107,458707.462,458707.386,458703.935,458704.214 ,458704.494,458706.132,458705.211,458741.474,458743.886,458761.49,458790.026,458790.455,458791.393,458792.687,458793.709,458794.397,458794.706,458794.618,458794.139,458793.297,458792.146,458790.757,458790.307,458761.707,458758.525,458753.535,458746.751,458749.056,458748.551,458752.179,458750.902,458748.619,458745.004,458728.865,458721.202,458720.354,458711.107));
		List<Integer> p3xlist = multiplyWithNew(factor, Arrays.asList(79432.657,79432.669,79432.287, 79431.451,79432.211,79420.303,79418.245,79398.617,79392.878,79393.933,79391.232,79397.904,79416.16,79408.547,79417.677,79431.406,79432.253,79432.657));						  
		List<Integer> p3ylist = multiplyWithNew(factor, Arrays.asList(458741.326,458739.999,458738.727,458737.487,458736.763,458724.227,458721.842,458701.979,458707.445,458708.553,458711.107,458720.354,458739.534,458746.751,458756.787,458743.744,458742.591,458741.326));

		//return createPolygonList(p3xlist, p3ylist);
		return createPolygonList(p1xlist, p1ylist, p2xlist, p2ylist, p3xlist, p3ylist);
	}
	
	private List<Polygon> createPolygonList(List<Integer>... lists) {
		
		List<Polygon> result = new ArrayList<Polygon>();

		int minx = -1;
		int miny = -1;
		for (int i=0; i<lists.length; i=i+2) {
			int minxlist = calculateMinimum(lists[i]);
			int minylist = calculateMinimum(lists[i+1]);
			if (i==0) {
				minx = minxlist;
				miny = minylist;
			} else {
				if (minxlist < minx) {
					minx = minxlist;
				}
				if (minylist < miny) {
					miny = minylist;
				}
			}
		}
		
		System.out.println("minx=" + minx + ", miny=" + miny);
		
		for (int i=0; i<lists.length; i=i+2) {
		
			List<Integer> xlist = lists[i];
			List<Integer> ylist = lists[i+1];
	
			Polygon polygon = new Polygon();
			result.add(polygon);
			for (int j=0; j<xlist.size(); j++) {
				
				int x = xlist.get(j) - minx;
				int y = ylist.get(j) - miny;	
				polygon.addVertex(x, y);
			}
		}
	
		return result;
	}
	
	private int calculateMinimum(List<Integer> list) {
		Integer min = list.get(0);
		for (int i=1; i<list.size(); i++) {
			if (list.get(i) < min) {
				min = list.get(i);
			}
		}
		return min;
	}

	private List<Integer> multiplyWith(int factor, List<String> list) {
		List<Integer> result = new ArrayList<Integer>();
		for (String current : list) {
			int i = multiplyWith(current, factor);
			result.add(i);
		}
		return result;
	}
	
	private List<Integer> multiplyWithNew(int factor, List<Double> list) {
		List<Integer> result = new ArrayList<Integer>();
		for (Double current : list) {
			int i = Math.round((float) (current * factor));
			result.add(i);
		}
		return result;
	}
	
	private int multiplyWith(String s, int factor) {
		float d = Float.valueOf(s) * factor;
		return Math.round(d);
	}
	
	public void testIntersectionArea() {
		
		Polygon p1 = new Polygon();
		p1.addVertex(100, 100);
		p1.addVertex(200, 100);
		p1.addVertex(200, 200);
		p1.addVertex(100, 200);
		
		Polygon p2 = new Polygon();
		p2.addVertex(150, 200);
		p2.addVertex(200, 120);
		p2.addVertex(300, 200);
		p2.addVertex(250, 350);
		p2.addVertex(230, 250);
		
		Polygon intersection = p1.intersect(p2);
		assertEquals(2000., area(intersection));
		
		draw(0, 0, 1, p1, p2, intersection);	
	}
	
	public void testProjection() {
		
		Point2d lineStart = new Point2dImpl(100,200);
		Point2d lineEnd = new Point2dImpl(140,240);
		Point2d point = new Point2dImpl(180,200);
		
		Object[] projection = PolygonUtil.calculateProjection(lineStart, lineEnd, point);
		
		assertEquals(140.0f, ((Point2d) projection[0]).getX()); 
		assertEquals(240.0f, ((Point2d) projection[0]).getY());
		assertEquals(1.0, (double) projection[1]); 
		
		System.out.println(projection[0] + " " + projection[1]);
	}
	
	public void testCenterAndAreaForBestShrinkedBox() {
		
		// original polygon
		Polygon p = createLShapedPolygonClockwise();

		List<Polygon> polygons = Arrays.asList(p);
		double area = PolygonUtil.area(polygons);
		assertEquals(3400., area, 0.1);
		
		Polygon box = PolygonUtil.createBestShrinkedBox(polygons);
		System.out.println("box=" + box);
		int size = box.getVertices().size();
		assertEquals(5, size);
		assertEquals(3400., area(box), 0.1);
		
		Point2d centroid = PolygonUtil.center(box.get(0), box.get(2));
		assertEquals(217.0, centroid.getX(), 0.1);
		assertEquals(250.0, centroid.getY(), 0.1);

		// rotated polygon	
		p.rotate(new Point2dImpl(200,200), 0.2);
		
		box = PolygonUtil.createBestShrinkedBox(polygons);
		System.out.println("box=" + box);
		assertEquals(3400., area(box), 0.1);
		
		centroid = PolygonUtil.center(box.get(0), box.get(2));
		assertEquals(206.72, centroid.getX(), 0.1);
		assertEquals(252.38, centroid.getY(), 0.1);
		
		// box with additional vertex
		box.addVertex(box.getVertices().get(1).getX(), box.getVertices().get(1).getY());
		System.out.println("box=" + box);
		size = box.getVertices().size();
		assertEquals(6, size);
		assertEquals(3400., area(box), 0.1);
		
		centroid = PolygonUtil.center(box.get(0), box.get(2));
		assertEquals(206.72, centroid.getX(), 0.1);
		assertEquals(252.38, centroid.getY(), 0.1);
	}
	
	public void testShrinkedBoxForClockwisePolygon() {
		
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -1., -1.57, -2.};
		
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			Polygon p = createLShapedPolygonClockwise();
			p.rotate(new Point2dImpl(200,200), angle);
			testShrinkedBoxClockwise(p);
		}
	}
	
	private void testShrinkedBoxClockwise(Polygon p) {
		
		double area = area(p);
		assertEquals(3400., area, 0.1);
		
		Polygon box = PolygonUtil.createShrinkedBox(p, 0, area);
		assertAreaAndIntersection(p, area, box, 2440.);
		
		box = PolygonUtil.createShrinkedBox(p, 1, area);
		assertAreaAndIntersection(p, area, box, 1800.);
		
		box = PolygonUtil.createShrinkedBox(p, 2, area);
		assertAreaAndIntersection(p, area, box, 2040.);

		box = PolygonUtil.createShrinkedBox(p, 3, area);
		assertAreaAndIntersection(p, area, box, 3000.);

		box = PolygonUtil.createShrinkedBox(p, 4, area);
		assertAreaAndIntersection(p, area, box, 2040.);

		box = PolygonUtil.createShrinkedBox(p, 5, area);
		assertAreaAndIntersection(p, area, box, 3080.);
	}
	
	public void testShrinkedBoxForAntiClockwisePolygon() {
		
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -1., -1.57, -2.};
		
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			Polygon p = createLShapedPolygonAntiClockwise();
			p.rotate(new Point2dImpl(200,200), angle);
			testShrinkedBoxAntiClockwise(p);
		}
	}
	
	private void testShrinkedBoxAntiClockwise(Polygon p) {
		
		double area = area(p);
		assertEquals(3400., area, 0.1);
		
		Polygon box = PolygonUtil.createShrinkedBox(p, 5, area);
		assertAreaAndIntersection(p, area, box, 2440.);
	
		box = PolygonUtil.createShrinkedBox(p, 4, area);
		assertAreaAndIntersection(p, area, box, 1800.);

		box = PolygonUtil.createShrinkedBox(p, 3, area);
		assertAreaAndIntersection(p, area, box, 1000.);
	
		box = PolygonUtil.createShrinkedBox(p, 2, area);
		assertAreaAndIntersection(p, area, box, 400.);

		box = PolygonUtil.createShrinkedBox(p, 1, area);
		assertAreaAndIntersection(p, area, box, 2040.);
	
		box = PolygonUtil.createShrinkedBox(p, 0, area);
		assertAreaAndIntersection(p, area, box, 3080.);
	}
	
	private void assertAreaAndIntersection(Polygon polygon, double polygonArea, Polygon box, double intersectionArea) {
		double boxArea = area(box);
		assertEquals(polygonArea, boxArea, 0.1);
		Polygon intersection = polygon.intersect(box);
		assertEquals(intersectionArea, area(intersection), 0.1);
	}
		
	public void testConvexHullClockwise() {
		
		Polygon p = createLShapedPolygonClockwise();
		Polygon hull = p.calculateConvexHull();
		
		assertEquals(6, hull.getVertices().size());
		assertEquals(p.get(0), hull.get(0));
		assertEquals(p.get(1), hull.get(1));
		assertEquals(p.get(2), hull.get(2));
		assertEquals(p.get(4), hull.get(3));
		assertEquals(p.get(5), hull.get(4));
		assertEquals(p.get(6), hull.get(5));	
	}
	
	public void testConvexHullAntiClockwise() {
		
		Polygon p = createLShapedPolygonAntiClockwise();
		
		Polygon hull = p.calculateConvexHull();
		assertEquals(6, hull.getVertices().size());
		// the polygon of the convex hull seems to be always ordered clockwise 
		assertEquals(p.get(6), hull.get(0));
		assertEquals(p.get(5), hull.get(1));
		assertEquals(p.get(4), hull.get(2));
		assertEquals(p.get(2), hull.get(3));
		assertEquals(p.get(1), hull.get(4));
		assertEquals(p.get(0), hull.get(5));	
	}
	
	public void testConvexHullThreePolygons() {
		
		String p1 = "120 100 160 100 160 140 120 140 120 100";
		String p2 = "140 180 140 200 160 200 160 180 140 180";
		String p3 = "100 120 140 120 140 160 100 160 100 120";
		String p = p1 + " " + p2 + " " + p3;
		Polygon polygon = createPolygon(p);
		Polygon hull = polygon.calculateConvexHull();
		
		assertEquals(7, hull.getVertices().size());
		assertEquals(new Point2dImpl(120, 100), hull.get(0));
		assertEquals(new Point2dImpl(160, 100), hull.get(1));
		assertEquals(new Point2dImpl(160, 200), hull.get(2));
		assertEquals(new Point2dImpl(140, 200), hull.get(3));
		assertEquals(new Point2dImpl(100, 160), hull.get(4));
		assertEquals(new Point2dImpl(100, 120), hull.get(5));	
		assertEquals(new Point2dImpl(120, 100), hull.get(6));	
	}
	
	public void testShrinkedBoxForConvexHull() {

		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57};

		for (double angle : angles) {
			System.out.println("angle=" + angle);
			Polygon p = createLShapedPolygonAntiClockwise();
			p.rotate(new Point2dImpl(200,200), angle);
			Polygon hull = p.calculateConvexHull();
			double area = area(hull);
			assertEquals(4200., area, 0.1);
			
			Polygon box = PolygonUtil.createShrinkedBox(hull, 2, area);
			assertAreaAndIntersection(p, area, box, 2929.78);
		}
	}
	
	public void testShringkedBoxError1() {
		
		Polygon p = createPolygon("79281.18 454438.34 79285.34 454439.88 79296.164 454443.9 79293.805 454450.78"
				+ " 79282.85 454446.72 79278.914 454445.25 79281.18 454438.34");
		
		//draw(-79200, -454400, 10, p);
		
		Polygon box = PolygonUtil.createShrinkedBox(p, 3, 103.24560546875);
		assertNull(box);
		//[(79281.18,454438.34), (79285.34,454439.88), (79296.164,454443.9), (79293.805,454450.78), (79282.85,454446.72), (79278.914,454445.25), (79281.18,454438.34)]
		//[(79293.805,454450.78), (79282.85,454446.72), (79278.914,454445.25), (79281.18,454438.34), (79285.34,454439.88), (79296.164,454443.9), (79293.805,454450.78)]
	}
	
	public void testBestShrinkedBoxForLShapedPolygonClockwise() {
		
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -1., -1.57, -2.};
		
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			Polygon p = createLShapedPolygonClockwise();
			p.rotate(new Point2dImpl(200,200), angle);
			
			Polygon box = PolygonUtil.createBestShrinkedBox(Arrays.asList(p));
			assertEquals(3400., area(box), 0.1);
			
			double intersectionArea = area(p.intersect(box));
			assertEquals(3080., intersectionArea, 0.1);
		}
	}
	
	public void testBestShrinkedBoxForLShapedPolygonAntiClockwise() {
		
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -1., -1.57, -2.};
		
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			Polygon p = createLShapedPolygonAntiClockwise();
			p.rotate(new Point2dImpl(200,200), angle);
			
			Polygon box = PolygonUtil.createBestShrinkedBox(Arrays.asList(p));
			assertEquals(3400., area(box), 0.1);
			
			double intersectionArea = area(p.intersect(box));
			assertEquals(3080., intersectionArea, 0.1);
		}
	}
	
	public void testBestShrinkedBoxForComplexPolygon() {
		
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -1., -1.57, -2.};
		//double[] angles = new double[] {2.};
		
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			List<Polygon> polygons = createComplexPolygonList();
			for (Polygon current : polygons) {
				current.rotate(new Point2dImpl(200,200), angle);
			}
			
			double area = PolygonUtil.area(polygons);
			assertEquals(377400.5, area, 0.1);
			
			Polygon box = PolygonUtil.createBestShrinkedBox(polygons);
			assertEquals(377400.5, area(box), 0.1);
			
			double intersectionArea = PolygonUtil.calculateIntersectionArea(polygons, box);
			assertEquals(338763.7, intersectionArea, 0.1);
			
			Polygon merged = PolygonUtil.merge(polygons);
			Polygon hull = merged.calculateConvexHull();
			draw(400, 400, 1, merged, hull, box);
		}
	}
	
	public void testBestShrinkedBoxForComplexPolygonWithThreeParts() {
		
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -1., -1.57, -2.};
		//double[] angles = new double[] {2.};
		
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			List<Polygon> polygons = createComplexPolygonListWithThreeParts();
			for (Polygon current : polygons) {
				current.rotate(new Point2dImpl(200,200), angle);
			}
			
			double area = PolygonUtil.area(polygons);
			assertEquals(478650.5, area, 1);
			
			Polygon box = PolygonUtil.createBestShrinkedBox(polygons);
			assertEquals(478650.5, area(box), 1);
			
			double intersectionArea = PolygonUtil.calculateIntersectionArea(polygons, box);
			assertEquals(392408., intersectionArea, 1);
			
			Polygon merged = PolygonUtil.merge(polygons);
			Polygon hull = merged.calculateConvexHull();
			//draw(800, 200, 0, polygons.get(0), polygons.get(1), polygons.get(2), hull, box);
			//draw(800, 200, 0, hull, box);
		}
	}
	
	/**
	 * The method Polygon.calculateArea() had problems with large coordinate values. The new method {@see PolygonUtil#calculateSignedArea(List)} 
	 * is tested here.
	 */
	public void testAreaForLargeCoordinates() {
	
		Polygon p = new Polygon();
		p.addVertex(79439.151f, 454733.054f);
		p.addVertex(79433.51f, 454741.397f);
		p.addVertex(79429.187f, 454738.596f);
		p.addVertex(79434.822f, 454730.25f);
		p.addVertex(79439.151f, 454733.054f);
		
		System.out.println("Polygon with large coordinates = " + p);
		
		double areaNew = area(p);
		assertEquals(51.933, areaNew, 0.01);
		
		Polygon p2 = new Polygon();
		p2.addVertex(39.151f, 33.054f);
		p2.addVertex(33.51f, 41.397f);
		p2.addVertex(29.187f, 38.596f);
		p2.addVertex(34.822f, 30.25f);
		p2.addVertex(39.151f, 33.054f);
		
		System.out.println("Polygon with small coordinates = " + p2);
		
		areaNew = area(p2);
		assertEquals(51.898, areaNew, 0.01);
	}
	
	/**
	 * The method Polygon.calculateSignedArea() had problems with large coordinate values. The new method {@see PolygonUtil#calculateSignedArea(List)} 
	 * is tested here.
	 */
	public void testAreaForNonClosedPolygon() {
	
		Polygon p = new Polygon();
		p.addVertex(79439.151f, 454733.054f);
		p.addVertex(79433.51f, 454741.397f);
		p.addVertex(79429.187f, 454738.596f);
		p.addVertex(79434.822f, 454730.25f);
		
		System.out.println("Polygon non closed = " + p);

		double areaNew = area(p);
		assertEquals(51.933, areaNew, 0.01);
	}
	
	/**
	 * The building from the Hague with the name = BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC"
	 * doesn't have any building parts and only contains one ground surface. We use here the corresponding result from the sparql query
	 * to check the calculation of the building data for ADMS
	 */
	public void testCalculateTheHagueBuildingDataForADMS() {
		
		String csvCoordinates = 
			"x,y,z\n" + 
			"79439.151,454733.054,1.841\n" + 
			"79433.51,454741.397,1.841\n" + 
			"79429.187,454738.596,1.841\n" + 
			"79434.822,454730.25,1.841\n" + 
			"79439.151,454733.054,1.841";
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(csvCoordinates);
		Polygon p = SimpleShapeConverter.convertTo2DPolygon(map, "x", "y");
		
		System.out.println("original Polygon = " + p);
		
		List<Polygon> polygons = Arrays.asList(p);
		
		double area = PolygonUtil.area(polygons);	
		System.out.println("area = " + area);
		Polygon box = PolygonUtil.createBestShrinkedBox(polygons);
		System.out.println("box = " + box);
		
		assertEquals(area, area(box), 0.1);
		
		SimpleShape shape = SimpleShapeConverter.simplifyShapes(polygons);
		System.out.println("simple shape result = " + shape);
		
		Polygon recreatedBox = SimpleShapeConverter.createPolygon(shape);
		System.out.println("recreated box = " + recreatedBox);
		
		assertNearlySamePolygons(box, recreatedBox);
		
		draw(-79425, -454725, 20, p, recreatedBox);
		
		assertEquals(0, shape.shapeType);
		assertEquals(79434.15625, shape.centerX, 0.1);
		assertEquals(454735.8125, shape.centerY, 0.1);
		assertEquals(10.145, shape.length, 0.01);
		assertEquals(5.119, shape.width, 0.01);
		assertEquals(145.955, shape.angle, 0.01);
	}
	
	public void testCalculateBERLINBuildingDataForADMS() {
		
		String query = createQueryPerformerForTheHague().getQueryBdnVerticesWithAndWithoutBuildingParts("http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#BuildingBLDG_0003000f0029558f");		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.BERLIN_IRI, query);
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		Polygon p = SimpleShapeConverter.convertTo2DPolygon(map, "x", "y");
		
		System.out.println("original Polygon = " + p);
		
		List<Polygon> polygons = Arrays.asList(p);
		
		double area = PolygonUtil.area(polygons);	
		System.out.println("area = " + area);
		Polygon box = PolygonUtil.createBestShrinkedBox(polygons);
		System.out.println("box = " + box);
		
		//assertEquals(area, area(box), 0.1);
		
		SimpleShape shape = SimpleShapeConverter.simplifyShapes(polygons);
		System.out.println("simple shape result = " + shape);
		
		Polygon recreatedBox = SimpleShapeConverter.createPolygon(shape);
		System.out.println("recreated box = " + recreatedBox);
		
		//assertNearlySamePolygons(box, recreatedBox);
		
		draw(-79425, -454725, 20, p, recreatedBox);
		
		assertEquals(0, shape.shapeType);
		assertEquals(392757.8125, shape.centerX, 0.1);
		assertEquals(5819013.0, shape.centerY, 0.1);
		assertEquals(61.0, shape.length, 0.01);
		assertEquals(103.23, shape.width, 0.01);
		assertEquals(22.15, shape.angle, 0.01);
	}
	
	public void testCalculateAngleToYAxis() {
		
		double angle = SimpleShapeConverter.calculateAngleBetweenVectorToYAxis(1, 1, 1, 3);
		assertEquals(0, angle, 0.1);
				
		angle = SimpleShapeConverter.calculateAngleBetweenVectorToYAxis(1, 1, 3, 3);
		assertEquals(45, angle, 0.1);
		
		angle = SimpleShapeConverter.calculateAngleBetweenVectorToYAxis(1, 1, 3, 1);
		assertEquals(90, angle, 0.1);
		
		angle = SimpleShapeConverter.calculateAngleBetweenVectorToYAxis(1, 1, -3, 5);
		assertEquals(135, angle, 0.1);
		
		angle = SimpleShapeConverter.calculateAngleBetweenVectorToYAxis(1, 1, 1, -2);
		assertEquals(0, angle, 0.1);	
	}
	
	public void testSimpleShapeForSimpleRotatedBox() {
				
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -.1, -1., -1.57, -2.};
		//double[] angles = new double[] {0.2};
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			Polygon p1 = createPolygon("100 300 200 500 600 300 500 100 100 300");
			p1.rotate(angle);
			System.out.println("original polygon = " + p1 );
			SimpleShape shape = SimpleShapeConverter.simplifyShapes(Arrays.asList(p1));
			System.out.println("shape = " + shape);
			Polygon p2 = SimpleShapeConverter.createPolygon(shape);
			System.out.println("recreated polygon = " + p2);	
			
			draw(0, 0, 1 , p1, p2);
			
			assertNearlySamePolygons(p1, p2);
		}
	}
	
	private void assertNearlySamePolygons(Polygon p1, Polygon p2) {
		
		double delta = 0.1;
		
		assertEquals(p1.size(), p2.size());
		
		for (int i=0; i<p1.size(); i++) {
			boolean found = false;
			for (int j=0; j<p2.size(); j++) {
				Point2d diff = p1.get(i).minus(p2.get(j));
				if (PolygonUtil.length(diff) <= delta) {
					found = true;
					break;
				}
			}
			
			assertTrue("Point with index=" + i + " wasn't found", found);
		}
	}
	
	public void testSimpleShapeForComplexPolygonWithThreeParts() {
		
		double[] angles = new double[] {0., 0.1, 0.2, 0.5, 1.4, 1.57, 2., -1., -1.57, -2.};
		//double[] angles = new double[] {-1.57};
		
		for (double angle : angles) {
			System.out.println("angle=" + angle);
			List<Polygon> polygons = createComplexPolygonList();
			for (Polygon current : polygons) {
				current.rotate(new Point2dImpl(200,200), angle);
			}
			
			SimpleShape shape = SimpleShapeConverter.simplifyShapes(polygons);
	
			Polygon box = SimpleShapeConverter.createPolygon(shape);
			
			double intersectionArea = PolygonUtil.calculateIntersectionArea(polygons, box);
			assertEquals(338763., intersectionArea, 1.);
			
			//draw(400, 400, 1., polygons.get(0), polygons.get(1), polygons.get(0), box);
		}
	}
	
	public void testCalculateCenterForSimplifiedShape() {
		Polygon polygon = createPolygon("-10 0 0 10 10 0 0 -10");
		SimpleShape shape = SimpleShapeConverter.simplifyShapes(Arrays.asList(polygon));
		assertEquals(0.0, shape.centerX, 0.001);
		assertEquals(0.0, shape.centerY, 0.001);
	}
	
	public void testCalculatePerimeterForNonClosedBox() {
		Polygon polygon = createPolygon("-10 -10 -10 10 10 10 10 -10");
		double perimeter = PolygonUtil.calculatePerimeter(polygon);
		assertEquals(80., perimeter, 0.01);
	}
	
	public void testCalculatePerimeterForClosedBox() {
		Polygon polygon = createPolygon("-10 -10 -10 10 10 10 10 -10 -10 -10");
		double perimeter = PolygonUtil.calculatePerimeter(polygon);
		assertEquals(80., perimeter, 0.01);
	}
	
	public void testCalculateCenterOfMass() {
		
		List<Polygon> polygons = new ArrayList<>();
		// clockwise polygon
		polygons.add(move(createPolygon("-10 -10 10 -10 10 10 -10 10 -10 -10"), 20, 20));
		// the same shape but anti clockwise
		polygons.add(move(createPolygon("-10 -10 -10 10 10 10 10 -10 -10 -10"), 20, 20));
		// the same shape but with additional edges
		polygons.add(move(createPolygon("-10 -10 -10 10 10 10 10 -10 5 -10 -2 -10 -10 -10"), 20, 20));
		
		for (Polygon current : polygons) {
			System.out.println(current);
			Point2d center = PolygonUtil.calculateCenterOfMass(current.getVertices());
			System.out.println("center = " + center);
			assertEquals(20.f, center.getX(), 0.01);
			assertEquals(20.f, center.getY(), 0.01);
		}
	}
	
	public void testSimpleShapeForPolygonWithEightEdges() {
		// this defines a symmetric polygon with 8 edges
		// clockwise and anti clockwise
		List<Polygon> polygons = new ArrayList<>();
		polygons.add(createPolygon("0 1 0 2 1 3 2 3 3 2 3 1 2 0 1 0 0 1"));
		polygons.add(createPolygon("0 1 1 0 2 0 3 1 3 2 2 3 1 3 0 2 0 1"));
		
		for (Polygon current : polygons) {
			
			Point2d center = PolygonUtil.calculateCenterOfMass(current.getVertices());
			System.out.println("center = " + center);

			SimpleShape shape = SimpleShapeConverter.simplifyShapes(Arrays.asList(current));
			// check that the shape is a circle
			assertEquals(1, shape.shapeType);
			assertEquals(1.5, shape.centerX);
			assertEquals(1.5, shape.centerY);
			assertEquals(3.0738, shape.length, 0.001);
		}
	}
}
