package uk.ac.cam.cares.jps.building;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

import org.openimaj.math.geometry.point.Point2d;
import org.openimaj.math.geometry.point.Point2dImpl;
import org.openimaj.math.geometry.shape.Polygon;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PolygonUtil {
	
	static Logger logger = LoggerFactory.getLogger(PolygonUtil.class);
	
	public static Polygon createBestShrinkedBox(List<Polygon> polygons) {
		
		double area = area(polygons);
		
		Polygon merged = merge(polygons);
				
		Polygon hull = merged.calculateConvexHull();
		
		// iterate over all sides of the hull and select the shrinked box so 
		// that it has maximum intersection with the given polygon (not its hull)
		double maxIntersectionArea = -1;
		Polygon maxBox = null;
		for (int i=0; i<hull.size()-1; i++) {
			Polygon box = createShrinkedBox(hull, i, area);
			if (box == null) {
				logger.warn(i + ", start=" + hull.get(i) + ", end=" + hull.get(i+1) + " no box was created");
				continue;
			}
			
			// don't use here: 
			// a) polygon.intersectionArea(box);
			// b) merged.intersect(box).calculateArea();
			double intersectionArea = calculateIntersectionArea(polygons, box);
		
			if (intersectionArea > maxIntersectionArea) {
				maxIntersectionArea = intersectionArea;
				maxBox = box;
			}
			
			logger.debug(i + ", start=" + hull.get(i) + ", end=" + hull.get(i+1) + ", intersectionArea=" + intersectionArea + ", maxIntersectionArea=" + maxIntersectionArea);
		}
		
		return maxBox;
	}
	
	public static double calculateIntersectionArea(List<Polygon> polygons, Polygon polygonForIntersection) {
		List<Polygon> intersections = new ArrayList<Polygon>();
		for (Polygon current : polygons) {
			intersections.add(current.intersect(polygonForIntersection));
		}
		return area(intersections);
	}
	
	public static double area(List<Polygon> polygons) {
		double sum = 0.;
		for (Polygon current : polygons) {
			sum += calculateArea(current.getVertices());
		}
		return sum;
	}
	
	public static Polygon merge(List<Polygon> polygons) {
		Polygon result = new Polygon();
		for (Polygon current : polygons) {
			for (Point2d currentPoint : current.getVertices()) {
				result.addVertex(currentPoint);
			}
		}
		return result;
	}

	public static Polygon createShrinkedBox(Polygon polygon, int startVertex, double area) {
		
		Point2d[] points = reorderVertices(polygon, startVertex);
		
		// for the line spanned by points[0] (the original start vertex) and points[1]
		// calculate all projections of all other vertices and select those with 
		// maximum and minimum extent
		double minFactor = 0;
		Point2d minPoint = points[0];
		double maxFactor = 1;
		Point2d maxPoint = points[1];
		// this is the orthogonal vector between the third point and the line spanned
		// by the first and second vertex 
		Point2d orthogonal = null;
		
		for (int i=2; i<polygon.size()-1; i++) {
			Object[] projection = calculateProjection(points[0], points[1], points[i]);
			Point2d projectedPoint = (Point2d) projection[0];
			double factor = (double) projection[1];
			if (factor >= maxFactor) {
				maxFactor = factor;
				maxPoint = projectedPoint;
			} else if (factor <= minFactor) {
				minFactor = factor;
				minPoint = projectedPoint;
			}
			
			if (i == 2) {
				orthogonal = points[i].minus(projectedPoint);
			}
		}
		
		double delta = 0.0001;
		if (Math.abs(orthogonal.getX()) < delta && Math.abs(orthogonal.getY()) < delta) {
			return null;
		}
		
		double length = length(maxPoint.minus(minPoint));
		double width = area / length;
		double lengthOrthogonal = length(orthogonal);
		double x = (orthogonal.getX() / lengthOrthogonal) * width; 
		double y = (orthogonal.getY() / lengthOrthogonal) * width;
		Point2d thirdBoxPoint = new Point2dImpl(maxPoint.getX() + x, maxPoint.getY() + y);
		Point2d fourthBoxPoint = new Point2dImpl(minPoint.getX() + x, minPoint.getY() + y);
		
		Polygon box = new Polygon(minPoint, maxPoint, thirdBoxPoint, fourthBoxPoint, minPoint);
		return box;
	}
	
	private static Point2d[] reorderVertices(Polygon polygon, int vertex) {
		int size = polygon.size();
		//System.out.println("enter createShrinkedPolygon, index = " + indexOfStartVertex + ", vertices = " + size);
		
		// reorder the vertices such that the start vertex becomes index 0
		Point2d[] points = new Point2d[size];
		for (int j = 0; j < size; j++) {
			int originalIndex = vertex + j;
			// -2 because we assume that the first vertex and the last vertex are the same geometric point.
			if (originalIndex >  size - 2) {
				originalIndex = originalIndex - size + 1; 
			}

			points[j] = polygon.getVertices().get(originalIndex);
		}
		
		return points;
	}
	
	/**
	 * Calculates the projection of point on the line through lineStart and lineEnd
	 * and the factor such that projection = factor * (lineEnd - lineStart)
	 * 
	 * @param lineStart
	 * @param lineEnd
	 * @param point
	 * @return the projection as Point2d and the corresponding factor 
	 */
	public static Object[] calculateProjection(Point2d lineStart, Point2d lineEnd, Point2d point) {
		
		// move all points such that lineStart becomes the origin (0,0)
		Point2d diff1 = lineEnd.minus(lineStart);
		Point2d diff2 = point.minus(lineStart);
		double factor = dotProduct(diff1, diff2) / dotProduct(diff1, diff1);
		
		// calculate projection and move it back by lineStart
		double projectedX = diff1.getX() * factor + lineStart.getX();
		double projectedY = diff1.getY() * factor + lineStart.getY();
		Point2d projectedPoint = new Point2dImpl(projectedX, projectedY);
		
		return new Object[] {projectedPoint, factor};
	}
	
	public static double dotProduct(Point2d p1, Point2d p2) {
		return (p1.getX() * p2.getX() + p1.getY() * p2.getY());
	}
	
	public static double length(Point2d p) {
		return Math.sqrt((Math.pow(p.getX(), 2) + Math.pow(p.getY(), 2)));
	}
	
	/**
	 * The original method had the same problem as {@link #calculateSignedArea(List)} and was adapted.<br>
	 * <br>
	 * Function to calculate the area of a polygon, according to the algorithm
	 * defined at http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/
	 * 
	 * @author Christopher Fuhrman (christopher.fuhrman@gmail.com)
	 * 
	 * @param points
	 * @return
	 */
	private static double calculateSignedAreaAdaptedFromFuhrman(List<Point2d> points) {
		int i, j, n = points.size();
		double area = 0;

		for (i = 0; i < n; i++) {
			j = (i + 1) % n;
			area += ((double) points.get(i).getX()) * ((double) points.get(j).getY());
			area -= ((double) points.get(j).getX()) * ((double) points.get(i).getY());
		}
		area /= 2.0;
		return (area);
	}
	
	/**
	 * Calculate the area of the polygon. This does not take into account holes
	 * in the polygon.
	 *
	 * @return the area of the polygon
	 */
	public static double calculateArea(List<Point2d> points) {
		final boolean closed = PolygonUtil.isClosed(points);
		double area = 0;

		List<Point2d> copyPoints = points;
		if (!closed && points.size() > 0) {	
			copyPoints = new ArrayList<Point2d>(points);
			copyPoints.add(points.get(0));
		}

		// This does not take into account the winding
		// rule and therefore holes
		for (int k = 0; k < copyPoints.size() - 1; k++) {
			final double ik = copyPoints.get(k).getX();
			final double jk = copyPoints.get(k).getY();
			final double ik1 = copyPoints.get(k + 1).getX();
			final double jk1 = copyPoints.get(k + 1).getY();

			area += ik * jk1 - ik1 * jk;
		}

		return Math.abs(0.5 * area);
	}
	
	public static boolean isClosed(List<Point2d> points) {
		if (points.size() > 0 && points.get(0).getX() == points.get(points.size() - 1).getX()
				&& points.get(0).getY() == points.get(points.size() - 1).getY())
			return true;
		return false;
	}
	
	public static Point2d center(Point2d p1, Point2d p2) {
		double x = ((double) p1.getX() + (double) p2.getX()) / 2.d;
		double y = ((double) p1.getY() + (double) p2.getY()) / 2.d;
		return new Point2dImpl(x, y);
	}
	
	/**
	 * Function to calculate the center of mass for a given polygon, according
	 * ot the algorithm defined at
	 * http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/
	 * 
	 * @author Christopher Fuhrman (christopher.fuhrman@gmail.com)
	 * @version 2006-09-27
	 * 
	 * @param polyPoints
	 *            array of points in the polygon
	 * @return point that is the center of mass
	 */
	// TODO-AE remove if not needed, Point2D is awt class !!!
	private static Point2D centerOfMass(Point2D[] polyPoints) {
//		double cx = 0, cy = 0;
//		double area = calculateArea(polyPoints);
//		// could change this to Point2D.Float if you want to use less memory
//		Point2D res = new Point2D.Double();
//		int i, j, n = polyPoints.length;
//
//		double factor = 0;
//		for (i = 0; i < n; i++) {
//			j = (i + 1) % n;
//			factor = (polyPoints[i].getX() * polyPoints[j].getY()
//					- polyPoints[j].getX() * polyPoints[i].getY());
//			cx += (polyPoints[i].getX() + polyPoints[j].getX()) * factor;
//			cy += (polyPoints[i].getY() + polyPoints[j].getY()) * factor;
//		}
//		area *= 6.0f;
//		factor = 1 / area;
//		cx *= factor;
//		cy *= factor;
//		res.setLocation(cx, cy);
//		return res;
		return null;
	}
}
