package uk.ac.cam.cares.jps.building;

import java.util.ArrayList;
import java.util.List;

import org.openimaj.math.geometry.point.Point2d;
import org.openimaj.math.geometry.point.Point2dImpl;
import org.openimaj.math.geometry.shape.Polygon;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

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
	// TODO-AE remove this method if not needed anymore
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
		return Math.abs(calculateSignedArea(points));
	}
	
	public static double calculateSignedArea(List<Point2d> points) {
		
		List<Point2d> closedPoints = close(points);
		// This does not take into account the winding
		// rule and therefore holes
		double area = 0;
		for (int k = 0; k < closedPoints.size() - 1; k++) {
			final double ik = closedPoints.get(k).getX();
			final double jk = closedPoints.get(k).getY();
			final double ik1 = closedPoints.get(k + 1).getX();
			final double jk1 = closedPoints.get(k + 1).getY();

			area += ik * jk1 - ik1 * jk;
		}

		return 0.5 * area;
	}
	
	public static List<Point2d> close(List<Point2d> points) {
		final boolean closed = PolygonUtil.isClosed(points);
	
		List<Point2d> closedList = new ArrayList<Point2d>(points);
		if (!closed && points.size() > 0) {	
			closedList = new ArrayList<Point2d>(points);
			closedList.add(points.get(0));
		}

		return closedList;
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
	 * This method was adapted for JPS.
	 * BUT: this method seems to calculate nonsense if the orientation of
	 * the polygon is clockwise
	 * 
	 * 
	 * Function to calculate the center of mass for a given polygon, according
	 * to the algorithm defined at
	 * http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/
	 * 
	 * @author Christopher Fuhrman (christopher.fuhrman@gmail.com)
	 * @version 2006-09-27
	 * 
	 * @param points in the polygon
	 * @return point that is the center of mass
	 */
	// TODO-AE: remove this method 
	private static Point2d calculateCenterOfMassWRONG(List<Point2d> points) {
		double cx = 0, cy = 0;
		double area = calculateSignedArea(points);
		
		int i, j, n = points.size();

		double factor = 0;
		for (i = 0; i < n; i++) {
			j = (i + 1) % n;
			factor = (points.get(i).getX() * points.get(j).getY()
					- points.get(j).getX() * points.get(i).getY());
			cx += (points.get(i).getX() + points.get(j).getX()) * factor;
			cy += (points.get(i).getY() + points.get(j).getY()) * factor;
		}
		area *= 6.0f;
		factor = 1 / area;
		cx *= factor;
		cy *= factor;
		return new Point2dImpl(cx, cy);
	}
	
	/**
	 * All x and y values must be nonnegative
	 * 
	 * @param points
	 * @return
	 */
	public static Point2d calculateCenterOfMass(List<Point2d> points) {
		
		points = close(points);
		
		int totvertices = points.size();
		double[] x = new double[totvertices];
		double[] y = new double[totvertices];
		double Cx = 0;
		double Cy = 0;
		
		for (int a = 0; a < totvertices; a++) {
			x[a] = points.get(a).getX();
			y[a] = points.get(a).getY();
			
			if ((x[a] < 0) || (y[a] < 0)) {
				throw new JPSRuntimeException("x and y values must be nonnegative for calculation of center of mass, points = " + points);
			}
		}
		
		double A = calculateSignedArea(points);
	
		// calculation of the Cx
		for (int a = 0; a < totvertices - 1; a++) {
			Cx += (x[a] + x[a + 1]) * (x[a] * y[a + 1] - x[a + 1] * y[a]);
		}
		// calculation of the Cy
		for (int a = 0; a < totvertices - 1; a++) {
			Cy += (y[a] + y[a + 1]) * (x[a] * y[a + 1] - x[a + 1] * y[a]);

		}

		return new Point2dImpl(Cx / 6. / A, Cy / 6. / A);
	}
	
	
	public static double calculatePerimeter(Polygon p) {
		
		double perimeter = 0;
		List<Point2d> closedPoints = close(p.getVertices());
		for (int i=0; i<closedPoints.size() - 1; i++) {
			perimeter += length(closedPoints.get(i+1).minus(closedPoints.get(i)));
		}
		
		return perimeter;
	}
	
	public static Polygon createBestFittingBoxAlongAllPolygonLines(List<Polygon> polygons) {
		
		double area = area(polygons);
		
		double intersectionAreaMax = -1;
		Polygon bestBox = null;
		for (Polygon current : polygons) {
			for (int i=0; i<current.size()-1; i++) {
				
				Polygon box = createBestFittingBoxAlongGivenLine(polygons, area, current.get(i), current.get(i+1));
				double intersectionArea = calculateIntersectionArea(polygons, box);
				if (intersectionArea > intersectionAreaMax) {
					intersectionAreaMax = intersectionArea;
					bestBox = box;
				}	
			}
		}
		
		return bestBox;
	}
	
	public static Point2d add(Point2d p1, Point2d p2) {
		return new Point2dImpl(p1.getX() + p2.getX(), p1.getY() + p2.getY());
	}
	
	public static Polygon createBestFittingBoxAlongGivenLine(List<Polygon> polygons, double area, Point2d startPoint, Point2d endPoint) {
		Point2d diff = endPoint.minus(startPoint);
		double length = length(diff);
		double width = area / length;
		double xortho = diff.getY() * width / length;
		double yortho = -diff.getX() * width / length;
		
		double intersectionAreaMax = -1;
		Polygon bestBox = null;
		for (int i=0; i<=1; i++) {
			if (i==1) {
				xortho = -xortho;
				yortho = -yortho;
			}
			
			Point2d ortho = new Point2dImpl(xortho, yortho);
			Polygon box = new Polygon(startPoint, endPoint, add(endPoint, ortho), add(startPoint, ortho), startPoint);
			double intersectionArea = calculateIntersectionArea(polygons, box);
			if (intersectionArea > intersectionAreaMax) {
				intersectionAreaMax = intersectionArea;
				bestBox = box;
			}	
		}
		
		
	
		return bestBox;
	}
}
