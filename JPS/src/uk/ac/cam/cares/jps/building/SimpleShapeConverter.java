package uk.ac.cam.cares.jps.building;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.openimaj.math.geometry.point.Point2d;
import org.openimaj.math.geometry.point.Point2dImpl;
import org.openimaj.math.geometry.shape.Polygon;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.MatrixToJsonConverter;

/**
 * @author Andreas
 *
 */
/**
 * @author Andreas
 *
 */
public class SimpleShapeConverter {

	public static class SimpleShape {
		
		/**
		 * shapeType = 0 means box, = 1 means circle
		 */
		public int shapeType = 0;
		public double centerX;
		public double centerY;
		public double length;
		public double width;
		/**
		 * between -90 and +90 degree
		 */
		public double angle;

		@Override
		public String toString() {
			String s = "SimpleShape[type=" + shapeType + ",centerX=" + centerX + ",centerY=" + centerY
					+ ",lenght=" + length + ",widht=" + width + ",angle=" + angle;
			return s;
		}
	}
	
	public static Polygon convertTo2DPolygon(String csvCoordinates, String xName, String yName) {
		
		Polygon result = new Polygon();
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(csvCoordinates);
		
		List<String> xColumn = map.get(xName);
		List<String> yColumn = map.get(yName);
		
		for (int i=0; i<xColumn.size(); i++) {
			float x = Float.valueOf(xColumn.get(i));
			float y = Float.valueOf(yColumn.get(i));
			result.addVertex(x, y);
		}
	
		return result;
	}
	
	public static List<Polygon> convertTo2DPolygons(String csvCoordinates, String pName, String xName, String yName) {
		
		List<Polygon> result = new ArrayList<Polygon>();
		
		if ((pName == null) || (pName.isEmpty())) {
			
			Polygon polygon = convertTo2DPolygon(csvCoordinates, xName, yName);
			result.add(polygon);
			
		} else {
			
			Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(csvCoordinates);
			
			List<String> pColumn = map.get(pName);
			List<String> xColumn = map.get(xName);
			List<String> yColumn = map.get(yName);
			
			Map<String, Polygon> polygonMap = new HashMap<String, Polygon>();
			
			for (int i=0; i<pColumn.size(); i++) {
				String pKey = (String) pColumn.get(i);
				float x = Float.valueOf(xColumn.get(i));
				float y = Float.valueOf(yColumn.get(i));
				Point2d point = new Point2dImpl(x, y);
				if (!polygonMap.containsKey(pKey)) {
					Polygon polygon = new Polygon(point);
					polygonMap.put(pKey, polygon);
				} else {
					polygonMap.get(pKey).addVertex(point);
				}
			}
			
			result.addAll(polygonMap.values());
		}
	
		return result;
	}
	
	public static SimpleShape simplifyShapes(List<Polygon> polygons) {
		
		SimpleShape result = new SimpleShape();
		
		double area = PolygonUtil.area(polygons);
		
		// TODO-AE URGENT URGENT check with Kevin 
		// whether circle only applies to single polygon
		// also get the perimeter calculation
		// what is needed: center and length and type = 1 only?
		if ((polygons.size() == 1))  {
			
//			double perimeter;
//			if (isCircular(area, perimeter)) {
//				result.type = 1
//				return result;	
//			}
		}
		
		
		result.shapeType = 0;
		
		Polygon box = PolygonUtil.createBestShrinkedBox(polygons);
		
		Point2d diff = box.get(1).minus(box.get(0));
		result.length = PolygonUtil.length(diff);
		result.width = area / result.length;
		
		System.out.println(box);
		
		// the method calculateCentroid() calculates wrong value for large coordinates
		//Point2d centroid = box.calculateCentroid();
		
		Point2d centroid = PolygonUtil.center(box.get(0), box.get(2));
		result.centerX = centroid.getX();
		result.centerY = centroid.getY();
		
		// TODO-AE URGENT URGENT Kevin --> angle
		// what does e.g. angle = 45 degree mean. In which direction the box is rotated?
		// check the sign of the angle. 
		result.angle = - calculateAngleBetweenVectorToXAxis(box.get(0).getX(), box.get(0).getY(), box.get(1).getX(), box.get(1).getY());
		
		return result;
	}
	
	public static Polygon createPolygon(SimpleShape shape) {
		
		if (shape.shapeType != 0) {
			throw new JPSRuntimeException("Can't create Polygon for shape type = " + shape.shapeType);
		}
		
		Polygon result = new Polygon();
		
		double dX = shape.length / 2.;
		double dY = shape.width / 2.;
		
		result.addVertex(new Point2dImpl(shape.centerX -  dX, shape.centerY - dY));
		result.addVertex(new Point2dImpl(shape.centerX -  dX, shape.centerY + dY));
		result.addVertex(new Point2dImpl(shape.centerX +  dX, shape.centerY + dY));
		result.addVertex(new Point2dImpl(shape.centerX +  dX, shape.centerY - dY));
		result.addVertex(new Point2dImpl(shape.centerX -  dX, shape.centerY - dY));
		
		result.rotate(new Point2dImpl(shape.centerX,  shape.centerY), shape.angle / 180. * Math.PI);
		
		return result;
	}
	
	/**
	 * Calculates the angle (in degree) between a vector and the x axis. If xstart <= xend, the the vector is given by
	 * end point minus start point else by start point minus end point. 
	 * 
	 * Example. (1,1) and (0,0) will result into angle = 45 degree
	 * 
	 * @param xstart x of start point
	 * @param ystart y of start point
	 * @param xend x of end point
	 * @param yend y of end point
	 * @return
	 */
	public static double calculateAngleBetweenVectorToXAxis(double xstart, double ystart, double xend, double yend) {
		
		double xdiff = xend - xstart;
		double ydiff = yend - ystart;
		
		if (xstart > xend) {
			xdiff = -xdiff;
			ydiff = -ydiff;
		} 
		
		// general formula for calculating the (positive or negative) angle between two vectors a and b:
		// angle = atan2( a.x*b.y - a.y*b.x, a.x*b.x + a.y*b.y )
		// In general the angle will be between -pi and pi but since xdiff is always non-negative
		// the angle will be between -pi/2 and pi/2
		// in the following: a = (xdiff, ydiff) and b = (1,0)
		
		double angle = Math.atan2(-ydiff, xdiff);
		return angle / Math.PI * 180;
	}
}
