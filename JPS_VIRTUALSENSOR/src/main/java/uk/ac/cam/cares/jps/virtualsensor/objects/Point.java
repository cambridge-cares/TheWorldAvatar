package uk.ac.cam.cares.jps.virtualsensor.objects;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Point {
    private double x;
    private double y;
    private String srsname;
    
    public Point(Point p) { // copy constructor
    	this.x = p.getX();
    	this.y = p.getY();
    	this.srsname = p.getSrsname();
    }
    
    public Point() {}
    
	public double getX() {
		return x;
	}
	public void setX(double x) {
		this.x = x;
	}
	public double getY() {
		return y;
	}
	public void setY(double y) {
		this.y = y;
	}
	public String getSrsname() {
		return srsname;
	}
	public void setSrsname(String srsname) {
		this.srsname = srsname;
	}
	
	public void transform(String targetCRS) {
		double[] new_xy = CRSTransformer.transform(this.srsname, targetCRS, new double[] {this.x,this.y});
		this.x = new_xy[0];
		this.y = new_xy[1];
		this.srsname = targetCRS;
	}
	
	/**
	 * calculate distance to p2
	 * @param p2
	 * @return
	 */
	public double distance(Point p2) {
		Point p1_copy = this;
		Point p2_copy = p2;
		
		p1_copy.transform(CRSTransformer.EPSG_4326);
		p2_copy.transform(CRSTransformer.EPSG_4326);
		
		double lat1 = p1_copy.getY();
		double lon1 = p1_copy.getX();
		double lat2 = p2_copy.getY();
		double lon2 = p2_copy.getX();
		if ((lat1 == lat2) && (lon1 == lon2)) {
			return 0;
		}
		else {
			double theta = lon1 - lon2;
			double dist = Math.sin(Math.toRadians(lat1)) * Math.sin(Math.toRadians(lat2)) + Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.cos(Math.toRadians(theta));
			dist = Math.acos(dist);
			dist = Math.toDegrees(dist);
			dist = dist * 60 * 1.1515;
//			if (unit.equals("K")) {
				dist = dist * 1.609344;
//			} else if (unit.equals("N")) {
//				dist = dist * 0.8684;
//			}
			return dist;
		}
	}
}
