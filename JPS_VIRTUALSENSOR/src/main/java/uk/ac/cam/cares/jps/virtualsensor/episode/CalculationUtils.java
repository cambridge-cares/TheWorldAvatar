package uk.ac.cam.cares.jps.virtualsensor.episode;

import java.math.RoundingMode;
import java.text.DecimalFormat;

public class CalculationUtils {
	
	public static double[] calculateCenterPoint(double xup, double yup,double xdown, double ydown) {
		DecimalFormat df = new DecimalFormat("0.0");
		df.setRoundingMode(RoundingMode.HALF_EVEN);
		double xcenter=(xup+xdown)/2;
		double ycenter=(yup+ydown)/2;
		double newdx=Double.valueOf(df.format(xcenter));
		double newdy=Double.valueOf(df.format(ycenter));
		double[]res= {newdx,newdy};
		return res;
	}
	
	public static double distanceWGS84(double lat1, double lon1, double lat2, double lon2, String unit) {
		if ((lat1 == lat2) && (lon1 == lon2)) {
			return 0;
		}
		else {
			double theta = lon1 - lon2;
			double dist = Math.sin(Math.toRadians(lat1)) * Math.sin(Math.toRadians(lat2)) + Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.cos(Math.toRadians(theta));
			dist = Math.acos(dist);
			dist = Math.toDegrees(dist);
			dist = dist * 60 * 1.1515;
			if (unit.equals("K")) {
				dist = dist * 1.609344;
			} else if (unit.equals("N")) {
				dist = dist * 0.8684;
			}
			return (dist);
		}
	}

}
