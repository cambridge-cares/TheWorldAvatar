package uk.ac.cam.cares.jps.visualization;

public class MapPoint {
	protected double lat;
	protected double lon;
	protected double alt;
	protected long time;
	protected String name = null;
	
	/**
	 * Create a point with no altitude.
	 * @param latitude
	 * @param longitude
	 */
	public  MapPoint(double latitude, double longitude) {
		lat = latitude;
		lon = longitude;
	}
	
	/**
	 * Create a point with name.
	 * @param latitude
	 * @param longitude
	 * @param altitude
	 * @param name
	 */
	public  MapPoint(double latitude, double longitude, double altitude,String nameofpoint) {
		lat = latitude;
		lon = longitude;
		name=nameofpoint;
		alt=altitude;
	}
	
	/**
	 * Create a point with name.
	 * @param latitude
	 * @param longitude
	 * @param name
	 */
	public  MapPoint(double latitude, double longitude,String nameofpoint) {
		lat = latitude;
		lon = longitude;
		name=nameofpoint;
	}
	
	/**
	 * Create a point.
	 * @param latitude
	 * @param longitude
	 * @param altitude
	 */
	public  MapPoint(double latitude, double longitude, double altitude) {
		lat = latitude;
		lon = longitude;
		alt = altitude;
	}
	
	/**
	 * Create a point with an associated time.
	 * @param latitude
	 * @param longitude
	 * @param altitude
	 * @param time
	 */
	public  MapPoint(double latitude, double longitude, double altitude, long time) {
		lat = latitude;
		lon = longitude;
		alt = altitude;
		this.time = time;
	}
	
	/**
	 * Create a point with an associated name and time.
	 * @param latitude
	 * @param longitude
	 * @param altitude
	 * @param time
	 * @param name
	 */
	public  MapPoint(double latitude, double longitude, double altitude, long time, String name) {
		lat = latitude;
		lon = longitude;
		alt = altitude;
		this.time = time;
		this.name = name;
	}
	
	/**
	 * Get the latitude of this point.
	 * @return
	 */
	public  double getLatitude() {
		return lat;
	}
	
	/**
	 * Get the longitude of this point.
	 * @return
	 */
	public  double getLongitude() {
		return lon;
	}
	
	/**
	 * Get the altitude of this point.
	 * @return
	 */
	public  double getAltitude() {
		return alt;
	}
	
	/**
	 * Get the time associated with this point.
	 * @return
	 */
	public  long getTime() {
		return time;
	}
	
	/**
	 * Get the name associated with this point.
	 * @return
	 */
	public  String getName() {
		return name;
	}
	
	/**
	 * Associate a name with this point.
	 * @param name
	 */
	public  void setName(String name) {
		this.name = name;
	}
	
	/**
	 * Associate a time with this point.
	 * @param time
	 */
	public  void setTime(long time) {
		this.time = time;
	}
	
	/**
	 * Get a human readable representation of this point;
	 */
	@Override
	public  String toString() {
		return name + ": " + lat + ", " + lon + ", " + alt + ", " + time;
	}

}
