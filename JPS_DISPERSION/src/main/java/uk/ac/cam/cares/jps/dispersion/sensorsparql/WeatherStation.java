package uk.ac.cam.cares.jps.dispersion.sensorsparql;

import org.json.JSONObject;

public class WeatherStation implements Comparable<WeatherStation>{
	private String stationiri;
	private double xcoord;
	private double ycoord;
	private double zcoord;
	// units are set according to requirements of Episode
	private double cloudcover; //fraction
	private double precipitation; //mm
	private double pressure; //mbar
	private double temperature; //celcius
	private double humidity; //percentage
	private double windspeed; // m/s
	private double winddirection; //degree
	private long timestamp; // unix timestamp
	private double distance; // distance to centre

	public WeatherStation() {}
	
	public WeatherStation(String stationiri) {
		JSONObject queryresult = SensorSparql.queryWeatherStationProperties(stationiri);
		this.stationiri = stationiri;
		this.xcoord = queryresult.getDouble("xval");
		this.ycoord = queryresult.getDouble("yval");
		this.zcoord = queryresult.getDouble("zval");
		this.cloudcover = queryresult.getDouble("vcloud");
		this.humidity = queryresult.getDouble("vhumidity");
		this.precipitation = queryresult.getDouble("vprecip");
		this.pressure = queryresult.getDouble("vpressure");
		this.temperature = queryresult.getDouble("vtemp");
		this.timestamp = queryresult.getLong("vtime");
		this.winddirection = queryresult.getDouble("vwindspeed");
		this.windspeed = queryresult.getDouble("vwinddirection");
	}
	
	public double getXcoord() {
		return xcoord;
	}
	public void setXcoord(double xcoord) {
		this.xcoord = xcoord;
	}
	public double getYcoord() {
		return ycoord;
	}
	public void setYcoord(double ycoord) {
		this.ycoord = ycoord;
	}
	public double getZcoord() {
		return zcoord;
	}
	public void setZcoord(double zcoord) {
		this.zcoord = zcoord;
	}
	public double getCloudcover() {
		return cloudcover;
	}
	public void setCloudcover(double cloudcover) {
		this.cloudcover = cloudcover;
	}
	public double getPrecipitation() {
		return precipitation;
	}
	public void setPrecipitation(double precipitation) {
		this.precipitation = precipitation;
	}
	public double getPressure() {
		return pressure;
	}
	public void setPressure(double pressure) {
		this.pressure = pressure;
	}
	public double getTemperature() {
		return temperature;
	}
	public void setTemperature(double temperature) {
		this.temperature = temperature;
	}
	public double getHumidity() {
		return humidity;
	}
	public void setHumidity(double humidity) {
		this.humidity = humidity;
	}
	public double getWindspeed() {
		return windspeed;
	}
	public void setWindspeed(double windspeed) {
		this.windspeed = windspeed;
	}
	public double getWinddirection() {
		return winddirection;
	}
	public void setWinddirection(double winddirection) {
		this.winddirection = winddirection;
	}
	public String getStationiri() {
		return stationiri;
	}
	public void setStationiri(String stationiri) {
		this.stationiri = stationiri;
	}
	public long getTimestamp() {
		return timestamp;
	}
	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}
	public double getDistance() {
		return distance;
	}

	public void setDistance(double distance) {
		this.distance = distance;
	}
	
	@Override
	public int compareTo(WeatherStation compareStation) {
		double compareDistance = ((WeatherStation) compareStation).getDistance();
		// ascending order
		return Double.compare(this.distance, compareDistance);
	}

	
}