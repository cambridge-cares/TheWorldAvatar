package uk.ac.cam.cares.jps.powsys.retrofit;

public class BusInfo {
	String busIri;
	double x;
	double y;
	String busNumberIri;
	String busNumber;
	String busType;
	String voltageMagnitudeIri;
	double voltageMagnitude;
	double baseKV;
	
	public String toString() {
		StringBuffer b = new StringBuffer("BusInfo[").append(busIri).append(",").append(x).append(",").append(y).
			append(",").append(busNumberIri).append(",").append(busNumber).append(",").append(busType).
			append(",").append(voltageMagnitude).append("]");
		return b.toString();	
	}
}
