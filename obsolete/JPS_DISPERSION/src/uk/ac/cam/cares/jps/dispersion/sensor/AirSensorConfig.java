package uk.ac.cam.cares.jps.dispersion.sensor;

public class AirSensorConfig {
private int numberOfDataSlot=2017; 
private String sensorXLocation="103.859";
private String sensorYLocation="1.27993";
private String sensorZLocation="2";
//private String rdf4jlocation = "http://localhost/rdf4j-server"; //for Claudius
private String rdf4jlocation = "http://localhost:8080/rdf4j-server"; //for Claudius
//enable the block below for AQMesh Sensor. 
	//private int numberOfDataSlot=2017; //so far 168+1 or 2017 for aqmesh (24*7*60/5+1)
	//private String sensorXLocation="103.786903";
	//private String sensorYLocation="1.336757";
	//private String sensorZLocation="28";

public int getNumberOfDataSlot() {
	return numberOfDataSlot;
}

public void setNumberOfDataSlot(int numberOfDataSlot) {
	this.numberOfDataSlot = numberOfDataSlot;
}

public String getSensorYLocation() {
	return sensorYLocation;
}

public void setSensorYLocation(String sensorYLocation) {
	this.sensorYLocation = sensorYLocation;
}

public String getSensorXLocation() {
	return sensorXLocation;
}

public void setSensorXLocation(String sensorXLocation) {
	this.sensorXLocation = sensorXLocation;
}

public String getSensorZLocation() {
	return sensorZLocation;
}

public void setSensorZLocation(String sensorZLocation) {
	this.sensorZLocation = sensorZLocation;
}
public String getRDF4JLocation() {
	return rdf4jlocation;
}

public void setRDF4Location(String rdf4jlocation ) {
	this.rdf4jlocation  = rdf4jlocation ;
}
}
