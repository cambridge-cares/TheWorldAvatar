package uk.ac.cam.cares.jps.dispersion.sensor;

public class AirSensorConfig {
private int numberOfDataSlot=169; //not 1month
private String sensorXLocation="103.859";
private String sensorYLocation="1.27993";
private String sensorZLocation="2";

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
}
