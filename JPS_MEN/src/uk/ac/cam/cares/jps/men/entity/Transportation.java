package uk.ac.cam.cares.jps.men.entity;

public class Transportation implements INamed {

	private String name = "";
	// TOOD-AE change to currency
	private double transportationCost = 0;
	// TOOD-AE change to currency
	private double installationCost = 0;
	private double emission = 0;
	
	public Transportation(String name) {
		this.name = name;
	}
	
	public double getTransportationCost() {
		return transportationCost;
	}
	
	public void setTransportationCost(double transportationCost) {
		this.transportationCost = transportationCost;
	}
	
	public double getInstallationCost() {
		return installationCost;
	}
	
	public void setInstallationCost(double installationCost) {
		this.installationCost = installationCost;
	}
	
	public double getEmission() {
		return emission;
	}
	
	public void setEmission(double emission) {
		this.emission = emission;
	}
	
	public String getName() {
		return name;
	}
	
	public String toString() {
		return "Transportation[" + getName() + ", transCost=" + getTransportationCost() 
			+ ", installCost=" + getInstallationCost() + ", emission=" + getEmission() + "]";
	}
}
