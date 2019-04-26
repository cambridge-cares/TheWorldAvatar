package uk.ac.cam.cares.jps.powsys.nuclear;

public class NuclearGenType {
	private String nucleargentype = "";
	private double capacity = 0;
	
	public NuclearGenType(String NuclearGenType) {
		this.nucleargentype = NuclearGenType;
	}
	
	public String getnucleargen() {
		return nucleargentype;
	}
	
	public double getcapacity() {
		return capacity;
	}
	public void setcapacity(double capacity) {
		this.capacity = capacity;
	}
}
