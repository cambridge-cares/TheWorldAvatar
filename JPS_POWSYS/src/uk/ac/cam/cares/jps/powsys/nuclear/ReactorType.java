package uk.ac.cam.cares.jps.powsys.nuclear;

public class ReactorType {
	private String reactortype = "";
	private double capacity = 0;
	
	public ReactorType(String reactortype) {
		this.reactortype = reactortype;
	}
	
	public String getreactor() {
		return reactortype;
	}
	
	public double getcapacity() {
		return capacity;
	}
	
	public void setcapacity(double capacity) {
		this.capacity = capacity;
	}

}
