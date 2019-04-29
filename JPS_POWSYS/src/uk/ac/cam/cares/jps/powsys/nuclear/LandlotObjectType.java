package uk.ac.cam.cares.jps.powsys.nuclear;

public class LandlotObjectType {
	private String Landlot="";
	private double x = 0;
	private double y = 0;
	
	public LandlotObjectType(String Landlot) {
		this.Landlot = Landlot;
	}
	
	public String getlot() {
		return Landlot;
	}
	
	public double getx() {
		return x;
	}
	
	public void setx(double x) {
		this.x = x;
	}
	
	public double gety() {
		return y;
	}
	
	public void sety(double y) {
		this.y = y;
	}
	
	

}
