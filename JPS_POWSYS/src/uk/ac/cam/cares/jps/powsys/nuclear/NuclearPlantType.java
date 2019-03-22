package uk.ac.cam.cares.jps.powsys.nuclear;

public class NuclearPlantType {
	private String nuclearplanttype = "";
	private double capacity = 0;
	private int numberreactora=0;
	private int numberreactorb=0;
	private double x = 0;
	private double y = 0;
	
	public NuclearPlantType(String nuclearplanttype) {
		this.nuclearplanttype = nuclearplanttype;
	}
	
	public String getnuclearplant() {
		return nuclearplanttype;
	}
	
	public double getcapacity() {
		return capacity;
	}
	
	public void setcapacity(double capacity) {
		this.capacity = capacity;
	}
	
	public int getnumberreactora() {
		return numberreactora;
	}
	public int getnumberreactorb() {
		return numberreactorb;
	}
	
	public void setnumberreactora(int numberreactora) {
		this.numberreactora = numberreactora;
	}
	public void setnumberreactorb(int numberreactorb) {
		this.numberreactorb = numberreactorb;
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
