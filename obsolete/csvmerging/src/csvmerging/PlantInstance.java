package csvmerging;

public class PlantInstance {

	private String name = "";
	private double capacity = 0;
	private double x = 0.0;
	private double y = 0.0;
	private double anngen = 0.0;
	private String country = "";
	private String Owner="";
	private String tech = "";
	private String fuel = "";
	private String specfuel = "";
	private int id = 0;
	private int year = 0;
	
	public PlantInstance(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public double getCapacity() {
		return capacity;
	}
	
	public void setCapacity(double capacity) {
		this.capacity = capacity;
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
	
	public double getanngen() {
		return anngen;
	}
	
	public void setanngen(double anngen) {
		this.anngen = anngen;
	}

	public String getCountry() {
		return country;
	}
	
	public void setCountry(String country) {
		this.country = country;
	}
	
	public String getOwner() {
		return Owner;
	}
	
	public void setOwner(String Owner) {
		this.Owner = Owner;
	}
	
	public int getLineID() {
		return id;
	}
	
	public void setLineID(int id) {
		this.id = id;
	}
	
	public String getTechnology() {
		return tech;
	}
	
	public void setTechnology(String tech) {
		this.tech = tech;
	}
	
	public String getGeneralFuel() {
		return fuel;
	}
	
	public void setGeneralFuel(String fuel) {
		this.fuel = fuel;
	}
	
	public String getSpecificFuel() {
		return specfuel;
	}
	
	public void setSpecificFuel(String specfuel) {
		this.specfuel = specfuel;
	}
	
	public int getYear() {
		return year;
	}
	
	public void setYear(int year) {
		this.year = year;
	}
}
