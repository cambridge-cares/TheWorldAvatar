package csvmerging;

public class PlantInstance {

	private String name = "";
	private double capacity = 0;
	private String country = "";
	private String tech = "";
	private String fuel = "";
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

	public String getCountry() {
		return country;
	}
	
	public void setCountry(String country) {
		this.country = country;
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
	
	public int getYear() {
		return year;
	}
	
	public void setYear(int year) {
		this.year = year;
	}
}
