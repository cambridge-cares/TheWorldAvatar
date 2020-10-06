package uk.ac.cam.cares.jps.men.entity;

public class Product implements INamed {

	private String name = "";
	private double capacity = 0;
	private double price = 0;

	public Product(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
	public double getCapacity() {
		return capacity;
	}

	/**
	 * @param capacity in t/year
	 */
	public void setCapacity(double capacity) {
		this.capacity = capacity;
	}

	public double getPrice() {
		return price;
	}
	
	public void setPrice(double price) {
		this.price = price;
	}
	
	public boolean equalProduct(Product product) {
		return (product != null) && this.getName().equals(product.getName());
	}
}
