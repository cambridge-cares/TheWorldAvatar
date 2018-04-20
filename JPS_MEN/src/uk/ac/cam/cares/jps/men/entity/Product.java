package uk.ac.cam.cares.jps.men.entity;

// TODO-AE rename to Ressource or whatever fits to EIP ontology
public class Product implements INamed {

	private String name = "";
	// TODO-AE change to physical quantity f
	private double capacity = 0;
	// TODO-AE change to currency object
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
