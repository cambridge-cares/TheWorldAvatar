package uk.ac.cam.cares.jps.men.entity;

public class Sink implements INamed {

	private String name = "";
	// if a named sink such as a consuming plant consumes more than one product, then for each product an own instance of Sink is created
	private Product product = null;
	private boolean nearSea = false;
	
	public Sink(String name, Product product) {
		this.name = name;
		this.product = product;
	}
	
	public String getName() {
		return name;
	}
	
	public Product getProduct() {
		return product;
	}

	public boolean isNearSea() {
		return nearSea;
	}

	public void setNearSea(boolean nearSea) {
		this.nearSea = nearSea;
	}
	
	public String toString( ) {
		return "Sink[name=" + getName() + ", raw material=" + getProduct().getName() + ", nearSea = " + isNearSea() + "]"; //product need to be changed by consumed (raw material needed)
	}
}
