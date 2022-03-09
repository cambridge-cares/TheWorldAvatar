package ontology_creator;

import java.util.HashSet;
import java.util.Set;

public class Reaction {
	private String name = "";
	private Set<String> reactants = new HashSet<String>();
	private Set<String> products = new HashSet<String>();
	
	Reaction(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	public Set<String> getReactants() {
		return reactants;
	}
	public Set<String> getProducts() {
		return products;
	}
	public void setName(String name) {
		this.name = name;
	}
	public void setReactants(Set<String> reactants) {
		this.reactants = reactants;
	}
	
	public void addReactant(String reactant) {
		this.reactants.add(reactant);
	}
	
	public void setProducts(Set<String> products) {
		this.products = products;
	}
	
	public void addProduct(String product) {
		this.products.add(product);
	}
	
	public boolean isComplete() {
		boolean complete = false;
		if (!this.name.isEmpty() || !this.reactants.isEmpty() || !this.products.isEmpty()) {
			complete = true;
		}
		return complete;
	}
	

}
