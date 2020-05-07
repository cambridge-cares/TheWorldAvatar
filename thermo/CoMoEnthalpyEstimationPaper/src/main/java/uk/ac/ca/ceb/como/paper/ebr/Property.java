package uk.ac.ca.ceb.como.paper.ebr;
/**
 * Properties to compare with the input values.
 * @author msff2
 *
 */
public enum Property {
	EBR_PROCESS_CROSS_VALIDATION("cross-validation"),
	EBR_PROCESS_CALCULATION("calculation");
	private String name;
	private Property(String name){
		this.name = name;
	}
	
	public String getName(){
		return name;
	}
}
