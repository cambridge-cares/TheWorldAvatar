package uk.ac.ceb.como.molhub.bean;
/**
 * 
 * @author nk510
 * Represents a query term that will be used in search box. For example: "(H2 and O) or Cl2"
 */
public class Term {

	private String id;
	private String name;
	
	
	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public String toString() {
		
		return  getName();
	}
	
}