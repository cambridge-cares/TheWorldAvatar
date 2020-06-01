package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

public class SpeciesBean {
	
	String ontocompchemIRI;
	String ontospeciesIRI;
	
	public SpeciesBean(String ontocompchemIRI, String ontospeciesIRI) {
		
		this.ontocompchemIRI=ontocompchemIRI;
		this.ontospeciesIRI=ontospeciesIRI;
	}
	
	
	public String getOntocompchemIRI() {
		return ontocompchemIRI;
	}
	
	public void setOntocompchemIRI(String ontocompchemIRI) {
		this.ontocompchemIRI = ontocompchemIRI;
	}
	
	public String getOntospeciesIRI() {
		return ontospeciesIRI;
	}
	
}
