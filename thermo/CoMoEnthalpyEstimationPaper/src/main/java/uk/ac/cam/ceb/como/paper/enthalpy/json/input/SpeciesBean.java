package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

public class SpeciesBean {
	
	
	String levelOfTheory;
	
	String ontocompchemIRI;
	String ontospeciesIRI;

	public SpeciesBean(String ontocompchemIRI, String ontospeciesIRI) {
		
		this.ontocompchemIRI=ontocompchemIRI;
		this.ontospeciesIRI=ontospeciesIRI;
	}
	
	public SpeciesBean(String levelOfTheory, String ontocompchemIRI, String ontospeciesIRI) {
		
		this.levelOfTheory=levelOfTheory;
		this.ontocompchemIRI=ontocompchemIRI;
		this.ontospeciesIRI=ontospeciesIRI;
	}
	
	public String getLevelOfTheory() {
		return levelOfTheory;
	}
	
	public void setLevelOfTheory(String levelOfTheory) {
		this.levelOfTheory = levelOfTheory;
	}
	
	public void setOntospeciesIRI(String ontospeciesIRI) {
		this.ontospeciesIRI = ontospeciesIRI;
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
