package uk.ac.ceb.como.molhub.model;

import uk.ac.ceb.como.molhub.bean.MoleculeProperty;

public class SearchResult {

	private String id;
	
	private MoleculeProperty moleculeProperty;
	
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public MoleculeProperty getMoleculeProperty() {
		return moleculeProperty;
	}

	public void setMoleculeProperty(MoleculeProperty moleculeProperty) {
		this.moleculeProperty = moleculeProperty;
	}

	
	
}