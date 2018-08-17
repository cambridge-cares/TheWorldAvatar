package uk.ac.ceb.como.molhub.bean;

public class MoleculeProperty {

	private String moleculeId;
	
	private String moleculeName;
	
	private String numberOfAtoms;
	
	public MoleculeProperty() {}
	
	public MoleculeProperty(String moleculeId, String moleculeName, String numberOfAtoms) {
		
		this.moleculeId=moleculeId;
		this.moleculeName=moleculeName;
		this.numberOfAtoms=numberOfAtoms;
	}

public MoleculeProperty(String moleculeId, String moleculeName) {
		
		this.moleculeId=moleculeId;
		this.moleculeName=moleculeName;
	}

public MoleculeProperty(String moleculeName) {
	
	this.moleculeName=moleculeName;
}

	public String getMoleculeId() {
		return moleculeId;
	}

	public void setMoleculeId(String moleculeId) {
		this.moleculeId = moleculeId;
	}

	public String getMoleculeName() {
		return moleculeName;
	}

	public void setMoleculeName(String moleculeName) {
		this.moleculeName = moleculeName;
	}

	public String getNumberOfAtoms() {
		return numberOfAtoms;
	}

	public void setNumberOfAtoms(String numberOfAtoms) {
		this.numberOfAtoms = numberOfAtoms;
	}
	
}
