package uk.ac.ceb.como.molhub.bean;

public class MoleculeProperty {

	private String moleculeId;
	
	private String moleculeName;
	
	public MoleculeProperty() {}
	
	public MoleculeProperty(String moleculeId, String moleculeName) {
		
		this.moleculeId=moleculeId;
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
	
}
