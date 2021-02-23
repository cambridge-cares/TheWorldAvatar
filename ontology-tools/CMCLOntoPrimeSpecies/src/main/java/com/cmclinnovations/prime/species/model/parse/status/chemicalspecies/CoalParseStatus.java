package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class CoalParseStatus {
	boolean coal = false;

	public boolean isCoal() {
		return coal;
	}

	public void setCoal(boolean coal) {
		this.coal = coal;
	}

	boolean specifiedBy = false;

	public boolean isSpecifiedBy() {
		return specifiedBy;
	}

	public void setSpecifiedBy(boolean specifiedBy) {
		this.specifiedBy = specifiedBy;
	}
}
