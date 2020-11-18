package com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff;

public class RateCoeffArrayParseStatus {
	boolean floatArray = false;
	boolean name = false;
	boolean units = false;
	boolean degreeT = false;
	boolean degreeP = false;
	public boolean isFloatArray() {
		return floatArray;
	}
	public void setFloatArray(boolean floatArray) {
		this.floatArray = floatArray;
	}
	public boolean isName() {
		return name;
	}
	public void setName(boolean name) {
		this.name = name;
	}
	public boolean isUnits() {
		return units;
	}
	public void setUnits(boolean units) {
		this.units = units;
	}
	public boolean isDegreeT() {
		return degreeT;
	}
	public void setDegreeT(boolean degreeT) {
		this.degreeT = degreeT;
	}
	public boolean isDegreeP() {
		return degreeP;
	}
	public void setDegreeP(boolean degreeP) {
		this.degreeP = degreeP;
	}
}
