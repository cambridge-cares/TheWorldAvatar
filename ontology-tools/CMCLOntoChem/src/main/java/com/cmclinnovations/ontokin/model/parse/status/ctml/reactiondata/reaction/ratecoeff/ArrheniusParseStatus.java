package com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff;

public class ArrheniusParseStatus {
	boolean arrhenius = false;
	boolean name = false;
	boolean type = false;
	boolean motzWise = false;
	boolean species = false;
	boolean A = false;
	boolean AUnits = false;
	boolean B = false;
	boolean BUnits = false;
	boolean E = false;
	boolean EUnits = false;
	boolean P = false;
	boolean PUnits = false;

	public boolean isArrhenius() {
		return arrhenius;
	}
	public void setArrhenius(boolean arrhenius) {
		this.arrhenius = arrhenius;
	}
	public boolean isName() {
		return name;
	}
	public void setName(boolean name) {
		this.name = name;
	}
	public boolean isType() {
		return type;
	}
	public void setType(boolean type) {
		this.type = type;
	}
	public boolean isMotzWise() {
		return motzWise;
	}
	public void setMotzWise(boolean motzWise) {
		this.motzWise = motzWise;
	}
	public boolean isSpecies() {
		return species;
	}
	public void setSpecies(boolean species) {
		this.species = species;
	}
	public boolean isA() {
		return A;
	}
	public void setA(boolean a) {
		A = a;
	}
	public boolean isE() {
		return E;
	}
	public void setE(boolean e) {
		E = e;
	}
	public boolean isP() {
		return P;
	}
	public void setP(boolean p) {
		P = p;
	}
	public boolean isAUnits() {
		return AUnits;
	}
	public void setAUnits(boolean aUnits) {
		AUnits = aUnits;
	}
	public boolean isEUnits() {
		return EUnits;
	}
	public void setEUnits(boolean eUnits) {
		EUnits = eUnits;
	}
	public boolean isPUnits() {
		return PUnits;
	}
	public void setPUnits(boolean pUnits) {
		PUnits = pUnits;
	}
	public boolean isB() {
		return B;
	}
	public void setB(boolean b) {
		B = b;
	}
	public boolean isBUnits() {
		return BUnits;
	}
	public void setBUnits(boolean bUnits) {
		BUnits = bUnits;
	}
}
