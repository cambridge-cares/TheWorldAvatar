package com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff;

public class CoverageParseStatus {
	boolean coverage = false;
	boolean species = false;
	boolean a = false;
	boolean aUnits = false;
	boolean m = false;
	boolean mUnits = false;
	boolean e = false;
	boolean eUnits = false;
	public boolean isCoverage() {
		return coverage;
	}
	public void setCoverage(boolean coverage) {
		this.coverage = coverage;
	}
	public boolean isSpecies() {
		return species;
	}
	public void setSpecies(boolean species) {
		this.species = species;
	}
	public boolean isA() {
		return a;
	}
	public void setA(boolean a) {
		this.a = a;
	}
	public boolean isM() {
		return m;
	}
	public void setM(boolean m) {
		this.m = m;
	}
	public boolean isE() {
		return e;
	}
	public void setE(boolean e) {
		this.e = e;
	}
	public boolean isaUnits() {
		return aUnits;
	}
	public void setaUnits(boolean aUnits) {
		this.aUnits = aUnits;
	}
	public boolean ismUnits() {
		return mUnits;
	}
	public void setmUnits(boolean mUnits) {
		this.mUnits = mUnits;
	}
	public boolean iseUnits() {
		return eUnits;
	}
	public void seteUnits(boolean eUnits) {
		this.eUnits = eUnits;
	}
}
