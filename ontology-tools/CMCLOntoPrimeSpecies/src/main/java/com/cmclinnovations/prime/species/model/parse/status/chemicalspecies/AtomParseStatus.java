package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class AtomParseStatus {
	boolean atom = false;

	public boolean isAtom() {
		return atom;
	}

	public void setAtom(boolean atom) {
		this.atom = atom;
	}

	boolean symbol = false;

	public boolean isSymbol() {
		return symbol;
	}

	public void setSymbol(boolean symbol) {
		this.symbol = symbol;
	}

	boolean isotope = false;

	public boolean isIsotope() {
		return isotope;
	}

	public void setIsotope(boolean isotope) {
		this.isotope = isotope;
	}
}
