package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class UncertaintyParseStatus {
	boolean uncertainty = false;

	public boolean isUncertainty() {
		return uncertainty;
	}

	public void setUncertainty(boolean uncertainty) {
		this.uncertainty = uncertainty;
	}

	boolean bound = false;

	public boolean isBound() {
		return bound;
	}

	public void setBound(boolean bound) {
		this.bound = bound;
	}

	boolean kind = false;

	public boolean isKind() {
		return kind;
	}

	public void setKind(boolean kind) {
		this.kind = kind;
	}

	boolean transformation = false;

	public boolean isTransformation() {
		return transformation;
	}

	public void setTransformation(boolean transformation) {
		this.transformation = transformation;
	}
}
