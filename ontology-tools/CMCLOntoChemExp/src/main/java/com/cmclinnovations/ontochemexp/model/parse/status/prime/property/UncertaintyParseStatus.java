package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class UncertaintyParseStatus {
	boolean uncertainty = false;
	boolean bound = false;
	boolean kind = false;
	boolean transformation = false;
	boolean type = false;

	public boolean isUncertainty() {
		return uncertainty;
	}

	public void setUncertainty(boolean uncertainty) {
		this.uncertainty = uncertainty;
	}

	public boolean isBound() {
		return bound;
	}

	public void setBound(boolean bound) {
		this.bound = bound;
	}

	public boolean isKind() {
		return kind;
	}

	public void setKind(boolean kind) {
		this.kind = kind;
	}

	public boolean isTransformation() {
		return transformation;
	}

	public void setTransformation(boolean transformation) {
		this.transformation = transformation;
	}

	public boolean isType() {
		return type;
	}

	public void setType(boolean type) {
		this.type = type;
	}
}
