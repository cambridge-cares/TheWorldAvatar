package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class NameParseStatus {
	boolean name = false;

	public boolean isName() {
		return name;
	}

	public void setName(boolean name) {
		this.name = name;
	}

	boolean source = false;

	public boolean isSource() {
		return source;
	}

	public void setSource(boolean source) {
		this.source = source;
	}

	boolean type = false;

	public boolean isType() {
		return type;
	}

	public void setType(boolean type) {
		this.type = type;
	}

	boolean descriptor = false;

	public boolean isDescriptor() {
		return descriptor;
	}

	public void setDescriptor(boolean descriptor) {
		this.descriptor = descriptor;
	}
}
