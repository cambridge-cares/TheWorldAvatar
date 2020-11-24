package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class PropertyParseStatus {
	boolean property = false;
	boolean name = false;
	boolean id = false;
	boolean label = false;
	boolean units = false;
	boolean description = false;
	boolean derivedPropertyExists = false;

	public boolean isProperty() {
		return property;
	}

	public void setProperty(boolean property) {
		this.property = property;
	}

	public boolean isName() {
		return name;
	}

	public void setName(boolean name) {
		this.name = name;
	}

	public boolean isID() {
		return id;
	}

	public void setID(boolean id) {
		this.id = id;
	}

	public boolean isLabel() {
		return label;
	}

	public void setLabel(boolean label) {
		this.label = label;
	}

	public boolean isUnits() {
		return units;
	}

	public void setUnits(boolean units) {
		this.units = units;
	}

	public boolean isDescription() {
		return description;
	}

	public void setDescription(boolean description) {
		this.description = description;
	}
	
	public boolean isDerivedPropertyExists() {
		return derivedPropertyExists;
	}
	
	public void setDerivedPropertyExists(boolean derivedPropertyExists) {
		this.derivedPropertyExists = derivedPropertyExists;
	}
}
