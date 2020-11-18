package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class IndicatorParseStatus {
	boolean indicator = false;
	boolean id = false;
	boolean transformation = false;
	boolean variableID = false;

	public boolean isIndicator() {
		return indicator;
	}

	public void setIndicator(boolean indicator) {
		this.indicator = indicator;
	}

	public boolean isID() {
		return id;
	}

	public void setID(boolean id) {
		this.id = id;
	}

	public boolean isTransformation() {
		return transformation;
	}

	public void setTransformation(boolean transformation) {
		this.transformation = transformation;
	}

	public boolean isVariableID() {
		return variableID;
	}

	public void setVariableID(boolean variableID) {
		this.variableID = variableID;
	}
}
