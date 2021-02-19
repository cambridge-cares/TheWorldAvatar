package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class ObservableParseStatus {
	boolean observable = false;
	boolean id = false;
	boolean variableID = false;

	public boolean isObservable() {
		return observable;
	}

	public void setObservable(boolean observable) {
		this.observable = observable;
	}

	public boolean isID() {
		return id;
	}

	public void setID(boolean id) {
		this.id = id;
	}

	public boolean isVariableID() {
		return variableID;
	}

	public void setVariableID(boolean variableID) {
		this.variableID = variableID;
	}
}
