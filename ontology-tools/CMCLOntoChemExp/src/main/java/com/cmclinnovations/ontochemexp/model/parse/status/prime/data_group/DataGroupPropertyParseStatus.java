package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.PropertyParseStatus;

public class DataGroupPropertyParseStatus extends PropertyParseStatus {
	boolean dataGroupProperty = false;

	public boolean isProperty() {
		return dataGroupProperty;
	}

	public void setProperty(boolean dataGroupProperty) {
		this.dataGroupProperty = dataGroupProperty;
	}
}
