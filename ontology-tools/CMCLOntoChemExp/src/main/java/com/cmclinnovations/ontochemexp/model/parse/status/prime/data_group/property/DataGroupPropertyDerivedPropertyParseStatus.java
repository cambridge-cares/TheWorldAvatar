package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.DerivedPropertyParseStatus;

public class DataGroupPropertyDerivedPropertyParseStatus extends DerivedPropertyParseStatus {
	boolean dataGroupPropertyDerivedProperty = false;

	public boolean isDerivedProperty() {
		return dataGroupPropertyDerivedProperty;
	}

	public void setDerivedProperty(boolean dataGroupPropertyDerivedProperty) {
		this.dataGroupPropertyDerivedProperty = dataGroupPropertyDerivedProperty;
	}
}
