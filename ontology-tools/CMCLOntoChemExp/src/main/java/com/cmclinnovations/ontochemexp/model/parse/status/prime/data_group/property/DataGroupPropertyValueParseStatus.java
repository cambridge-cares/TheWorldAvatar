package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ValueParseStatus;

public class DataGroupPropertyValueParseStatus extends ValueParseStatus {
	boolean dataGroupPropertyValue = false;

	public boolean isValue() {
		return dataGroupPropertyValue;
	}

	public void setValue(boolean dataGroupPropertyValue) {
		this.dataGroupPropertyValue = dataGroupPropertyValue;
	}
}
