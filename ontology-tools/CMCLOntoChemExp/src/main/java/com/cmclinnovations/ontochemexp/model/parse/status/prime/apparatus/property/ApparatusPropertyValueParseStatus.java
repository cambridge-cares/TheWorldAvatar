package com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ValueParseStatus;

public class ApparatusPropertyValueParseStatus extends ValueParseStatus {
	boolean apparatusPropertyValue = false;

	public boolean isValue() {
		return apparatusPropertyValue;
	}

	public void setValue(boolean apparatusPropertyValue) {
		this.apparatusPropertyValue = apparatusPropertyValue;
	}
}
