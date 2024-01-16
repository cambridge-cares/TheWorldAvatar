package com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ValueParseStatus;

public class CommonPropertiesPropertyValueParseStatus extends ValueParseStatus {
	boolean commonPropertiesPropertyValue = false;

	public boolean isValue() {
		return commonPropertiesPropertyValue;
	}

	public void setValue(boolean commonPropertiesPropertyValue) {
		this.commonPropertiesPropertyValue = commonPropertiesPropertyValue;
	}
}
