package com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.PropertyParseStatus;

public class CommonPropertiesPropertyParseStatus extends PropertyParseStatus {
	boolean commonPropertiesProperty = false;

	public boolean isProperty() {
		return commonPropertiesProperty;
	}

	public void setProperty(boolean commonPropertiesProperty) {
		this.commonPropertiesProperty = commonPropertiesProperty;
	}
}
