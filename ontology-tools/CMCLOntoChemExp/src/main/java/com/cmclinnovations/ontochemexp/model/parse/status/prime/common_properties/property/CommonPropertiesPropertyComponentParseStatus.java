package com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ComponentParseStatus;

public class CommonPropertiesPropertyComponentParseStatus extends ComponentParseStatus {
	boolean commonPropertiesPropertyComponent = false;

	public boolean isComponent() {
		return commonPropertiesPropertyComponent;
	}

	public void setComponent(boolean commonPropertiesPropertyComponent) {
		this.commonPropertiesPropertyComponent = commonPropertiesPropertyComponent;
	}
}
