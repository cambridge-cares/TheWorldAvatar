package com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.UncertaintyParseStatus;

public class ComponentUncertaintyParseStatus extends UncertaintyParseStatus {
	boolean commonPropertiesPropertyComponentUncertainty = false;

	public boolean isUncertainty() {
		return commonPropertiesPropertyComponentUncertainty;
	}

	public void setUncertainty(boolean commonPropertiesPropertyComponentUncertainty) {
		this.commonPropertiesPropertyComponentUncertainty = commonPropertiesPropertyComponentUncertainty;
	}
}
