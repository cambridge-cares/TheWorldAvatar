package com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.UncertaintyParseStatus;

public class CommonPropertiesPropertyUncertaintyParseStatus extends UncertaintyParseStatus {
	boolean commonPropertiesPropertyUncertainty = false;

	public boolean isUncertainty() {
		return commonPropertiesPropertyUncertainty;
	}

	public void setUncertainty(boolean commonPropertiesPropertyUncertainty) {
		this.commonPropertiesPropertyUncertainty = commonPropertiesPropertyUncertainty;
	}
}
