package com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.AmountParseStatus;

public class CommonPropertiesPropertyComponentAmountParseStatus extends AmountParseStatus {
	boolean commonPropertiesPropertyComponentAmount = false;
	
	public boolean isAmount() {
		return commonPropertiesPropertyComponentAmount;
	}

	public void setAmount(boolean commonPropertiesPropertyComponentAmount) {
		this.commonPropertiesPropertyComponentAmount = commonPropertiesPropertyComponentAmount;
	}
}
