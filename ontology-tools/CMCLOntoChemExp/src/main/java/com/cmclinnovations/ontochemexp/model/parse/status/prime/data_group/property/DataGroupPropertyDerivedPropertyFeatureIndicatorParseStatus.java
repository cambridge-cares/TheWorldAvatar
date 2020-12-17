package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.IndicatorParseStatus;

public class DataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus extends IndicatorParseStatus {
	boolean dataGroupPropertyDerivedPropertyFeatureIndicator = false;
	
	public boolean isIndicator() {
		return dataGroupPropertyDerivedPropertyFeatureIndicator;
	}

	public void setIndicator(boolean dataGroupPropertyDerivedPropertyFeatureIndicator) {
		this.dataGroupPropertyDerivedPropertyFeatureIndicator = dataGroupPropertyDerivedPropertyFeatureIndicator;
	}
}
