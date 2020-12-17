package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.FeatureParseStatus;

public class DataGroupPropertyDerivedPropertyFeatureParseStatus extends FeatureParseStatus {
	boolean dataGroupPropertyDerivedPropertyFeature = false;

	public boolean isFeature() {
		return dataGroupPropertyDerivedPropertyFeature;
	}

	public void setFeature(boolean dataGroupPropertyDerivedPropertyFeature) {
		this.dataGroupPropertyDerivedPropertyFeature = dataGroupPropertyDerivedPropertyFeature;
	}
}
