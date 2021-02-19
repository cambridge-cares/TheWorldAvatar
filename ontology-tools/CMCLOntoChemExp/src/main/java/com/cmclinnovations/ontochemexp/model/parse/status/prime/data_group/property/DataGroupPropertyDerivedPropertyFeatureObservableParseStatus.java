package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ObservableParseStatus;

public class DataGroupPropertyDerivedPropertyFeatureObservableParseStatus extends ObservableParseStatus {
	boolean dataGroupPropertyDerivedPropertyFeatureObservable = false;
	
	public boolean isObservable() {
		return dataGroupPropertyDerivedPropertyFeatureObservable;
	}

	public void setObservable(boolean dataGroupPropertyDerivedPropertyFeatureObservable) {
		this.dataGroupPropertyDerivedPropertyFeatureObservable = dataGroupPropertyDerivedPropertyFeatureObservable;
	}
}
