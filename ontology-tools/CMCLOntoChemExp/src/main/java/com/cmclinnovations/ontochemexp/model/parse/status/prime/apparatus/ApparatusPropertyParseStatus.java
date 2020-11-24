package com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.PropertyParseStatus;

public class ApparatusPropertyParseStatus extends PropertyParseStatus {
	boolean apparatusProperty = false;
	
	public boolean isProperty() {
		return apparatusProperty;
	}
	public void setProperty(boolean apparatusProperty) {
		this.apparatusProperty = apparatusProperty;
	}
}
