package com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.UncertaintyParseStatus;

public class ApparatusPropertyUncertaintyParseStatus extends UncertaintyParseStatus {
	boolean apparatusPropertyUncertainty = false;

	public boolean isUncertainty() {
		return apparatusPropertyUncertainty;
	}

	public void setUncertainty(boolean apparatusPropertyUncertainty) {
		this.apparatusPropertyUncertainty = apparatusPropertyUncertainty;
	}
}
