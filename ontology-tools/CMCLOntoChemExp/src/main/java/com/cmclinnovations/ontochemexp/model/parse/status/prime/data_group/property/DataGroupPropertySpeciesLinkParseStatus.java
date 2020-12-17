package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.SpeciesLinkParseStatus;

public class DataGroupPropertySpeciesLinkParseStatus extends SpeciesLinkParseStatus {
	boolean daataGroupPropertySpeciesLink = false;

	public boolean isSpeciesLink() {
		return daataGroupPropertySpeciesLink;
	}

	public void setSpeciesLink(boolean daataGroupPropertySpeciesLink) {
		this.daataGroupPropertySpeciesLink = daataGroupPropertySpeciesLink;
	}
}
