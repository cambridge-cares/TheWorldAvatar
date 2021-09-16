package com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.SpeciesLinkParseStatus;

public class ComponentSpeciesLinkParseStatus extends SpeciesLinkParseStatus {
	boolean commonPropertiesPropertyComponentSpeciesLink = false;

	public boolean isSpeciesLink() {
		return commonPropertiesPropertyComponentSpeciesLink;
	}

	public void setSpeciesLink(boolean commonPropertiesPropertyComponentSpeciesLink) {
		this.commonPropertiesPropertyComponentSpeciesLink = commonPropertiesPropertyComponentSpeciesLink;
	}
}
