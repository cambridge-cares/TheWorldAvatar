package com.cmclinnovations.ontochemexp.model.parse.status.prime;

/**
 * This class contains getters and setters to flags that maintain whether
 * or not a PrIMe experiment's commonProperties elements and attributes have already 
 * been parsed.
 * 
 */
public class CommonPropertiesParseStatus {
	boolean commonProperties = false;

	
	public boolean isCommonProperties() {
		return commonProperties;
	}
	public void setCommonProperties(boolean commonProperties) {
		this.commonProperties = commonProperties;
	}
}
