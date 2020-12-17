package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property;

import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ComponentParseStatus;

public class DataGroupPropertyComponentParseStatus extends ComponentParseStatus {
	boolean dataGroupPropertyComponent = false;
	boolean dataGroupPropertyComponentSpeciesLink = false;
	boolean componentValue = true;
	
	public boolean isComponent() {
		return dataGroupPropertyComponent;
	}
	
	public void setComponent(boolean dataGroupPropertyComponent) {
		this.dataGroupPropertyComponent = dataGroupPropertyComponent;
	}
	
	public boolean isComponentSpeciesLink() {
		return dataGroupPropertyComponentSpeciesLink;
	}
	
	public void setComponentSpeciesLink(boolean dataGroupPropertyComponentSpeciesLink) {
		this.dataGroupPropertyComponentSpeciesLink = dataGroupPropertyComponentSpeciesLink;
	}
	
	public boolean hasComponentValue() {
		return componentValue;
	}
	
	public void setComponentValue(boolean componentValue) {
		this.componentValue = componentValue;
	}
}
