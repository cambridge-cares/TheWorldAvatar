package com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlElement;

import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Component;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Property;

public class CommonPropertiesProperty extends Property {
	@XmlElement
	private ArrayList<Component> component;
	
	public ArrayList<Component> getComponent() {
		return component;
	}
	public void setComponent(ArrayList<Component> component) {
		this.component = component;
	}
}
