package com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlMixed;
import javax.xml.bind.annotation.XmlValue;

import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;

@XmlAccessorType(XmlAccessType.FIELD)
public class DataGroupPropertyComponent {
	@XmlMixed
	@XmlElementRef(type = SpeciesLink.class, name = "speciesLink")
	List<Object> items = new ArrayList<Object>();
	
	public List<Object> getItems() {
		return items;
	}

	public void setItems(List<Object> items) {
		this.items = items;
	}
}
