package com.cmclinnovations.jps.agent.file_management.mods.cases;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlMixed;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.jps.agent.file_management.mods.models.ModelS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.DetailS;

//@XmlType(propOrder = { "items" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Case {
	@XmlAttribute
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	@XmlMixed
	@XmlElementRef(type = ModelS.class, name = "models")
	List<Object> items = new ArrayList<Object>();

	public List<Object> getItems() {
		return items;
	}

	public void setItems(List<Object> items) {
		this.items = items;
	}
//
//	@XmlElement(name = "details")
//	private DetailS detailS;
//
//	public DetailS getDetailS() {
//		return detailS;
//	}
//
//	public void setDetailS(DetailS detailS) {
//		this.detailS = detailS;
//	}
}
