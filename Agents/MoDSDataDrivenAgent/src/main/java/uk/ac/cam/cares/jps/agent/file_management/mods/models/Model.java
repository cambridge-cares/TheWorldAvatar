package uk.ac.cam.cares.jps.agent.file_management.mods.models;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlMixed;
import javax.xml.bind.annotation.XmlType;

import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.DetailS;

//@XmlType(propOrder = { "detailS" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Model {
	@JsonProperty("name")
	@XmlAttribute
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	@JsonProperty("details")
	@XmlElement(name = "details")
	private DetailS detailS;

	public DetailS getDetailS() {
		return detailS;
	}

	public void setDetailS(DetailS detailS) {
		this.detailS = detailS;
	}
	
//	@JsonProperty("details")
//	@XmlElementRef(type = DetailS.class, name = "details")
//	private List<Object> items = new ArrayList<Object>();
//
//	public List<Object> getItems() {
//		return items;
//	}
//
//	public void setItems(List<Object> items) {
//		this.items = items;
//	}
	
//	@JsonProperty("details")
//	@XmlElementRef(type = DetailS.class, name = "details")
//	private DetailS items = new DetailS();
//
//	public DetailS getItems() {
//		return items;
//	}
//
//	public void setItems(DetailS items) {
//		this.items = items;
//	}
	
//	@JsonProperty("content")
//	@XmlMixed
//	private List<String> content;
//	
//	public List<String> getContent() {
//		return content;
//	}
//	
//	public void setContent(List<String> content) {
//		this.content = content;
//	}
}
