package uk.ac.cam.cares.jps.agent.mods.common.backend.mods.algorithms;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.agent.mods.common.backend.mods.parameters.DetailS;

@XmlType(propOrder = { "detailS" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Algorithm {
	@JsonProperty("name")
	@XmlAttribute
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	@JsonProperty("displayName")
	@XmlAttribute(name = "display_name")
	private String displayName;

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
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
}
