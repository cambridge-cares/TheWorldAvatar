package uk.ac.cam.cares.jps.agent.file_management.mods;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.DetailS;

@XmlType(propOrder = { "detailS" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Global {
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
