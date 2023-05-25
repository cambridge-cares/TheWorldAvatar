package uk.ac.cam.cares.jps.agent.mods.common.backend.mods.parameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.agent.mods.common.backend.mods.files.File;

@XmlAccessorType(XmlAccessType.FIELD)
public class WorkingWrite extends File {
	@JsonProperty("name")
	@XmlAttribute(name="file_name")
	private String fileName;
	
	public String getFileName() {
		return fileName;
	}
	
	public void setFileName(String fileName) {
		this.fileName = fileName;
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
