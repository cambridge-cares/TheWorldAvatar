package com.cmclinnovations.jps.agent.file_management.mods.parameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class WorkingRead {
	@XmlAttribute(name="file_name")
	private String fileName;
	
	public String getFileName() {
		return fileName;
	}
	
	public void setFileName(String fileName) {
		this.fileName = fileName;
	}
	
	@XmlElement(name = "details")
	private DetailS detailS;

	public DetailS getDetailS() {
		return detailS;
	}

	public void setDetailS(DetailS detailS) {
		this.detailS = detailS;
	}
}
