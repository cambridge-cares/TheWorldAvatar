package com.cmclinnovations.jps.agent.file_management.mods.functions;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.jps.agent.file_management.mods.parameters.DetailS;

@XmlType(propOrder = { "detailS" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Function {
	@XmlAttribute
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@XmlAttribute(name = "usage")
	private String usage;

	public String getUsage() {
		return usage;
	}

	public void setUsage(String usage) {
		this.usage = usage;
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
