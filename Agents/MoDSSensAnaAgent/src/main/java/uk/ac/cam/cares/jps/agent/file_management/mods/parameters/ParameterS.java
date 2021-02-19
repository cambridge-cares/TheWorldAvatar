package uk.ac.cam.cares.jps.agent.file_management.mods.parameters;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class ParameterS {
	@XmlAttribute
	private String version;

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	@XmlElement(name = "parameter")
	private ArrayList<Parameter> parameterList;

	public ArrayList<Parameter> getParameter() {
		return parameterList;
	}

	public void setParameter(ArrayList<Parameter> parameterList) {
		this.parameterList = parameterList;
	}
}
