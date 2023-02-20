package uk.ac.cam.cares.jps.agent.mods.common.backend.mods.models;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.fasterxml.jackson.annotation.JsonProperty;

@XmlAccessorType(XmlAccessType.FIELD)
public class SimplifiedModelS {
	@JsonProperty("model")
	@XmlElement(name = "model")
	private List<SimplifiedModel> caseModelList;

	public List<SimplifiedModel> getCaseModelList() {
		return caseModelList;
	}

	public void setCaseModelList(List<SimplifiedModel> caseModelList) {
		this.caseModelList = caseModelList;
	}
}
