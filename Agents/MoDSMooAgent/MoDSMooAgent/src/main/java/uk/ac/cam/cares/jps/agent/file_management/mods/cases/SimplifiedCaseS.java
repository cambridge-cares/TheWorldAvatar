package uk.ac.cam.cares.jps.agent.file_management.mods.cases;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.fasterxml.jackson.annotation.JsonProperty;

@XmlAccessorType(XmlAccessType.FIELD)
public class SimplifiedCaseS {
	@JsonProperty("case")
	@XmlElement(name = "case")
	private List<SimplifiedCase> simplifiedCaseList;

	public List<SimplifiedCase> getSimplifiedCaseList() {
		return simplifiedCaseList;
	}

	public void setSimplifiedCaseList(List<SimplifiedCase> simplifiedCaseList) {
		this.simplifiedCaseList = simplifiedCaseList;
	}
}
