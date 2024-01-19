package uk.ac.cam.cares.jps.agent.file_management.mods.parameters;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonProperty;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class DetailS {
	@JsonProperty("detail")
	@XmlElement(name = "detail")
	private ArrayList<Detail> detailList;
	
	public ArrayList<Detail> getDetail() {
		return detailList;
	}
	
	public void setDetail(ArrayList<Detail> detailList) {
		this.detailList = detailList;
	}
}
