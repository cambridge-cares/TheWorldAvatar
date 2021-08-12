package uk.ac.cam.cares.jps.agent.mods.common.backend.mods.models;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonProperty;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class ModelS {
	@JsonProperty("model")
	@XmlElement(name = "model")
	private ArrayList<Model> modelList;

	public ArrayList<Model> getModel() {
		return modelList;
	}

	public void setModel(ArrayList<Model> modelList) {
		this.modelList = modelList;
	}

}
