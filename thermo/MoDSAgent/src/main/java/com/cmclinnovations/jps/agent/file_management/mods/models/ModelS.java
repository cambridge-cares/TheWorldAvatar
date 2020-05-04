package com.cmclinnovations.jps.agent.file_management.mods.models;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class ModelS {
	@XmlElement(name = "model")
	private ArrayList<Model> modelList;

	public ArrayList<Model> getModel() {
		return modelList;
	}

	public void setModel(ArrayList<Model> modelList) {
		this.modelList = modelList;
	}

}
