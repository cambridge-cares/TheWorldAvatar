package com.cmclinnovations.jps.agent.file_management.mods.algorithms;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class AlgorithmS {
	@XmlElement(name = "algorithm")
	private ArrayList<Algorithm> algorithmList;

	public ArrayList<Algorithm> getAlgorithm() {
		return algorithmList;
	}

	public void setAlgorithm(ArrayList<Algorithm> algorithmList) {
		this.algorithmList = algorithmList;
	}
}
