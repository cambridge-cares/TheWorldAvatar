package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

public class MoDSJson {
	private LinkedHashMap<String, String> activeParameter = new LinkedHashMap<String, String>();
	private LinkedHashMap<String, String> passiveParameter = new LinkedHashMap<String, String>();
	private String outputs = new String();
	private List<String> nameOfCases = new ArrayList<>();

	public LinkedHashMap<String, String> getActiveParameter() {
		return activeParameter;
	}

	public void setActiveParameter(LinkedHashMap<String, String> activeParameter) {
		this.activeParameter = activeParameter;
	}

	public LinkedHashMap<String, String> getPassiveParameter() {
		return passiveParameter;
	}

	public void setPassiveParameter(LinkedHashMap<String, String> passiveParameter) {
		this.passiveParameter = passiveParameter;
	}

	public String getOutputs() {
		return outputs;
	}

	public void setOutputs(String outputs) {
		this.outputs = outputs;
	}

	public List<String> getNameOfCases() {
		return nameOfCases;
	}

	public void setNameOfCases(List<String> nameOfCases) {
		this.nameOfCases = nameOfCases;
	}
}
