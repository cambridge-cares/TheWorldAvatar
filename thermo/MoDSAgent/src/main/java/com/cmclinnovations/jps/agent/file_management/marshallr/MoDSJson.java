package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MoDSJson {
	private HashMap<String, String> activeParameter = new HashMap<String, String>();
	private HashMap<String, String> passiveParameter = new HashMap<String, String>();
	private String outputs = new String();
	private List<String> nameOfCases = new ArrayList<>();

	public HashMap<String, String> getActiveParameter() {
		return activeParameter;
	}

	public void setActiveParameter(HashMap<String, String> activeParameter) {
		this.activeParameter = activeParameter;
	}

	public HashMap<String, String> getPassiveParameter() {
		return passiveParameter;
	}

	public void setPassiveParameter(HashMap<String, String> passiveParameter) {
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
