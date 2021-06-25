package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.util.LinkedHashMap;
import java.util.List;

public class ExecutableModel {
	private String modelName;
	private LinkedHashMap<String, String> activeParameters;
	private List<String> passiveParameters;
	private List<String> outputResponses;
	private List<String> expFiles;
	private List<String> caseNames;

	public String getModelName() {
		return modelName;
	}

	public void setModelName(String modelName) {
		this.modelName = modelName;
	}

	public LinkedHashMap<String, String> getActiveParameters() {
		return activeParameters;
	}

	public void setActiveParameters(LinkedHashMap<String, String> activeParameters) {
		this.activeParameters = activeParameters;
	}

	public List<String> getPassiveParameters() {
		return passiveParameters;
	}

	public void setPassiveParameters(List<String> passiveParameters) {
		this.passiveParameters = passiveParameters;
	}

	public List<String> getOutputResponses() {
		return outputResponses;
	}

	public void setOutputResponses(List<String> outputResponses) {
		this.outputResponses = outputResponses;
	}

	public List<String> getExpFiles() {
		return expFiles;
	}

	public void setExpFiles(List<String> expFiles) {
		this.expFiles = expFiles;
	}

	public List<String> getCaseNames() {
		return caseNames;
	}

	public void setCaseNames(List<String> caseNames) {
		this.caseNames = caseNames;
	}
}
