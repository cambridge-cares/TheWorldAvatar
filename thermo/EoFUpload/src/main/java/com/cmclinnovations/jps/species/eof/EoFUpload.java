package com.cmclinnovations.jps.species.eof;

import java.io.File;

/**
 * EBR Agent estimates the enthalpy of formation of a set of given target<br>
 * species. This class uploads the estimated enthalpy of formation of those<br>
 * species to the OntoSpcies knowledge graph. 
 * 
 * @author msff2
 *
 */
public class EoFUpload {

	private File outputDirPath;
	private File jsonInputFile;
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}
	
	/**
	 * EBR Agent copies the output (result) to a sub-folder under the job<br>
	 * folder on the server where the agent runs. The agent also copies<br>
	 * the JSON input (HTTP request) to the same sub-folder. For uploading<br>
	 * the enthalpy of formation of the current set of (target) species, EBR<br>
	 * Agent needs to call this constructor.
	 * 
	 * @param outputDirPath
	 * @param jsonInputFile
	 */
	public EoFUpload(File outputDirPath, File jsonInputFile){
		this.outputDirPath = outputDirPath;
		this.jsonInputFile = jsonInputFile;
	}

	public File getOutputDirPath() {
		return outputDirPath;
	}

	public void setOutputDirPath(File outputDirPath) {
		this.outputDirPath = outputDirPath;
	}

	public File getJsonInputFile() {
		return jsonInputFile;
	}

	public void setJsonInputFile(File jsonInputFile) {
		this.jsonInputFile = jsonInputFile;
	}
}
