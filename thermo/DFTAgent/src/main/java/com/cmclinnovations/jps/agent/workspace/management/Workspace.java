package com.cmclinnovations.jps.agent.workspace.management;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;

import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.lang3.StringUtils;

import com.cmclinnovations.jps.agent.json.parser.AgentRequirementParser;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
import com.cmclinnovations.jps.agent.quantum.calculation.Jobs;
import com.cmclinnovations.jps.agent.quantum.calculation.Property;
import com.cmclinnovations.jps.agent.quantum.calculation.Utils;
import com.cmclinnovations.jps.kg.OntoAgentKG;
import com.cmclinnovations.jps.kg.object.model.Job;
import com.google.common.io.Files;

public class Workspace {
	
	public long previousTimeStamp = System.nanoTime();
	
	public File createAgentWorkspace(String workspaceParentPath, String agentClass){
		if(isWorkspaceAvailable(workspaceParentPath, agentClass)){
			return getWorkspaceName(workspaceParentPath, agentClass);
		}
		return createWorkspaceName(workspaceParentPath, agentClass);
	}
	
	public File getWorkspaceName(String workspaceParentPath, String agentClass){
		File dir = new File(workspaceParentPath);
		for(File file:dir.listFiles()){
			if(file.isDirectory()){
				if(file.getName().toLowerCase().startsWith(agentClass.toLowerCase())){
					String[] tokens = file.getName().split("_");
					if(tokens.length==2 && tokens[1].length() > 6 && NumberUtils.isNumber(tokens[1])){
						return file;
					}
				}
			}
		}
		return createWorkspaceName(workspaceParentPath, agentClass);
	}
	
	private File createWorkspaceName(String workspaceParentPath, String agentClass){
		String workspaceName = agentClass.concat("_").concat("" + System.nanoTime());
		File workspace = new File(workspaceParentPath.concat(File.separator).concat(workspaceName));
		if(workspace.mkdir()){
			return workspace;
		}
		return null;
	}
	
	private boolean isWorkspaceAvailable(String workspaceParentPath, String agentClass){
		File dir = new File(workspaceParentPath);
		if(dir!=null && dir.isDirectory()){
			return isWorkspaceAvailable(dir, agentClass);
		}
		return false;
	}
	
	protected boolean isWorkspaceAvailable(File dir, String agentClass){
		for(File file:dir.listFiles()){
			if(file.isDirectory()){
				if(file.getName().toLowerCase().startsWith(agentClass.toLowerCase())){
					return true;
				}
			}
		}
		return false;
	}
	
	public String createInputFile(String inputFilePath, String jobFolder, String geometry, String jsonString) throws IOException{
		BufferedWriter inputFile = Utils.openBufferedWriter(inputFilePath);
		if(inputFile == null){
			return null;
		}
		inputFile.write(Property.JOB_NO_OF_CORES_PREFIX.getPropertyName().concat(AgentRequirementParser.getNumberOfCores(OntoAgentKG.getNumberOfCores()).concat("\n")));
		inputFile.write(Property.JOB_MEMORY_PREFIX.getPropertyName().concat(AgentRequirementParser.getRAMSize(OntoAgentKG.getMemorySize()))
				.concat(Property.JOB_MEMORY_UNITS.getPropertyName()).concat("\n"));
		inputFile.write(Property.JOB_CHK_POINT_FILE_ADDRESS_PART.getPropertyName().concat("_")
				.concat(getTimeStampPart(jobFolder))
				.concat(Property.CHK_POINT_FILE_EXTENSION.getPropertyName()).concat("\n"));
		inputFile.write(Property.JOB_PRINT_DIRECTIVE.getPropertyName().concat(" ")
				.concat(JSonRequestParser.getLevelOfTheory(jsonString)).concat(" ")
				.concat(JSonRequestParser.getJobKeyword(jsonString)).concat(" ")
				.concat(JSonRequestParser.getAlgorithmChoice(jsonString)).concat("\n\n"));
		inputFile.write(" ".concat(jobFolder).concat("\n\n"));
		inputFile.write(Property.SPECIES_CHARGE_ZERO.getPropertyName().concat(" ")
				.concat(Property.SPECIES_MULTIPLICITY.getPropertyName()).concat("\n"));
		inputFile.write(geometry.concat("\n"));
		inputFile.close();
		return Jobs.JOB_SETUP_SUCCESS_MSG.getName();
	}

	public String createStatusFile(File workspaceFolder, String statusFilePath) throws IOException{
		BufferedWriter statusFile = Utils.openBufferedWriter(statusFilePath);
		if(statusFile == null){
			return null;
		}
		statusFile.write(Jobs.ATTRIBUTE_JOB_STATUS.getName().concat(" "));
		statusFile.write(Jobs.STATUS_JOB_NOT_STARTED.getName().concat("\n"));
		statusFile.write(Jobs.ATTRIBUTE_JOB_ID.getName().concat("\n"));
		statusFile.write(Jobs.ATTRIBUTE_AGENT_ID.getName().concat(" "));
		statusFile.write(workspaceFolder.getName().concat("\n"));
		statusFile.write(Jobs.ATTRIBUTE_HPC_ADDRESS.getName().concat(" "));
		statusFile.write(Property.HPC_CAMBRIDGE_ADDRESS.getPropertyName().concat("\n"));
		statusFile.close();
		return Jobs.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	public String createJSONInputFile(File workspaceFolder, String jsonInputFilePath, String jsonString) throws IOException{
		BufferedWriter jsonInputFile = Utils.openBufferedWriter(jsonInputFilePath);
		if(jsonInputFile == null){
			return null;
		}
		jsonInputFile.write(jsonString);
		jsonInputFile.close();
		return Jobs.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	public String getWorkspacePath(String workspace){
		return Property.AGENT_WORKSPACE_DIR.getPropertyName()
		.concat(File.separator)
		.concat(workspace);
	}
	
	public String getInputFilePath(File jobFolder){
		return jobFolder.getAbsolutePath()
		.concat(File.separator)
		.concat(Property.HPC_CAMBRIDGE_ADDRESS.getPropertyName())
		.concat("_").concat(getTimeStampPart(jobFolder.getName()))
		.concat(Property.INPUT_FILE_EXTENSION.getPropertyName());
	}
	
	public File createJobFolder(String workspacePath){
		String jobFolder = Property.HPC_CAMBRIDGE_ADDRESS.getPropertyName().concat("_").concat("" + Utils.getTimeStamp());
		File workspace = new File(workspacePath.concat(File.separator).concat(jobFolder));
		if(workspace.mkdir()){
			return workspace;
		}
		return null;
	}
	
	public String getTimeStampPart(String folder){
		if(folder.contains("_")){
			String[] tokens = folder.split("_");
			if(tokens.length==2 && tokens[1]!=null && StringUtils.isNumeric(tokens[1])){
				return tokens[1];
			}
		}
		return null;
	}
	
	public String getStatusFilePath(File jobFolder){
		return jobFolder.getAbsolutePath().concat(File.separator).concat(Property.STATUS_FILE_NAME.getPropertyName()).concat(Property.STATUS_FILE_EXTENSION.getPropertyName());
	}
	
	public String getJSONInputFilePath(File jobFolder){
		return jobFolder.getAbsolutePath().concat(File.separator).concat(Property.JSON_INPUT_FILE_NAME.getPropertyName()).concat(Property.JSON_FILE_EXTENSION.getPropertyName());
	}
	
	public String copyScriptFile(String source, String destination) throws IOException{
		try{
		copyFile(new File(source),
				new File(destination.concat(File.separator)
						.concat(Property.SLURM_SCRIPT_FILE_NAME.getPropertyName())));
		return Jobs.JOB_SETUP_SUCCESS_MSG.getName();
		}catch(IOException e){
			e.printStackTrace();
			return null;
		}
	}
	
	private void copyFile(File from, File to) throws IOException{
		Files.copy(from, to);
	}
}
