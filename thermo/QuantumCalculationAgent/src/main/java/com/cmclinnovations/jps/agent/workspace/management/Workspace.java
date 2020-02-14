package com.cmclinnovations.jps.agent.workspace.management;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;

import org.apache.commons.lang.math.NumberUtils;

import com.cmclinnovations.jps.agent.quantum.calculation.Property;
import com.cmclinnovations.jps.agent.quantum.calculation.Utils;

public class Workspace {
	
	public String createAgentWorkspace(String workspaceParentPath, String agentClass){
		if(isWorkspaceAvailable(workspaceParentPath, agentClass)){
			return getWorkspaceName(workspaceParentPath, agentClass);
		}
		return null;
	}
	
	public String getWorkspaceName(String workspaceParentPath, String agentClass){
		File dir = new File(workspaceParentPath);
		for(File file:dir.listFiles()){
			if(file.isDirectory()){
				if(file.getName().toLowerCase().startsWith(agentClass.toLowerCase())){
					String[] tokens = file.getName().split("_");
					if(tokens.length==2 && tokens[1].length() > 6 && NumberUtils.isNumber(tokens[1])){
						return file.getName();
					}
				}
			}
		}
		return getWorkspaceName(workspaceParentPath, agentClass);
	}
	
	private String createWorkspaceName(String workspaceParentPath, String agentClass){
		String workspaceName = agentClass.concat("_").concat("" + System.nanoTime());
		File workspace = new File(workspaceParentPath.concat(workspaceName));
		if(workspace.mkdir()){
			return workspaceName;
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
	
	public void createInputFile() throws IOException{
		BufferedWriter inputFile = Utils.openBufferedWriter(
				Property.AGENT_WORKSPACE_DIR.getPropertyName()
				.concat(Property.HPC_CAMBRIDGE_ADDRESS.getPropertyName())
				.concat(Property.EXTENSION_INPUT_FILE.getPropertyName()));
		inputFile.write(Property.JOB_NO_OF_CORES.getPropertyName().concat("\n"));
		inputFile.write(Property.JOB_MEMORY.getPropertyName().concat("\n"));
		inputFile.write(Property.JOB_CHK_POINT_FILE.getPropertyName().concat("\n"));
		inputFile.write(Property.JOB_PRINT_DIRECTIVE.getPropertyName().concat("\n"));
		inputFile.close();
	}

}
