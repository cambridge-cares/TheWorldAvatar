package com.cmclinnovations.slurm.job;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;

import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.lang3.StringUtils;

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
		
	public String getTimeStampPart(String folder){
		if(folder.contains("_")){
			String[] tokens = folder.split("_");
			if(tokens.length==2 && tokens[1]!=null && StringUtils.isNumeric(tokens[1])){
				return tokens[1];
			}
		}
		return null;
	}
	
	private void copyFile(File from, File to) throws IOException{
		Files.copy(from, to);
	}
}
