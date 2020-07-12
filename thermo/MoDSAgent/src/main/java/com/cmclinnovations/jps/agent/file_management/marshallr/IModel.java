package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public interface IModel {
	/**
	 * Collect all information required by MoDS to execute the model. 
	 * 
	 * @param experimentIRI
	 * @param mechanismIRI
	 * @param reactionIRIList
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	public ExecutableModel formExecutableModel(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) 
			throws IOException, MoDSAgentException;
	
	/**
	 * Form all files required by MoDS to execute the model. 
	 * 
	 * @param exeModel
	 * @param jobFolderPath
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	public List<String> formFiles(ExecutableModel exeModel) throws IOException, MoDSAgentException;
	
	/**
	 * Create the files used during the 'initial read' when executing the model. 
	 * 
	 * @param initial
	 * @param modelName
	 * @param activeParameters
	 * @param caseNames
	 * @param expFiles
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	public List<String> createFolderInitial(List<String> activeParameters) throws IOException, MoDSAgentException ;
	
	/**
	 * Create the files used during the 'working write' when executing the model. 
	 * 
	 * @param all
	 * @param modelName
	 * @param processedActiveParam
	 * @param caseNames
	 * @param expFiles
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	public List<String> createFolderAll(List<String> processedActiveParam) throws IOException, MoDSAgentException;
}
