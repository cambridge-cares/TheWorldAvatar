package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;

public interface IModel {
	/**
	 * Collect all information required by MoDS to execute the model. 
	 * 
	 * @param experimentIRI
	 * @param mechanismIRI
	 * @param reactionIRIList
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 */
	public ExecutableModel formExecutableModel(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) 
			throws IOException, AutoMechCalibAgentException;
	
	/**
	 * Form all files required by MoDS to execute the model. 
	 * 
	 * @param exeModel
	 * @param jobFolderPath
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 */
	public List<String> formFiles(ExecutableModel exeModel, String otherOptions) throws IOException, AutoMechCalibAgentException;
	
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
	 * @throws AutoMechCalibAgentException
	 */
	public List<String> createFolderInitial(List<String> activeParameters) throws IOException, AutoMechCalibAgentException ;
	
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
	 * @throws AutoMechCalibAgentException
	 */
	public List<String> createFolderAll(List<String> processedActiveParam) throws IOException, AutoMechCalibAgentException;
	
	/**
	 * Set up all the components of executable in the MoDS input file. 
	 * 
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 */
	public void setUpMoDS() throws IOException, AutoMechCalibAgentException;
	
	/**
	 * Set up the simulation script required for the model to execute. 
	 * 
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 */
	public void placeScript() throws IOException, AutoMechCalibAgentException;
}
