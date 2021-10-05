package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;

public interface IModel {
	/**
	 * Collect all information required by MoDS to execute the model. 
	 * 
	 * @param experimentIRI
	 * @param mechanismIRI
	 * @param reactionIRIList
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public ExecutableModel formExecutableModel(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) 
			throws IOException, MoDSMooAgentException;
	
	/**
	 * Collect all information required by MoDS to execute the model. 
	 * 
	 * @param pathCSV
	 * @param mechanismIRI
	 * @param reactionIRIList
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	
	/*public ExecutableModel formExecutableModelCSV(String pathCSV, String mechanismIRI, List<String> reactionIRIList) 
			throws IOException, MoDSMooAgentException;
	*/
	
	/**
	 * Form all files required by MoDS to execute the model. 
	 * 
	 * @param exeModel
	 * @param jobFolderPath
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public List<String> formFiles(ExecutableModel exeModel, String otherOptions) throws IOException, MoDSMooAgentException;
	
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
	 * @throws MoDSMooAgentException
	 */
	public List<String> createFolderInitial(List<String> activeParameters) throws IOException, MoDSMooAgentException ;
	
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
	 * @throws MoDSMooAgentException
	 */
	public List<String> createFolderAll(List<String> processedActiveParam) throws IOException, MoDSMooAgentException;
	
	/**
	 * Set up all the components of executable in the MoDS input file. 
	 * 
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void setUpMoDS() throws IOException, MoDSMooAgentException;
	
	/**
	 * Set up the simulation script required for the model to execute. 
	 * 
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	public void placeScript() throws IOException, MoDSMooAgentException;
}
