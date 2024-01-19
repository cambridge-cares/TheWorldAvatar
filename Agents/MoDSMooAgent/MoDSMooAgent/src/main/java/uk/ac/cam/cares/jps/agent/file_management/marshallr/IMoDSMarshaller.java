package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.List;

import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;


public interface IMoDSMarshaller {
	public void initialise(String jobFolderName) throws IOException, MoDSMooAgentException;
	
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException;
	
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException;
	
	public String marshall() throws IOException, MoDSMooAgentException;

	public void plugInModelMoo(List<String> dataVar, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException;

	/**
	 * Set up all the components of executable in the MoDS input file. 
	 * 
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	void setUpMoDS() throws IOException, MoDSMooAgentException;
}
