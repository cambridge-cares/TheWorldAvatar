package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;

import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;

public interface IMoDSMarshaller {
	public void initialise(String jobFolderName) throws IOException, AutoMechCalibAgentException;
	
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, AutoMechCalibAgentException;
	
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, AutoMechCalibAgentException;
	
	public String marshall() throws IOException, AutoMechCalibAgentException;
}
