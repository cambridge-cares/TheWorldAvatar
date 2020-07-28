package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.List;

import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSAgentException;

public interface IMoDSMarshaller {
	public void initialise(String jobFolderName) throws IOException, MoDSAgentException;
	
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException;
	
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException;
	
	public String marshall() throws IOException, MoDSAgentException;
}
