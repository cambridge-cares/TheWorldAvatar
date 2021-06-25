package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;

import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;

public interface IMoDSMarshaller {
	public void initialise(String jobFolderName) throws IOException, MoDSMechCalibAgentException;
	
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMechCalibAgentException;
	
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMechCalibAgentException;
	
	public String marshall() throws IOException, MoDSMechCalibAgentException;
}
