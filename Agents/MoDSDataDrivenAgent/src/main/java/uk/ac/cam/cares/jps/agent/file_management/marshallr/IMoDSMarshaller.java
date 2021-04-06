package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;

import uk.ac.cam.cares.jps.agent.mechanism.DataDriven.MoDSDataDrivenAgentException;

public interface IMoDSMarshaller {
	public void initialise(String jobFolderName) throws IOException, MoDSDataDrivenAgentException;
	
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSDataDrivenAgentException;
	
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSDataDrivenAgentException;
	
	public String marshall() throws IOException, MoDSDataDrivenAgentException;
}
