package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;

import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgentException;

public interface IMoDSMarshaller {
	public void initialise(String jobFolderName) throws IOException, MoDSSensAnaAgentException;
	
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSSensAnaAgentException;
	
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSSensAnaAgentException;
	
	public String marshall() throws IOException, MoDSSensAnaAgentException;
}
