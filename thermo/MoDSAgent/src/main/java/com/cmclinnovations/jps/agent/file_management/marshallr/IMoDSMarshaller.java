package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.List;

import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public interface IMoDSMarshaller {
	public void initialise(String jobFolderName) throws IOException, MoDSAgentException;
	
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException;
	
	public void plugInCantera() throws IOException, MoDSAgentException;
	
	public void marshall() throws IOException, MoDSAgentException;
}
