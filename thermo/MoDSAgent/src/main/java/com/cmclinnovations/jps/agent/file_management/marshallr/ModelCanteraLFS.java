package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public class ModelCanteraLFS extends MoDSMarshaller implements IModel {

	@Override
	public ExecutableModel formExecutableModel(List<String> experimentIRI, String mechanismIRI,
			List<String> reactionIRIList) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<String> formFiles(ExecutableModel exeModel) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<String> createFolderInitial(List<String> activeParameters) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<String> createFolderAll(List<String> processedActiveParam) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		return null;
	}
	
}
