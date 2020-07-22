package com.cmclinnovations.jps.agent.file_management.marshallr.yrt23;

import java.io.IOException;
import java.util.List;

import com.cmclinnovations.jps.agent.file_management.marshallr.ExecutableModel;
import com.cmclinnovations.jps.agent.file_management.marshallr.MoDSMarshaller;
import com.cmclinnovations.jps.agent.file_management.marshallr.ModelCanteraLFS;
import com.cmclinnovations.jps.agent.file_management.marshallr.ModelKineticsSRM;
import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public class MoDSMarshaller4yrt23 extends MoDSMarshaller {
	@Override
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		ModelKineticsSRM4yrt23 kineticsSRM = new ModelKineticsSRM4yrt23();
		ExecutableModel exeModel = kineticsSRM.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		kineticsSRM.formFiles(exeModel);
		kineticsSRM.setUpMoDS();
	}

	@Override
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		ModelCanteraLFS4yrt23 canteraLFS = new ModelCanteraLFS4yrt23();
		ExecutableModel exeModel = canteraLFS.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		canteraLFS.formFiles(exeModel);
		canteraLFS.setUpMoDS();
	}
}
