package com.cmclinnovations.jps.agent.file_management.marshallr.sensana;

import java.io.IOException;
import java.util.List;

import com.cmclinnovations.jps.agent.file_management.marshallr.ExecutableModel;
import com.cmclinnovations.jps.agent.file_management.marshallr.MoDSMarshaller;
import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public class MoDSMarshallerSensAna extends MoDSMarshaller {
	@Override
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		ModelKineticsSRMSensAna kineticsSRM = new ModelKineticsSRMSensAna();
		ExecutableModel exeModel = kineticsSRM.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		kineticsSRM.formFiles(exeModel);
		kineticsSRM.setUpMoDS();
	}
}
