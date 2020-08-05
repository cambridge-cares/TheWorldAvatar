package uk.ac.cam.cares.jps.agent.file_management.marshallr.sensana;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;

import uk.ac.cam.cares.jps.agent.file_management.marshallr.ExecutableModel;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSMarshaller;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSAgentException;

public class MoDSMarshallerSensAna extends MoDSMarshaller {
	@Override
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, LinkedHashMap<String, String> ignDelayOption) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		ModelKineticsSRMSensAna kineticsSRM = new ModelKineticsSRMSensAna();
		ExecutableModel exeModel = kineticsSRM.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		kineticsSRM.formFiles(exeModel, ignDelayOption);
		kineticsSRM.setUpMoDS();
	}
}
