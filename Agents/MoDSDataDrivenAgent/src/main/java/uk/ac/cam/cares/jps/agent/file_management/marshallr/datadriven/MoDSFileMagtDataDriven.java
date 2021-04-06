package uk.ac.cam.cares.jps.agent.file_management.marshallr.datadriven;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.IMoDSMarshaller;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSFileManagement;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgentException;

public class MoDSFileMagtDataDriven extends MoDSFileManagement {
	private MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty;
	
	public MoDSFileMagtDataDriven(MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty) {
		super(modsDataDrivenAgentProperty);
		this.modsDataDrivenAgentProperty = modsDataDrivenAgentProperty;
	}
	
	@Override
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSDataDrivenAgentException {
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = new ArrayList<>(); // not in use, just for the arg place
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshallerDataDriven(modsDataDrivenAgentProperty);
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList, jsonString);
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList, jsonString);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		return jobFolderPath;
	}
	
}
