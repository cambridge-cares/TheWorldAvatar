package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.List;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentProperty;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgentException;

public class MoDSFileManagement extends MoDSMarshaller {
	Logger logger = Logger.getLogger(MoDSFileManagement.class);
	private MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty;
	
	public MoDSFileManagement(MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty) {
		super(modsDataDrivenAgentProperty);
		this.modsDataDrivenAgentProperty = modsDataDrivenAgentProperty;
	}
	
	/**
	 * Create all input files that required by a MoDS job. 
	 * 
	 * @param jsonString
	 * @param jobFolderName
	 * @return
	 * @throws IOException
	 * @throws MoDSDataDrivenAgentException
	 */
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSDataDrivenAgentException {
		
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = JSonRequestParser.getOntoKinReactionsIRI(jsonString);
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshaller(modsDataDrivenAgentProperty);
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList, jsonString);
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList, jsonString);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		logger.info("The requested MoDS job was created.");
		return jobFolderPath;
	}
}
