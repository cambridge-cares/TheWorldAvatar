package uk.ac.cam.cares.jps.agent.file_management;

import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSMarshaller;
import uk.ac.cam.cares.jps.agent.mods.common.backend.mods.MoDS;

public class InitMoDSInputs extends MoDSMarshaller implements IInitMoDSInputs {
	
	public InitMoDSInputs(MoDSMechCalibAgentProperty modsMechCalibAgentProperty) {
		super(modsMechCalibAgentProperty);
	}
	
	/**
	 * Initialise the instance that stores the content in the MoDS input file. 
	 */
	public void init() {
		mods = new MoDS();
		
		modsJsonNode = new ObjectMapper().createObjectNode();
	}
}